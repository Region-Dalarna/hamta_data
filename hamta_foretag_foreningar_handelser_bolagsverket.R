
hamta_foretag_foreningar_handelser <- function(region_vekt = "*",
                                           bolagsform_kod = c("AB", "E", "HB", "KB"),            # finns: "AB", "E", "HB", "KB", "EK", "BRF", "FL", "I", "BF", "SF", "KHF", "OFB", "BFL", "BAB", "FAB", "MB", "SB", "S", "TSF", "SCE", "TPAB", "SE", "FOF", "OTPB", "TPF"
                                                                                                 # som betyder: "Aktiebolag", "Enskilda näringsidkare", "Handelsbolag", "Kommanditbolag", "Ekonomiska föreningar", "Bostadsrättsföreningar", "Filialer till utländska företag", "Ideella föreningar som bedriver näring", "Bostadsföreningar", "Sambruksföreningar", "Kooperativa hyresrättsföreningar", "Ömsesidiga försäkringsbolag", "Utländska bankers filialer", "Bankaktiebolag", "Försäkringsaktiebolag", "Medlemsbanker", "Sparbanker", "NA", "Trossamfund", "Europabolag"
                                           handelse_klartext = "*",                  # finns: "Nyregistrerade", "Avslutade", "Alla registrerade"
                                           tid_koder = "*",
                                           kortnamn_lan = TRUE,                       # om TRUE så tas län bort från länsnamnet, annars skrivs det ut
                                           tabort_tomma_kolumner = TRUE               # tar bort kolumner som helt saknar värden (kan vara bra att behålla om man vill ha konsistenta dataset till antalet kolumner)
                                           ) {
  
  # =================================================================================================
  #
  # Funktionen hämtar data från Bolagsverket för Nyregistrerade, Avslutade eller Alla registrerade 
  # organisationer såsom företag, föreningar och andra organisationer. 
  #
  # Standardinställning är: Aktiebolag, Enskilda firmor, Handelsbolag och Kommanditbolag.
  # tid_koder = "*" innebär att alla år och månader tas med. Om bara ett år skickas med så tas alla månader 
  #                 det året med. "9999" = senaste året och månaden som finns i datasetet
  #
  # region_vekt = "*" innebär att alla regioner tas med. "00" för riket, länskoder för län (t.ex. "20" för 
  #               Dalarna) och kommunkoder för kommuner (t.ex. "2021" för Vansbro). Det går bra att hämta
  #               alla tre typer av regioner samtidigt, som t.ex. c("00", "20", "2021").
  #
  # =================================================================================================
  options(dplyr.summarise.inform = FALSE)
  library(data.table)
  library(tidyverse)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  
  handelser_variabler <- c("armanad", "ar", "manad", "regfam", "regfamtext", "handelsekod", "handelse", "org_kod", "org")
  
  alla_regioner <- hamtaregtab()
  
  alla_lan <- alla_regioner$regionkod[nchar(alla_regioner$regionkod) == 2  & alla_regioner$regionkod != "00"]
  alla_kommuner <- alla_regioner$regionkod[nchar(alla_regioner$regionkod) == 4]
  
  if (all(region_vekt == "*")) {
    region_vekt <- alla_regioner$regionkod
  } else {
    regionkoder_riket <- region_vekt[region_vekt == "00"]
    regionkoder_lan <- region_vekt[region_vekt %in% alla_lan]
    regionkoder_kommuner <- region_vekt[region_vekt %in% alla_kommuner]
  }
  
  ftg_sokvag <- "https://static.bolagsverket.se/statistik/ftgstat_oppna.csv"
  response <- httr::GET(ftg_sokvag)                               # hämta data från Bolagsverket
  temp_file <- tempfile(fileext = ".csv")                         # skapa en temporär CSV-fil
  writeBin(httr::content(response, "raw"), temp_file)             # skriv data till den temporära CSV-filen
  dataset_df <- fread(temp_file, encoding = "Latin-1")            # Läs in CSV-filen med fread()
  
  # Läs in nyckel för typ av organisation
  orgtyp_nyckel <- suppressMessages(read_csv2("https://github.com/Region-Dalarna/depot/raw/refs/heads/main/beskrivning_ftg_foreningar_bolagsverket.csv", locale = locale(encoding = "UTF-8"), show_col_types = FALSE)) %>% 
    mutate(org = org %>% str_remove_all("Antal "), 
           org = org %>% str_replace_all("Ekonomisk föreningar", "Ekonomiska föreningar"))
  
  # Börja bearbeta datasetet
  retur_df <- dataset_df %>% 
    mutate(handelsekod = handelse,
           handelse = case_when(handelsekod == 1 ~ "Nyregistrerade",
                                handelsekod == 2 ~ "Alla registrerade",
                                handelsekod == 3 ~ "Avslutade",
                                TRUE ~ "Okänd"),
           manadskod = manad,
           manad = format(as.Date(paste0("01-", manad, "-01"), format = "%d-%m-%Y"), "%B"),
           SATEKOMMUN = ifelse(SATEKOMMUN == "0", "00", SATEKOMMUN), 
           regionkod = paste0(str_pad(SATELAN, 2, "left", "0"), str_pad(SATEKOMMUN, 2, "left", "0")) %>% as.character()) %>% 
    pivot_longer(cols = c(AB:TPF), names_to = "org_kod", values_to = "antal", values_drop_na = TRUE) %>%
    left_join(orgtyp_nyckel, by = "org_kod") 
  
  
  # hantera tid_koder och "9999"
  giltiga_armanad <- unique(retur_df$armanad) %>% as.character() %>% sort()
  tid_koder <- tid_koder %>%           # ersätt "9999" med senaste år
    str_replace_all("9999", giltiga_armanad %>% max())
  
  # hantera tid_koder som startar med ett år där alla månader ska vara med samt även 
  # intervaller där alla giltiga värden mellan två år eller år-måander ska vara med
  tid_vekt <- if (all(tid_koder == "*")) {
    giltiga_armanad
  } else {
    
    tid_koder_ej_kolon <- tid_koder[!str_detect(tid_koder, ":")]
    tid_koder_med_kolon <- tid_koder[str_detect(tid_koder, ":")]
    
    # ta ut alla koder, om bara år har skickats med tas alla månader med etc. 
    tid_koder_ej_kolon <- tid_koder_ej_kolon %>%
      map(~ giltiga_armanad[str_starts(giltiga_armanad, .x)]) %>%
      unlist()
    
    tid_vekt_med_kolon <- map(tid_koder_med_kolon, function(period) {
      
      period_justerad <- str_split(period, ":") %>% unlist() %>%
        map_chr(~ if_else(str_length(.x) == 4, str_c(.x, "01"), .x))
      
      intervall <- map_int(period_justerad, ~ which(giltiga_armanad == .x))
      retur_txt <- giltiga_armanad[intervall[1]:intervall[2]] %>% unlist()
      return(retur_txt)
    }) %>% unlist()
    
    retur_vekt <- c(tid_koder_ej_kolon, tid_vekt_med_kolon) %>% sort()
  } # slut if-sats om tid_koder == "*"
  
  # filtrera bort rader användaren inte behöver
  if (any(bolagsform_kod != "*")) retur_df <- retur_df %>% filter(org_kod %in% bolagsform_kod)
  if (any(handelse_klartext != "*")) retur_df <- retur_df %>% filter(handelse %in% handelse_klartext)
  if (any(tid_koder != "*")) retur_df <- retur_df %>% filter(armanad %in% tid_vekt)
  
  # ta ut riket
  retur_riket <- if (length(regionkoder_riket) > 0) {
    retur_df %>% 
      mutate(regionkod = "00",
             region = "Riket",
             regfam = NA, 
             regfamtext = NA) %>%
      group_by(regionkod, region, across(any_of(handelser_variabler))) %>% 
      summarise(antal = sum(antal, na.rm = TRUE)) %>% 
      ungroup()
  } else NULL
  
  # ta ut län
  retur_lan <- if (length(regionkoder_lan) > 0) {
    retur_df %>% 
      mutate(regionkod = str_pad(SATELAN, 2, "left", "0"),
             region = LANTEXT,
             regfam = NA, 
             regfamtext = NA) %>%
      filter(regionkod %in% regionkoder_lan) %>%
      group_by(regionkod, region, across(any_of(handelser_variabler))) %>% 
      summarise(antal = sum(antal, na.rm = TRUE)) %>% 
      ungroup()
  } else NULL
  
  retur_kommun <- if (length(regionkoder_kommuner) > 0) {
    retur_df %>% 
      mutate(region = KOMTEXT) %>%
      filter(regionkod %in% regionkoder_kommuner) %>%
      group_by(regionkod, region, across(any_of(handelser_variabler))) %>% 
      summarise(antal = sum(antal, na.rm = TRUE)) %>% 
      ungroup()
  } else NULL
  
  retur_alla <- bind_rows(retur_riket, retur_lan, retur_kommun) %>% 
    select(armanad, ar, manad, regionkod, region, regfamkod = regfam, regfam = regfamtext, 
           handelse, bolagsformkod = org_kod, bolagsform = org, antal)
  
  # skapar kortnamn för län om användaren valt det
  if (kortnamn_lan) retur_alla <- retur_alla %>% mutate(region = region %>% skapa_kortnamn_lan())
  # tar bort kolumner med enbart na-värden, om användaren valt att göra det
  if (tabort_tomma_kolumner) retur_alla <- retur_alla %>% select(where(~ !all(is.na(.))))
  
  return(retur_alla)  
}
