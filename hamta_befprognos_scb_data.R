
hamta_befprognos_data <- function(
    region_vekt = "20",
    alder_list = "*",                  
    kon_klartext = c("kvinnor", "män"),     # "män", "kvinnor"
    cont_klartext = "Folkmängd",       # "Folkmängd", "Födda", "Döda", "Inrikes inflyttning", "Inrikes utflyttning", "Invandring", "Utvandring"
    tid_vekt = "*",                    # kan vara enskilda år, om "+" eller "-" skickas med så tas prognosåret + eller - antalet år som skickas med, 
                                       # ex. "+0" så tas själva prognosåret med. Om man vill ha pronosåret och ytterligare 10 år efter det så lägger man med: paste0("+", c(0:10))
    url_prognos_vektor = "G:/Samhällsanalys/Statistik/Befolkningsprognoser/Profet/datafiler/",           # behövs i både pxweb och profetdelen 
    prognos_ar = "9999",               # NA = alla år, eller enskilda år, "9999" = senaste år
    long_format = FALSE
) {
  
  # ===========================================================================================================
  #
  # Skript för att hämta data från SCB för befolkningsprognoser alternativt från körningar som man gjort i 
  # Profet. För Profet-filer så anges en mapp där Profetfilerna sparas. Samtliga filer i mappen läses in
  # och vilka prognosår (om det finns flera) som används styrs med parametern prognos_ar. För SCB:s API:er så 
  # är däremot en url ett prognosår så vill man jämföra flera prognoser med varandra behöver man skicka med
  # en url per prognosår. 
  # 
  # Parametrar som skickas med (= variabler i SCB-tabellen) är:
  # - Innehåll                                                    # Folkmängd (standard) därutöver: "Födda", "Döda", "Inrikes inflyttning", "Inrikes utflyttning", "Invandring", "Utvandring"
  # - Region                                                      # tabellen innehåller kommuner och län, för Profet så är regioner begränsat till vad man har tillgång till där (OBS! För närvarande görs ingen kontroll av regioner i Profetfilerna, bör nog läggas till)
  # - Kön                                                         # finns enbart kvinnor och män (inte totalt)
  # - Ålder                                                       # * = alla åldrar (standard), finns i ettårsgrupper tom 100+ år (en utveckling att kunna skicka med NA och kunna returnera totalt och inte åldersuppdelat)
  # - url_prognos_vektor                                          # url:er för att hämta data, från SCB via API:er eller till en mapp där det finns Profetfiler
  # - prognos_ar                                                  # "9999" = senaste tillgängliga år, gäller endaste för Profetfiler, för SCB:s API:er så styrs det av den url man skickar med ovan
  # - tid_vekt (dvs. år)                                          # * = alla år (standard). Om man bara vill jämföra tex 10 år framåt så blir det billigare att bara hämta det året och inte alla däremellan
  # - long_format                                                 # TRUE = data returneras i longformat, FALSE = i wideformat (standard)
  #
  # Skapat av: Peter Möller, Region Dalarna
  #            november 2023
  # Senast uppdaterat:  december 2023
  #                     (mindre justeringar)
  # 
  # ===========================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         pxweb,
         readxl)
  options(scipen = 999)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  
  url_scbtabell <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401A/BefProgOsiktRegN"   # används för profet att hämta innehållsvariabler

  #if (region_vekt[!hamtakommuner(lan = profet_lan, T, F)] )
  
  hamta_url_rad_i_vektor <- function(hamta_url) {
    
  # ================================== beräkna rätt tid_vekt ==============================================
   
  if (any(tid_vekt != "*")) {               # om inte "*" är valt
    jmfr_vekt <- tid_vekt[str_detect(tid_vekt, "\\+|\\-")] %>% as.numeric()              # alla år som ska beräknas utifrån prognosår
    andra_ar_vekt <- tid_vekt[!str_detect(tid_vekt, "\\+|\\-")] %>% as.numeric()         # övriga år = år som skickas med som de är, tex. "2015"
    
    if(str_detect(hamta_url, "https://api.scb.se")){
      
       prognos_ar <- hamta_giltiga_varden_fran_tabell(hamta_url, "tid") %>% min() %>% as.numeric() %>% unique()
      
    } else {
      
      filsokvagar <- list.files(hamta_url, pattern = ".csv", full.names = TRUE)
      
      # behåll de som finns i region_vekt
      if (!"20" %in% region_vekt) filsokvagar <- filsokvagar[!str_detect(filsokvagar, "lan")]                        # ta bort länsfiler ur sokvagsvektorn om inte "lan" är med i sökvägen
      if (!any(hamtakommuner(lan = "20", F, F, F) %in% region_vekt)) filsokvagar <- filsokvagar[!str_detect(filsokvagar, "kommun")]      # ta bort kommunfiler ur sokvagsvektorn om ingen av Dalarnas kommuners kommunkoder är med
      
      # kontrollera vilka prognosår som finns bland profet-filerna i mappen
      # prognos_ar <- map_int(filsokvagar, ~ read_xlsx(.x, sheet = "Info") %>% 
      #                       pull() %>% 
      #                       .[!is.na(.)] %>% 
      #                       .[str_detect(., "prognosperiod")] %>% 
      #                       parse_number(.))
      prognos_ar <- map_int(filsokvagar, ~ parse_number(.))%>% unique()
    }
    
    start_ar <- prognos_ar  - 1                # ta bort -1 igen?
    jmfr_ar <- start_ar + jmfr_vekt
    if (length(jmfr_ar) > 0)  hamta_tid_vekt <- c(jmfr_ar, andra_ar_vekt) else hamta_tid_vekt <- andra_ar_vekt
    
  } else hamta_tid_vekt <- tid_vekt                   # om tid_Vekt == "*" så blir hamta_tid_vekt det också
  
  
  
  hamta_data_fran_tabell <- function(url_prognos) {
    
    if (str_detect(url_prognos, "https://api.scb.se")) {  
      
      # pxvarlist(url_prognos)
      # pxvardelist(url_prognos, "ALDER", skriv_vektlista_till_clipboard = TRUE)
      
      if (all(cont_klartext == "*")) cont_klartext <- hamta_giltiga_varden_fran_tabell(url_prognos, "contentscode", klartext = TRUE)
      cont_vekt <- hamta_kod_med_klartext(url_prognos, cont_klartext, skickad_fran_variabel = "contentscode")
      kon_vekt <- hamta_kod_med_klartext(url_prognos, kon_klartext, skickad_fran_variabel = "kon")
      alder_vekt <- alder_list %>% unlist()
      if (all(cont_klartext == "Födda")) alder_vekt <- "0"
      
      query_list <- list(Region = region_vekt,
                         Alder = alder_vekt,
                         Kon = kon_vekt,
                         ContentsCode = cont_vekt,
                         Tid = hamta_tid_vekt %>% as.character())
      
      px_uttag <- pxweb_get(url = url_prognos,
                            query = query_list)
                            
      px_df <- as.data.frame(px_uttag) %>% 
        cbind(regionkod = as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>% 
                select(Region)) %>% rename(regionkod = Region) %>% relocate(regionkod, .before = region) %>% 
        mutate(prognos_ar = prognos_ar %>% as.character())
      
      
      # man kan välja bort long-format, då låter vi kolumnerna vara wide om det finns fler innehållsvariabler, annars
      # pivoterar vi om till long-format, dock ej om det bara finns en innehållsvariabel
      if (long_format) {
        px_df <- px_df %>% 
          konvertera_till_long_for_contentscode_variabler(url_prognos)
        
      } # slut if-sats som kontrollera om vi vill ha df i long-format
      retur_df <- px_df
      
    } else {                    # om vi inte har en url som innehåller en adress till SCB:s API så utgår vi från att det är en profet-fil
      
      # hämta regionnyckel
      regionnyckel <- hamtaregtab()
      
      
      filsokvagar <- list.files(hamta_url, pattern = ".csv", full.names = TRUE)
      
      # behåll de som finns i region_vekt
      if (!"20" %in% region_vekt) filsokvagar <- filsokvagar[!str_detect(filsokvagar, "lan")]                        # ta bort länsfiler ur sokvagsvektorn om inte "20" är med som regionkod
      if (!any(hamtakommuner(lan = "20", F, F, F) %in% region_vekt)) filsokvagar <- filsokvagar[!str_detect(filsokvagar, "kommun")]      # ta bort kommunfiler ur sokvagsvektorn om ingen av Dalarnas kommuners kommunkoder är med

      # map_chr(filsokvagar, ~ read_xlsx(.x, sheet = "Info") %>% 
        #                     pull() %>% 
        #                     .[!is.na(.)] %>% 
        #                     .[str_detect(., "prognosperiod")] %>% 
        #                     parse_number(.) %>% 
        #                     as.character())
      
      alla_prognos_ar <- map_chr(filsokvagar, ~ parse_number(.) %>% as.character()) %>% unique()
      sok_prognos_ar <- prognos_ar %>% paste0(collapse = "|")               # för att kunna använda nedan i str_detect
      # ta ut rätt år utifrån användarens val
      #if (all(alla_prognos_ar == "9999")) filsokvagar <- filsokvagar[which(alla_prognos_ar == max(alla_prognos_ar))]
      if (all(prognos_ar == "9999")) filsokvagar <- filsokvagar[str_detect(filsokvagar, max(alla_prognos_ar))]
      #if (all(!is.na(alla_prognos_ar) & alla_prognos_ar != "9999")) filsokvagar <- filsokvagar[which(alla_prognos_ar %in% prognos_ar)]
      if (all(!is.na(prognos_ar) & prognos_ar != "9999")) filsokvagar <- filsokvagar[str_detect(filsokvagar, sok_prognos_ar)]
      if (any(str_detect(prognos_ar, "9999"))) prognos_ar <- alla_prognos_ar %>% str_replace("9999", max(alla_prognos_ar))
      
      # kontrollera vilka prognosår som finns bland profet-filerna i mappen som ska användas
      progn_ar <- map_chr(filsokvagar, ~ parse_number(.) %>% as.character()) 
      
      # vektor för att döpa om kolumner i profetfilen så att de blir samma som i pxwebs befolkningsprognostabeller
      rename_profet <- c("regionkod" = "lan_kod", "regionkod" = "kommun_kod", "ålder" = "age", "kön" = "kon", "år" = "year", 
                         "Folkmängd" = "pop", "Födda" = "fodda", "Döda" = "doda", "Inrikes inflyttning" = "inrikes_inflyttade", 
                         "Inrikes utflyttning" = "inrikes_utflyttade", "Invandring" = "immigranter", "Utvandring" = "emigranter")
      
      contvar_vekt <- c("Folkmängd", "Födda", "Döda", "Inrikes inflyttning", "Inrikes utflyttning", "Invandring", "Utvandring")
      if (all(cont_klartext == "*")) cont_klartext <- contvar_vekt
      
      las_in_profet_fil <- function(profetfil_sokvag, fil_prognosar) {
        
      if (any(hamta_tid_vekt != "*")) {
        fil_start_ar <- as.numeric(fil_prognosar)  - 1                # ta bort -1 igen?
        fil_jmfr_ar <- fil_start_ar + jmfr_vekt
        fil_hamta_tid_vekt <- if (length(fil_jmfr_ar) > 0) c(fil_jmfr_ar, andra_ar_vekt) else andra_ar_vekt
      } else fil_hamta_tid_vekt <- "*"
        
        # fil_prognosar <- read_xlsx(profetfil_sokvag, sheet = "Info") %>% 
        #   pull() %>% 
        #   .[!is.na(.)] %>% 
        #   .[str_detect(., "prognosperiod")] %>% 
        #   parse_number(.) %>% 
        #   as.character()
        
        #profet_df <- read_xlsx(profetfil_sokvag, sheet = "Data")                          # gammal, när vi läste in excelfiler, nu kör vi csv (som vi zippar för att spara utrymme)
        profet_df <- read_csv(profetfil_sokvag, show_col_types = FALSE) 
        kolnamn <- names(profet_df)                                                        # hämta kolumnnamn för att kunna avgöra om det är kommuner eller län
        
        # kolla om det är kommunfil, i så fall lägger vi ihop in- och utflyttning inom län och från utanför län
        if (all(c("utomin", "inomin", "utomut", "inomut") %in% kolnamn)) {
          profet_df <- profet_df %>% 
            mutate(inrikes_inflyttade = inomin + utomin,
                   inrikes_utflyttade = inomut + utomut) %>% 
            select(-c(inomin, utomin, inomut, utomut))
        }
        
        if (all(fil_hamta_tid_vekt == "*")) fil_hamta_tid_vekt <- c((fil_prognosar %>% as.numeric()):2100)
        
        profet_df <- profet_df %>% 
          rename(any_of(rename_profet))                                                    # döp om kolumner så de heter samma som i pxweb-befolkningsprognoserna
        
        regionkod_len <- if (any(str_detect(kolnamn, "lan_kod"))) 2 else 4                 # kolla om det är län eller kommuner, väljer längd på regionkoden utifrån vad vi har i datasetet
        
        profet_df <- profet_df %>% 
          mutate(regionkod = regionkod %>% str_sub(1,regionkod_len),
                 kön = ifelse(kön == 1, "män", "kvinnor"),
                 ålder = ifelse(ålder == 100, paste0(ålder, "+ år"), paste0(ålder, " år")),
                 år = år %>% as.character(),
                 prognos_ar = fil_prognosar) %>%
          filter(år %in% (fil_hamta_tid_vekt %>% as.character())) %>% 
          left_join(regionnyckel, by = "regionkod") %>% 
          relocate(region, .after = regionkod)
        
        tabort_contvar <- contvar_vekt[contvar_vekt != cont_klartext]                      # ta bort den innehållsvariabel som användaren vill ha ur vektorn, använd den för att ta bort variabler
        
        # ta bort variabler som användaren inte valt
        profet_df <- profet_df %>% 
          select(-all_of(tabort_contvar))
        
        if (all(cont_klartext == "Födda")) profet_df <- profet_df %>% filter(ålder == 0)
        
        # man kan välja bort long-format, då låter vi kolumnerna vara wide om det finns fler innehållsvariabler, annars
        # pivoterar vi om till long-format, dock ej om det bara finns en innehållsvariabel
        if (long_format) {
          
          profet_df <- profet_df %>% 
            konvertera_till_long_for_contentscode_variabler(url_scbtabell)
          
        } # slut if-sats som kontrollera om vi vill ha df i long-format
        return(profet_df)
      } # slut funktion för att läsa in profetfiler
      
      retur_filer <- map2_dfr(filsokvagar, progn_ar, ~ las_in_profet_fil(.x, .y))
      
      return(retur_filer)
      
    } # if-sats, else-delen som är om det är en sökväg till en profet-fil
  } # funktion att hämta data från url:er (scb-api:er eller profet-filer)
  
  retur_df <- map_dfr(hamta_url, ~ hamta_data_fran_tabell(url_prognos = .x)) %>% 
    filter(regionkod %in% region_vekt)
  
  return(retur_df)
  
  } # slut funktion för varje url-rad i url-vektorn
  
  befprognos_df <- map_dfr(url_prognos_vektor, ~ hamta_url_rad_i_vektor(hamta_url = .x))
  
  return(befprognos_df)
  
} # slut funktion  
