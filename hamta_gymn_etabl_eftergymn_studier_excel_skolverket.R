
hamta_gymn_avgangna_etablering_studier_skolverket <- function(region_vekt = "20",                        # NA = riket, alla län och alla kommuner
                                                        valda_ar = "9999",                          # "9999" senaste år, "*" = alla år 
                                                        huvudman = "Samtliga",                     # finns: "Samtliga", "Kommunal" och "Enskild", det går att välja flera
                                                        konvertera_andel_till_numerisk = TRUE      # TRUE = numerisk kolumn av andel, då försvinner prickar och liknande och blir NA. Vill man se vad som är prickar och hur många det är kan man sätta denna till FALSE
                                                        ){
  
  # ==================================================================================================================
  #
  # Skript för att hämta genomströmning för gymnasieavgångna från Skolverket per kommun. Detta skript hämtar alla 
  # kommuner i en excelfil, en per år. Det går således inte att snabba upp skriptet genom att välja någon eller några 
  # få kommuner. Däremot kan man begränsa hur många år man hämtar. 
  #
  # De variabler i detta dataset är: kommuner, gymnasieprogram, typ av huvudman, antal ungdomar,
  #                                  Andel etablerade 1, 3 och 5 år efter examen,
  #                                  Andel studerande 1, 3 och 5 år efter examen,
  #
  # Skapat av: Peter Möller, Region Dalarna
  #
  # ==================================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         readxl,
         glue,
         httr)
  
 source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R", encoding = "utf-8", echo = FALSE)
  # extrahera senaste år från en tabell på Skolverkets webbplats där länken inte är bunden till vilket år det är
  GET("https://siris.skolverket.se/siris/sitevision_doc.getFile?p_id=553342", write_disk(tf_artalfil <- tempfile(fileext = ".xlsx")))
  
  artal_txt <- suppressMessages(read_excel(tf_artalfil, sheet = 1)) %>% 
    dplyr::pull(1) %>%
    .[!is.na(.)] %>% 
    .[str_detect(., "Etableringsstatus")] %>% 
    str_extract("\\d{4}") %>% 
    as.numeric() %>% 
    `+`(1) %>% 
    as.character()
  
  giltiga_ar <- 2014:as.numeric(artal_txt) %>% as.character()
  
  valda_ar <- valda_ar %>% str_replace("9999", artal_txt) %>% str_replace("\\*", giltiga_ar) %>% unique() %>% .[. %in% giltiga_ar]
  
  if (length(valda_ar) > 0) {
    url_lista <- c(url_lan = "https://siris.skolverket.se/siris/reports/export_api/runexport/?pFormat=xls&pExportID=186&pAr=2021&pLan=&pKommun=&pHmantyp=&pUttag=null&pToken=26189526A7CF7181E06311BA650A455E&pFlikar=1&pVerkform=21",
                   url_kommun = "https://siris.skolverket.se/siris/reports/export_api/runexport/?pFormat=xls&pExportID=184&pAr=2021&pLan=&pKommun=&pHmantyp=&pUttag=null&pToken=25CB9D24F0FA4D5DE06311BA650A8D29&pFlikar=1&pVerkform=21")
    
    # om vi fått ett annat år när vi extraherat senaste år ovan än vad som finns i url:erna ovan (år 2019) så används detta istället
    if (artal_txt != "2021") {
      url_lista <- map_chr(url_lista, ~ str_replace(.x, "&pAr=\\d{4}", paste0("&pAr=", artal_txt)))
    }
    
    df_list <- list()                   # vi sparar hämtad statistik till denna lista
    
    # om region_Vekt är NA så hämtas alla län, kommuner och riket
    if (all(is.na(region_vekt))) region_vekt <- hamtaregtab()$regionkod %>% 
      .[. != "00"]
    
    andel_vekt <- c("Andel etablerade 1 år efter examen",
                    "Andel studerande 1 år efter examen",
                    "Andel etablerade 3 år efter examen",
                    "Andel studerande 3 år efter examen",
                    "Andel etablerade 5 år efter examen",
                    "Andel studerande 5 år efter examen")
    
    las_in_excelfil <- function(fil_url) {
      
      GET(fil_url, write_disk(tf_excelfil <- tempfile(fileext = ".xlsx")))
      flikar <- excel_sheets(tf_excelfil) %>% .[!str_detect(., "beskrivning")]
      start_rad <- suppressMessages(read_excel(tf_excelfil, sheet = 1, range = cell_cols("A"), col_types = "text")) %>%
        mutate(radnummer = row_number()) %>%       # Lägg till radnummer för varje rad
        filter(...1 %in% c("Län", "Kommun")) %>%        # Filtrera för att hitta "Kommun"
        dplyr::pull(radnummer)                            # Hämta radnumret 
       
        
      etablering_df <- suppressMessages(map(flikar, ~ read_excel(tf_excelfil, sheet = .x, skip = start_rad, col_types = "text") %>%
                             rename_with(
                               .fn = function(x) {
                                 # Tilldela nya namn bara till de kolumner som matchar
                                 x[startsWith(x, "Andel")] <- andel_vekt
                                 x  # returnera uppdaterade namn
                               },
                               .cols = starts_with("Andel")
                             ) %>%
                           pivot_longer(any_of(c(starts_with("Andel"), starts_with("Antal"))), values_to = "andel", names_to = "etablering") %>%
                           mutate(Gymnasieprogram = .x) %>% 
                           filter(`Typ av huvudman` %in% huvudman,
                                  andel != "."))) %>% 
        list_rbind() %>% 
        relocate(Gymnasieprogram, .before = 1)
  
      
      # om det är län som hämtas
      if ("Län" %in% names(etablering_df) & !"Kommun-kod" %in% names(etablering_df)) {
        etablering_df <- etablering_df %>%
          rename(regionkod = `Läns-kod`,
                 region = Län) %>% 
          relocate(region, .before = 1) %>% 
          relocate(regionkod, .before = 1)
      }
      
      # om det är kommuner som hämtas
      if ("Län" %in% names(etablering_df) & "Kommun-kod" %in% names(etablering_df)) {
        etablering_df <- etablering_df %>%
          rename(regionkod = `Kommun-kod`,
                 region = Kommun) %>% 
          select(-c(`Läns-kod`, Län)) %>%
          relocate(region, .before = 1) %>% 
          relocate(regionkod, .before = 1) 
      }
      
      lasar_txt <- read_excel(tf_excelfil, sheet = 1, range = "A5", col_types = "text")
      lasar_txt <- names(lasar_txt) %>% str_remove("Avgångsläsår: ")
      
      etablering_df <- etablering_df %>% 
        filter(regionkod %in% region_vekt) %>% 
        mutate(läsår = lasar_txt) %>% 
        relocate(läsår, .before = 1)
      
      return(etablering_df)
    } # slut läs in excelfil-funktion
    
    alla_ar <- map(valda_ar, function(ar) {
      
    url_lista <- map_chr(url_lista, ~ str_replace(.x, "&pAr=\\d{4}", paste0("&pAr=", ar)))
    
    if (length(region_vekt[nchar(region_vekt) == 2 & region_vekt != "00"]) > 0) df_list[["lan"]] <- las_in_excelfil(url_lista[["url_lan"]])
    if (length(region_vekt[nchar(region_vekt) == 4]) > 0) df_list[["kommun"]] <- las_in_excelfil(url_lista[["url_kommun"]])
    
    retur_df <- bind_rows(df_list)
    if (konvertera_andel_till_numerisk) retur_df <- suppressWarnings(retur_df %>% mutate(andel = parse_number(andel)))
    return(retur_df)
    
    }, .progress = TRUE) %>% 
      list_rbind()
    
    return(alla_ar)
    
  } else { # slut if-sats för att testa om det finns giltiga år
   message(glue("Inga giltiga år medskickade till funktionen. Följande år finns i tabellen: {giltiga_ar %>% list_komma_och()}. Kontrollera valda år och försök igen.")) 
  } # slut test om det finns giltiga år
  
} # slut funktion
   
  