

hamta_gymn_elever_kon_bakgrund_arskurs_prg_skolverket <- function(region_vekt = "20",                     # NA = riket, alla län och alla kommuner
                                                                  valda_ar = "9999",                          # "9999" senaste år, "*" = alla år 
                                                                  gymnasieprogram = "*",                     # "*" = alla gymnasieprogram, annars anges programnamn, dessa finns: "Nationella program", "Högskoleförberedande program", "Yrkesprogram", "Introduktionsprogrammen", "Barn- och fritidsprogrammet", "Bygg- och anläggningsprogramme", "Ekonomiprogrammet", "El- och energiprogrammet", "Estetiska programmet", "Fordons- och transportprogramm", "Försäljnings- och serviceprogr", "Handels- och administrationspr", "Hantverksprogrammet", "Hotell- och turismprogrammet", "Humanistiska programmet", "Industritekniska programmet", "International Baccalaureate", "Introduktionsprogram, Individu", "Introduktionsprogram, Programi", "Introduktionsprogram, Språkint", "Introduktionsprogram, Yrkesint", "Naturbruksprogrammet", "Naturvetenskapsprogrammet", "Restaurang- och livsmedelsprog", "Riksrekryterande utbildningar", "Samhällsvetenskapsprogrammet", "Teknikprogrammet", "VVS- och fastighetsprogrammet", "Vård- och omsorgsprogrammet"
                                                                  huvudman = "Samtliga",                     # finns: "Samtliga", "Kommunal" och "Enskild", det går att välja flera
                                                                  konvertera_andel_till_numerisk = TRUE      # TRUE = numerisk kolumn av andel, då försvinner prickar och liknande och blir NA. Vill man se vad som är prickar och hur många det är kan man sätta denna till FALSE
                                                                  ) {         
  
  # ==================================================================================================================
  #
  # Skript för att hämta gymnasieelever per kön, bakgrund och årskurs från Skolverket per län, kommun eller för riket. 
  # Detta skript hämtar alla kommuner i en excelfil för alla år. Det går således inte att snabba upp skriptet genom att 
  # välja någon eller några få kommuner och man hämtar alltid alla år. Men man kan ändå filtrera ut de kommuner, län
  # eller riket som man vill ha för att få ett mindre dataset. Skriptet kollar att det har senaste år och använder detta.
  # Det variabler som ingår är läsår, regionkod, region, Gymnasieprogram, Typ av huvudman, Genomströmning samt andel. 
  #
  # Kön, bakgrund och föräldras utbildning är i procent, antal elever i absoluta tal.
  #
  # Skapat av: Peter Möller, Region Dalarna
  #
  # ==================================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         readxl,
         httr)
  
  # extrahera senaste år från en tabell på Skolverkets webbplats där länken inte är bunden till vilket år det är
  GET("https://siris.skolverket.se/siris/sitevision_doc.getFile?p_id=552681", write_disk(tf_artalfil <- tempfile(fileext = ".xlsx")))
  #excel_sheets(tf_artalfil)
  artal_txt <- suppressMessages(read_excel(tf_artalfil, sheet = "5A")) %>% 
    dplyr::pull(1) %>%
    .[!is.na(.)] %>% 
    .[str_detect(., "läsåret")] %>% 
    str_extract("\\d{4}")              # för hela: "\\d{4}/\\d{2}"
  
  giltiga_ar <- 2011:as.numeric(artal_txt) %>% as.character()
  
  valda_ar <- valda_ar %>% str_replace("9999", artal_txt) %>% str_replace("\\*", giltiga_ar) %>% unique() %>% .[. %in% giltiga_ar]
  
  if (length(valda_ar) > 0) {
      # url:er till samtliga geografiska nivåer (riket, län och kommuner)
      url_lista <- c(
                     url_riket = "https://siris.skolverket.se/siris/reports/export_api/runexport/?pFormat=xls&pExportID=445&pAr=2023&pLan=&pKommun=&pHmantyp=&pUttag=null&pToken=26E3C4BF90EC124BE06311BA650A5972&pFlikar=1&pVerkform=21",
                     url_lan = "https://siris.skolverket.se/siris/reports/export_api/runexport/?pFormat=xls&pExportID=340&pAr=2023&pLan=&pKommun=&pHmantyp=&pUttag=null&pToken=26E3C4BF90EC124BE06311BA650A5972&pFlikar=1&pVerkform=21",
                     url_kommun = "https://siris.skolverket.se/siris/reports/export_api/runexport/?pFormat=xls&pExportID=58&pAr=2023&pLan=&pKommun=&pHmantyp=&pUttag=null&pToken=26E3C4BF90EC124BE06311BA650A5972&pFlikar=1&pVerkform=21"
                     )
      
      # om vi fått ett annat år när vi extraherat senaste år ovan än vad som finns i url:erna ovan (år 2019) så används detta istället
    
      url_lista <- map_chr(url_lista, ~ str_replace(.x, "&pAr=\\d{4}", paste0("&pAr=", artal_txt)))
      
      df_list <- list()                   # vi sparar hämtad statistik till denna lista
      
      # om region_Vekt är NA så hämtas alla län, kommuner och riket
      if (all(region_vekt == "*")) region_vekt <- hamtaregtab()$regionkod
      
      las_in_excelfil <- function(fil_url) {
        
        GET(fil_url, write_disk(tf_excelfil <- tempfile(fileext = ".xlsx")))
        flikar <- excel_sheets(tf_excelfil) %>% .[!str_detect(., "beskrivning")]
        if (!all(gymnasieprogram == "*")) flikar <- flikar[flikar %in% gymnasieprogram]
        
        dataset_df <- map(flikar, ~ read_excel(tf_excelfil, sheet = .x, skip = 8, col_types = "text") %>%
                            pivot_longer(any_of(c(starts_with("Andel"), starts_with("Antal"))), values_to = "varde", names_to = "variabel") %>%
                            mutate(gymnasieprogram = .x) %>% 
                            filter(`Typ av huvudman` %in% huvudman,
                                   varde != ".")) %>%
          list_rbind() %>% 
          relocate(gymnasieprogram, .before = 1)

        lasar_txt <- suppressMessages(read_excel(tf_excelfil, sheet = 1)) %>% 
          dplyr::pull(1) %>%
          .[!is.na(.)] %>% 
          .[str_detect(., "läsår")] %>% 
          str_extract("\\d{4}/\\d{2}")
        
        # om det är riket som hämtas
        if ("Riket" %in% names(dataset_df)) {
          dataset_df <- dataset_df %>%
            mutate(regionkod = "00") %>%
            rename(region = Riket) %>% 
            relocate(region, .before = 1) %>% 
            relocate(regionkod, .before = 1) 
        }
        
        # om det är län som hämtas
        if ("Läns-kod" %in% names(dataset_df) & !"Kommun-kod" %in% names(dataset_df)) {
          dataset_df <- dataset_df %>%
            rename(regionkod = `Läns-kod`,
                   region = Län) %>% 
            relocate(region, .before = 1) %>% 
            relocate(regionkod, .before = 1) 
        }
        
        # om det är kommuner som hämtas
        if ("Län" %in% names(dataset_df) & "Kommun-kod" %in% names(dataset_df)) {
          dataset_df <- dataset_df %>%
            rename(regionkod = `Kommun-kod`,
                   region = Kommun) %>% 
            select(-c(`Läns-kod`, Län)) %>%
            relocate(region, .before = 1) %>% 
            relocate(regionkod, .before = 1) 
        }
        
        dataset_df <- dataset_df %>% 
          mutate(lasar = lasar_txt) %>%
          rename(huvudman = `Typ av huvudman`) %>%
          relocate(lasar, .before = 1) %>%
          filter(regionkod %in% region_vekt)
        
        return(dataset_df)
      } # slut läs in excelfil-funktion
      
      # här görs själva jobbet med att extrahera information för de regioner och år vi har valt
      alla_ar <- map(valda_ar, function(ar) {
        
        url_lista <- map_chr(url_lista, ~ str_replace(.x, "&pAr=\\d{4}", paste0("&pAr=", ar)))
        
        if (length(region_vekt[region_vekt == "00"]) > 0) df_list[["riket"]] <- las_in_excelfil(url_lista[["url_riket"]])            
        if (length(region_vekt[nchar(region_vekt) == 2 & region_vekt != "00"]) > 0) df_list[["lan"]] <- las_in_excelfil(url_lista[["url_lan"]])
        if (length(region_vekt[nchar(region_vekt) == 4]) > 0) df_list[["kommun"]] <- las_in_excelfil(url_lista[["url_kommun"]])
        
        retur_df <- bind_rows(df_list)
        if (konvertera_andel_till_numerisk) retur_df <- suppressWarnings(retur_df %>% mutate(varde = parse_number(varde)))
        return(retur_df)
        
      }, .progress = TRUE) %>% 
        list_rbind()
      
      return(alla_ar)
  
  } else { # slut if-sats för att testa om det finns giltiga år
    message(glue("Inga giltiga år medskickade till funktionen. Följande år finns i tabellen: {giltiga_ar %>% list_komma_och()}. Kontrollera valda år och försök igen.")) 
  } # slut test om det finns giltiga år


}
  
