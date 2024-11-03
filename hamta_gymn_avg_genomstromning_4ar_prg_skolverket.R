
#source("G:/skript/hamta_data/func_skolverket_webbtabeller.R", encoding = "utf-8", echo = FALSE)
#source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)


hamta_gymn_avg_genomstromning_4ar_prg_skolverket <- function(region_vekt = "20",                     # NA = riket, alla län och alla kommuner
                                                             huvudman = "Samtliga",                     # finns: "Samtliga", "Kommunal" och "Enskild", det går att välja flera
                                                             konvertera_andel_till_numerisk = TRUE      # TRUE = numerisk kolumn av andel, då försvinner prickar och liknande och blir NA. Vill man se vad som är prickar och hur många det är kan man sätta denna till FALSE
) {         
  
  # ==================================================================================================================
  #
  # Skript för att hämta genomströmning för gymnasieavgångna från Skolverket per län, kommun eller för riket. 
  # Detta skript hämtar alla kommuner i en excelfil för alla år. Det går således inte att snabba upp skriptet genom att 
  # välja någon eller några få kommuner och man hämtar alltid alla år. Men man kan ändå filtrera ut de kommuner, län
  # eller riket som man vill ha för att få ett mindre dataset. Skriptet kollar att det har senaste år och använder detta.
  # Det variabler som ingår är läsår, regionkod, region, Gymnasieprogram, Typ av huvudman, Genomströmning samt andel. 
  #
  # Absolut tal finns inte i datasetet.
  #
  # Skapat av: Peter Möller, Region Dalarna
  #
  # ==================================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         readxl,
         httr)
  
  # extrahera senaste år från en tabell på Skolverkets webbplats där länken inte är bunden till vilket år det är
  GET("https://siris.skolverket.se/siris/sitevision_doc.getFile?p_id=552731", write_disk(tf_artalfil <- tempfile(fileext = ".xlsx")))
  excel_sheets(tf_artalfil)
  artal_txt <- suppressMessages(read_excel(tf_artalfil, sheet = "Tabell 3 B Sid 2")) %>% 
    dplyr::pull(1) %>%
    .[!is.na(.)] %>% 
    .[str_detect(., "Genomströmning")] %>% 
    str_extract("\\d{4}")
  
  # url:er till samtliga geografiska nivåer (riket, län och kommuner)
  url_lista <- c(url_riket = "https://siris.skolverket.se/siris/reports/export_api/runexport/?pFormat=xls&pExportID=402&pAr=2019&pLan=&pKommun=&pHmantyp=&pUttag=null&pToken=25CB9D24F0FA4D5DE06311BA650A8D29&pFlikar=1&pVerkform=21",
                 url_lan = "https://siris.skolverket.se/siris/reports/export_api/runexport/?pFormat=xls&pExportID=403&pAr=2019&pLan=&pKommun=&pHmantyp=&pUttag=null&pToken=25CB9D24F0FA4D5DE06311BA650A8D29&pFlikar=1&pVerkform=21",
                 url_kommun = "https://siris.skolverket.se/siris/reports/export_api/runexport/?pFormat=xls&pExportID=404&pAr=2019&pLan=&pKommun=&pHmantyp=&pUttag=null&pToken=05D2B2A8022E3496E06320BA650A6F89&pFlikar=1&pVerkform=21")
  
  # om vi fått ett annat år när vi extraherat senaste år ovan än vad som finns i url:erna ovan (år 2019) så används detta istället
  if (artal_txt != "2019") {
    url_lista <- map_chr(url_lista, ~ str_replace(.x, "&pAr=\\d{4}", paste0("&pAr=", artal_txt)))
  }
  
  df_list <- list()                   # vi sparar hämtad statistik till denna lista
  
  # om region_Vekt är NA så hämtas alla län, kommuner och riket
  if (all(is.na(region_vekt))) region_vekt <- hamtaregtab()$regionkod
  
  las_in_excelfil <- function(fil_url) {
    
    GET(fil_url, write_disk(tf_excelfil <- tempfile(fileext = ".xlsx")))
    flikar <- excel_sheets(tf_excelfil) %>% .[!str_detect(., "beskrivning")]
    
    genomstr_df <- map(flikar, ~ read_excel(tf_excelfil, sheet = .x, skip = 6, col_types = "text") %>%
                         pivot_longer(starts_with("20"), values_to = "andel", names_to = "läsår") %>%
                         mutate(Gymnasieprogram = .x) %>% 
                         filter(`Typ av huvudman` %in% huvudman)) %>% 
      list_rbind() %>% 
      relocate(läsår, .before = 1) %>%
      relocate(Gymnasieprogram, .after = läsår)
    
    # om det är riket som hämtas
    if ("Riket" %in% names(genomstr_df)) {
      genomstr_df <- genomstr_df %>%
        mutate(regionkod = "00") %>%
        rename(region = Riket) %>% 
        relocate(region, .before = 1) %>% 
        relocate(regionkod, .before = 1) %>% 
        relocate(läsår, .before = 1)
    }
    
    # om det är län som hämtas
    if ("Län" %in% names(genomstr_df) & !"Kommunkod" %in% names(genomstr_df)) {
      genomstr_df <- genomstr_df %>%
        rename(regionkod = Länskod,
               region = Län) %>% 
        relocate(region, .before = 1) %>% 
        relocate(regionkod, .before = 1) %>% 
        relocate(läsår, .before = 1)
    }
    
    # om det är kommuner som hämtas
    if ("Län" %in% names(genomstr_df) & "Kommunkod" %in% names(genomstr_df)) {
      genomstr_df <- genomstr_df %>%
        rename(regionkod = Kommunkod,
               region = Kommun) %>% 
        select(-c(Länskod, Län)) %>%
        relocate(region, .before = 1) %>% 
        relocate(regionkod, .before = 1) %>% 
        relocate(läsår, .before = 1)
    }
    
    genomstr_df <- genomstr_df %>% 
      filter(regionkod %in% region_vekt)
    
    return(genomstr_df)
  } # slut läs in excelfil-funktion
  
  if (length(region_vekt[region_vekt == "00"]) > 0) df_list[["riket"]] <- las_in_excelfil(url_lista[["url_riket"]])            
  if (length(region_vekt[nchar(region_vekt) == 2 & region_vekt != "00"]) > 0) df_list[["lan"]] <- las_in_excelfil(url_lista[["url_lan"]])
  if (length(region_vekt[nchar(region_vekt) == 4]) > 0) df_list[["kommun"]] <- las_in_excelfil(url_lista[["url_kommun"]])
  
  
  retur_df <- bind_rows(df_list)
  if (konvertera_andel_till_numerisk) retur_df <- suppressWarnings(retur_df %>% mutate(andel = parse_number(andel)))
  
  
  return(retur_df)
}
