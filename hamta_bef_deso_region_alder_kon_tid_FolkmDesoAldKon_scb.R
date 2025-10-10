# ==============================================================================================================================================
#
# Skript för att hämta befolkning på RegSO eller DeSO. Finns ålder och kön. 
#
# Man kan skicka med DeSO eller RegSO-koder för att få precis de områden man vill ha. Om man skickar med en läns- eller kommunkod får man alla
# RegSO eller DeSO i dessa län eller kommuner. 
#
# ==============================================================================================================================================

library(tidyverse)
library(pxweb)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
options(dplyr.summarise.inform = FALSE)

hamta_bef_deso_regso <- function(
    region_vekt = "20",                     # går att skicka läns och kommunkod för att ta alla deso/regso i ett län/en kommun
    alder_klartext = "totalt",                  # Finns: "totalt", "0-4 år", "5-9 år", "10-14 år", "15-19 år", "20-24 år", "25-29 år", "30-34 år", "35-39 år", "40-44 år", "45-49 år", "50-54 år", "55-59 år", "60-64 år", "65-69 år", "70-74 år", "75-79 år", "80- år"
    region_indelning = "RegSO",             # Finns: "RegSO" eller "DeSO"
    kon_klartext = c("kvinnor", "män"),     # Finns: "män", "kvinnor", "totalt"
    lagg_ihop_versioner = TRUE,             # om det finns flera versioner av RegSO/DeSO, lägg ihop dem till en
    tid_vekt = "*") {
  
  url_bef <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101Y/FolkmDesoAldKon"
  cont_vekt = "000007Y7"
  
  # vi hämtar alla regionkoder i databasen
  alla_regionkoder <- hamta_giltiga_varden_fran_tabell(url_bef, "region") 
  # vi tar ut endast RegSO eller DeSO beroende på vad användaren valt
  alla_regionkoder <- if(region_indelning == "RegSO") alla_regionkoder[str_detect(alla_regionkoder, "R")] else alla_regionkoder[str_detect(alla_regionkoder, "A|B|C")]
  
  if (region_vekt != "*") {
    # här delar vi upp de medskickade regionkoderna i RegSO/DeSO, länskoder och kommunkoder 
    fardiga_regionkoder <- region_vekt[str_length(region_vekt) == 9]           # RegSO eller DeSO
    lanskoder <- region_vekt[str_length(region_vekt) == 2]                     # länskoder
    kommunkoder <- region_vekt[str_length(region_vekt) == 4]                   # kommunkoder
    
    # här hämtar vi alla RegSO/DeSO för de länskoder eller kommunkoder som skickats med
    region_lan <- alla_regionkoder[str_sub(alla_regionkoder, 1,2) %in% lanskoder & str_length(alla_regionkoder) > 7]
    region_kommun <- alla_regionkoder[str_sub(alla_regionkoder, 1,4) %in% kommunkoder & str_length(alla_regionkoder) > 7]
  
    # vi lägger ihop vektorerna för färdiga RegSO/DeSO, samt RegSO/DeSO för de län och kommuner som skickats med samt tar bort dubletter
    alla_region <- c(fardiga_regionkoder, region_lan, region_kommun) %>% .[!duplicated(.)]
    
  } else alla_region <- alla_regionkoder                     # om användaren vill ha samtliga koder för RegSO eller DeSO
  
  kon_vekt <- if(is.na(kon_klartext)) "*" else hamta_kod_med_klartext(url_bef, kon_klartext, skickad_fran_variabel = "kon")
  alder_vekt <- hamta_kod_med_klartext(url_bef, alder_klartext, skickad_fran_variabel = "alder")
  
  query_list <- list(Region = alla_region,
                     Alder = alder_vekt,
                     Kon = kon_vekt,
                     ContentsCode = cont_vekt,
                     Tid = tid_vekt)
  
  if (is.na(kon_klartext)) query_list <- query_list[names(query_list) != "Kon"]
  
  px_uttag <- pxweb_get(url = url_bef,
                        query = query_list)
  
  px_df <- suppress_specific_warning(
    as.data.frame(px_uttag) %>% 
    cbind(regionkod = as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>% 
            select(Region)) %>% rename(regionkod = Region) %>% relocate(regionkod, .before = region)
  )
  
  # extrahera områdesnamn ur region om det är RegSO + kommunkod och kommunnamn
  if (region_indelning == "RegSO") {
    px_df <- px_df %>% 
      mutate(omrade = str_extract(string = region,
                                  pattern = "(?<=\\().*(?=\\))"),
             kommunkod = regionkod %>% str_sub(1,4),
             kommun = str_split(region, "\\(")[[1]][1]) %>% 
      relocate(omrade, .after = region) %>% 
      relocate(kommunkod, .after = omrade) %>% 
      relocate(kommun, .after = kommunkod)
  } else { # om DeSO, extahera kommunkod och kommunnamn
  
    kommunkoder <- str_sub(px_df$regionkod, 1, 4) %>% .[!duplicated(.)]             # ta ut alla unika kommunkoder i datasetet
    kommunnyckel <- hamtaregion_kod_namn(kommunkoder)                               # skapa df med kommunkod och kommunnamn som vi kan köra en left_join med
    
    # skapa kommunkod från DeSO-kod och koppla på kommun från kommunnyckeln
    px_df <- px_df %>% 
      mutate(kommunkod = regionkod %>% str_sub(1,4)) %>%
      left_join(kommunnyckel %>% rename(kommun = region), by = c("kommunkod" = "regionkod")) %>% 
    relocate(kommunkod, .after = region) %>% 
    relocate(kommun, .after = kommunkod)
    
  } # slut test om det är RegSO eller DeSO
  
  # lägg ihop tidsserier av olika deso- eller regsoversioner om lagg_ihop_versioner == TRUE
  if (lagg_ihop_versioner) {
    kolumner <- names(px_df) %>% .[. != "Antal"]
    
    px_df <- px_df %>% 
      mutate(regionkod = regionkod %>% str_remove("_.*"),
             kommun) %>% 
      group_by(across(any_of(kolumner))) %>% 
      summarise(Antal = sum(Antal, na.rm = TRUE), .groups = "drop")
  }
  
  px_df <- px_df %>% 
    mutate(kommunkod = kommunkod %>% str_trim(),
           kommun = kommun %>% str_trim())
  
  return(px_df)
  
} # slut funktion
