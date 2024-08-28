hamta_data_matchning_lan_fa <-function(region = hamtaAllaLan(tamedriket = TRUE), # Val av region/FA-region
                                output_mapp = NA, # Här hamnar data som sparas. NA gör att data inte sparas
                                filnamn = "matchning.xlsx" , # Filnamn.
                                kon_klartext = c("män", "kvinnor","samtliga anställda"), 
                                alder_fodelseland = c("Sverige","Norden/EU","Afrika","Asien","Övriga_världen","totalt"), # För alternativa val, se nedan
                                tid = "*", # "Om man enbart vill ha senaste år skriv "9999". För intervall kan man skriva c(2015:"9999"). För alla år "*"
                                returnera_data = TRUE){ # Om data skall returneras som en dataframe
  
  # ===========================================================================================================
  # Skript som hämtar data för matchningsgrad i procent på i första hand länsnivå. Finns även för FA-region (från SCB)
  # Använd pxvardelist("https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM9906/AM9906A/RegionInd19M2N", "Region") för en förteckning över regioner
  # Val för variabeln alder_fodelseland:
  # "20-64", "20-39" , "Sverige", "Norden/EU", "Afrika", "Asien", "Övriga_världen", "totalt"
  # Skapad av Jon Frank
  # Senast ändrad: 2024-01-24
  # ===========================================================================================================
  
  # Paket som används
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 pxweb,
                 readxl)
  
  options(dplyr.summarise.inform = FALSE)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  # url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM9906/AM9906A/RegionInd19M2N"
  
  url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM9906/AM9906A/RegionInd19M2N1"
  
  # Använder klartext i vissa fall.
  if (all(kon_klartext == "*")){
    
    kon_vekt = "*"
    
  }else kon_vekt <- hamta_kod_med_klartext(url, kon_klartext, skickad_fran_variabel = "kon")
  
  giltiga_ar <- hamta_giltiga_varden_fran_tabell(url, "tid")
  if (all(tid != "*")) tid <- tid %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar] %>% unique()

  pxweb_query_list <- 
    list("Region" = region,
         "Kon" = kon_vekt,
         "AlderFodelselandgr" = alder_fodelseland,
         "ContentsCode" = "000005SF",
         "Tid" = tid)
  
  # Download data 
  matchning_df <- pxweb_get(url = url,
                            query = pxweb_query_list) %>%
    as.data.frame(column.name.type = "text", variable.value.type = "text") %>% 
        mutate(region = skapa_kortnamn_lan(region,byt_ut_riket_mot_sverige = TRUE))
  
  if(returnera_data == TRUE) return(matchning_df)
  
  if (!is.na(output_mapp) & !is.na(filnamn)){
    write.xlsx(matchning_df,paste0(output_mapp,filnamn))
  }
  
}
