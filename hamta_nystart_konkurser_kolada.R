#test = hamta_data_nystartade_konkurser(alla_regioner = FALSE,spara_data = FALSE)

hamta_data_nystartade_konkurser = function(region = c("0020"), # Val av region. Börjr med 00 för regioner (och Sverige) i Kolada
                                           alla_regioner = FALSE, # TRUE om man vill ha alla regioner. Övertrumfar region
                                           alla_kommuner = TRUE, # TRUE om man vill ha alla kommuner för valda kommuner.
                                           ta_med_riket = TRUE, # TRUE om man vill ha med riket.
                                           outputmapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                           filnamn = c("nystartade.xlsx","konkurser.xlsx"), # Filnamn. Bör inte ändras.
                                           cont_cod = c("N00999","N00926"), # "N00999" om man vill ha nystartade, "N00926" om man vill ha konkurser. Ordning bör vara samma som filnamn
                                           tid = 1900:2100, # "Om man enbart vill ha senaste år"99" om man enbart vill ha senaste år. Välj ett högt värde som sista värde om alla år skall vara med.
                                           spara_data = FALSE, # Om man vill spara data
                                           returnera_data = TRUE){ # Om man vill returnera data som en lista av dataframes.
  
  # ===========================================================================================================
  # 
  # Skript som hämtar data för nystartade företag och/eller konkurser från Kolada. Tabellen finns inte uppdelad på kön
  # Returnerar en lista med dataframes och sparar två separata Excel-dokument (om man väljer att ta hem båda)
  # Skapad av Jon Frank
  # Senast ändrad: 2023-12-08
  # ===========================================================================================================
  # Paket som används
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 rKolada,
                 readxl)
  
  # Skapar en lista som används för att returnera dataframes.
  lista_data = lst()
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
 
  if(alla_regioner == TRUE){
    region = hamtaAllaLan(tamedriket = FALSE) 
    region = paste0("00",region)
  }
  
  if(alla_kommuner == TRUE){
    region_kommun = hamtakommuner(substr(region,3,4),tamedlan = FALSE,tamedriket = FALSE)
    region = c(region,region_kommun)
  }
  
  if(ta_med_riket == TRUE){
    region = c("0000",region)
  } 
  
  # Nystartade företag
  if("N00999" %in% cont_cod ){
    
    if("99" %in% tid) tid <- max(unique(hamta_kolada_giltiga_ar("N00999",vald_region = region)))
    
    cont = "N00999"
    
    Nystartade_df <- get_values(
      kpi = cont,
      municipality = region,
      period = tid
    )
    
    Nystartade_df <- Nystartade_df %>% 
      select(year,value,municipality) %>%
      rename("nystartade_ftg" = value)
    
    fil = paste0(outputmapp,filnamn[1])
    if (spara_data == TRUE) write.xlsx(Nystartade_df, fil)
    
    lista_data = c(lista_data,lst(Nystartade_df))
    
  }
  
  # Konkurser
  if("N00926" %in% cont_cod ){
    
    if("99" %in% tid) tid <- max(unique(hamta_kolada_giltiga_ar("N00926",vald_region = region)))
    
    cont = "N00926"
    
    Konkurser_df <- get_values(
      kpi = cont,
      municipality = region,
      period = tid
    )
    
    # Konkurser
    Konkurser_df <- Konkurser_df %>% 
      select(year,value,municipality) %>%
      rename("konkurser" = value) 
    
    fil = paste0(outputmapp,filnamn[2])
    
    if (spara_data == TRUE) write.xlsx(Konkurser_df, fil)
    
    lista_data = c(lista_data,lst(Konkurser_df))
  }
  
  if(returnera_data == TRUE) return(lista_data)
  
}
