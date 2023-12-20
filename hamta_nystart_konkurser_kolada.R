
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

hamta_data_nystartade_konkurser = function(region = hamtakommuner("20",tamedlan = TRUE,tamedriket = TRUE), # Val av region.
                                           output_mapp = NA,
                                           filnamn = "nystartade_konkurser.xlsx", # Filnamn. 
                                           cont_cod = c("N00999","N00926"), # "N00999" om man vill ha nystartade, "N00926" om man vill ha konkurser.
                                           tid = 1900:2100, # "Om man enbart vill ha senaste år"99" om man enbart vill ha senaste år. Välj ett högt värde som sista värde om alla år skall vara med.
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
  
  # I Kolada har alla regioner fyra siffror varför region och riket behöver justeras
  region = ifelse(nchar(region) == 2,paste0("00",region),region)
  
  # Skapar en lista som används för att returnera dataframes.
  lista_data = lst()

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
    
    lista_data = c(lista_data,lst("Nystartade" = Nystartade_df))
    
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
    
    lista_data = c(lista_data,lst("Konkurser" = Konkurser_df))
  }
  
  if (!is.na(output_mapp) & !is.na(filnamn)){
    write.xlsx(lista_data,paste0(output_mapp,filnamn))
  }
  
  if(returnera_data == TRUE) return(lista_data)
  
}
