#test = hamta_data_nystartade_konkurser(alla_regioner = FALSE,spara_data = FALSE)

hamta_data_nystartade_konkurser = function(region = c("0020"), # Val av region. Börjr med 00 för regioner (och Sverige) i Kolada
                                           alla_regioner = FALSE, # TRUE om man vill ha alla regioner. Övertrumfar region
                                           alla_kommuner = TRUE, # TRUE om man vill ha alla kommuner för valda kommuner.
                                           ta_med_riket = TRUE, # TRUE om man vill ha med riket.
                                           konsuppdelat = TRUE, # TRUE om man vill ha könsuppdelad data, FALSE annars
                                           outputmapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                           filnamn = c("medelalder.xlsx","dem_fors_kvot.xlsx"), # Filnamn. Bör inte ändras.
                                           cont_cod = c("N00959","N00927"), #Medelålder respektive demografisk försörjningskvot. Byt helst inte ordning (då blir det fel med excelfiler) 
                                           tid = 1900:2100, # "Om man enbart vill ha senaste år"9999" om man enbart vill ha senaste år. Välj ett högt värde som sista värde om alla år skall vara med.
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
  
  # Medelålder
  if("N00959" %in% cont_cod ){
    
    if("9999" %in% tid) tid <- max(unique(hamta_kolada_giltiga_ar("N00959",vald_region = region)))
    
    cont = "N00959"
    
    medelalder_df <- hamta_kolada_df(kpi_id = cont,
                                      valda_kommuner = region,
                                      valda_ar = tid,
                                      konsuppdelat = konsuppdelat)
    
    medelalder_df <- medelalder_df %>% 
      select(year, municipality, municipality_id, gender,fraga, value) %>%
        rename(variabel = fraga) 
    
    fil = paste0(outputmapp,filnamn[1])
    
    if (spara_data == TRUE) write.xlsx(Nystartade_df, fil)
    
    lista_data = c(lista_data,lst(medelalder_df))
    
  }
  
  # Demografisk försörjningskvot
  if("N00927" %in% cont_cod ){
    
    if("9999" %in% tid) tid <- max(unique(hamta_kolada_giltiga_ar("N00927",vald_region = region)))
    
    cont = "N00927"
    
    demo_df <- hamta_kolada_df(kpi_id = cont,
                               valda_kommuner = region,
                               valda_ar = tid,
                               konsuppdelat = konsuppdelat)
    
    # Konkurser
    demo_df <- demo_df %>% 
      select(year, municipality, municipality_id, gender,fraga, value) %>%
        rename(variabel = fraga) 
    
    fil = paste0(outputmapp,filnamn[2])
    
    if (spara_data == TRUE) write.xlsx(Konkurser_df, fil)
    
    lista_data = c(lista_data,lst(demo_df))
  }
  
  if(returnera_data == TRUE) return(lista_data)
  
}