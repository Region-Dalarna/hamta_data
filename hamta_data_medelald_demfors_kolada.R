source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

hamta_data_medel_demo = function(region = hamtakommuner("20",tamedlan = TRUE,tamedriket = TRUE), # Val av region.
                                 konsuppdelat = TRUE, # TRUE om man vill ha könsuppdelad data, FALSE annars
                                 output_mapp = NA,
                                 filnamn = "medelalder_forskvot.xlsx", #c("medelalder.xlsx","dem_fors_kvot.xlsx"), # Filnamn. Bör inte ändras.
                                 cont_cod = c("N00959","N00927"), # Medelålder respektive demografisk försörjningskvot. Byt helst inte ordning (då blir det fel med excelfiler) 
                                 tid = 1900:2100, # "Om man enbart vill ha senaste år"9999" om man enbart vill ha senaste år. Välj ett högt värde som sista värde om alla år skall vara med.
                                 spara_data = FALSE, # Om man vill spara data
                                 returnera_data = TRUE){ # Om man vill returnera data som en lista av dataframes.
  
  # ===========================================================================================================
  # 
  # Skript som hämtar data för nystartade företag och/eller konkurser från Kolada. Tabellen finns inte uppdelad på kön
  # Returnerar en lista med dataframes och ett Exceldokument där data hamnar i olika likar
  # Skapad av Jon Frank
  # Senast ändrad: 2023-12-08
  # ===========================================================================================================
  # Paket som används
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 rKolada)
  
  # I Kolada har alla regioner fyra siffror varför region och riket behöver justeras
  region = ifelse(nchar(region) == 2,paste0("00",region),region)
  
  # Skapar en lista som används för att returnera dataframes.
  lista_data = lst()
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

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
    
    #fil = paste0(outputmapp,filnamn[1])
    
    #if (spara_data == TRUE) write.xlsx(medelalder_df, fil)
    
    lista_data = c(lista_data,lst("Medelalder" = medelalder_df))
    
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
    
   # fil = paste0(outputmapp,filnamn[2])
    
    #if (spara_data == TRUE) write.xlsx(Konkurser_df, fil)
    
    lista_data = c(lista_data,lst("Demografisk_forsorjningskvot",demo_df))
  }
  
  if (!is.na(output_mapp) & !is.na(filnamn)){
    write.xlsx(antal_sektor_df,paste0(output_mapp,filnamn))
  }
  
  if(returnera_data == TRUE) return(lista_data)
  
}
