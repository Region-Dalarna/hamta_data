#test_list <- hamta_data_branschbredd(spara_data = FALSE,ta_med_riket = FALSE)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
source("https://raw.githubusercontent.com/JonFrank81/funktioner/main/func_API_alternativ.R")

hamta_data_branschbredd <-function(region = hamtakommuner("20",tamedlan = TRUE,tamedriket = TRUE), # Val av region.
                                   output_mapp = NA, # Här hamnar data som sparas.
                                   filnamn = c("branschbredd.xlsx"), # Filnamn.
                                   tid = 1900:2100, # "Om man enbart vill ha senaste år skriv "9999". Välj ett högt värde som sista värde om alla år skall vara med
                                   returnera_data = TRUE){ # Om data skall returneras som en dataframe
  
  # ===========================================================================================================
  # 
  # Skript som hämtar data för branschbredd från Kolada. Tabellen finns inte uppdelad på kön
  # Skapad av Jon Frank
  # Senast ändrad: 2023-12-20
  # Ej helt uppdaterat. Funktionen byt_namn_lan_kolada skall flyttas från mitt privata skript till func_API (Jon)
  # ===========================================================================================================
  
  # Paket som används
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 rKolada,
                 readxl)
  
  options(dplyr.summarise.inform = FALSE)
  
  # I Kolada har alla regioner fyra siffror varför region och riket behöver justeras
  region = ifelse(nchar(region) == 2,paste0("00",region),region)
  
  if("9999" %in% tid) tid <- max(unique(hamta_kolada_giltiga_ar("N45702",vald_region = region)))
  
  #### Ta hem data från Kolada
  branschbredd <- get_values(
    kpi = c("N45702"),
    municipality = region,
    period = tid)
  
  # Väljer ut relevanta variabler och döper om value till Branschbredd
  
  branschbredd <- branschbredd %>% 
    select(year,value,municipality,municipality_id) %>% 
      rename("Branschbredd" = value) %>% 
        mutate(municipality = byt_namn_lan_kolada(municipality))
  
  if(returnera_data == TRUE) return(branschbredd)
  
  if (!is.na(output_mapp) & !is.na(filnamn)){
    write.xlsx(branschbredd,paste0(output_mapp,filnamn))
  }

}
