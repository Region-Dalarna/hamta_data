
# Funktioner som sourcas från Region Dalarna
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

#test_list=diag_utbniva(skapa_fil=FALSE)
hamta_data_utbniva <- function(region = hamtakommuner("20",tamedlan = TRUE,tamedriket = TRUE), # Använd förslagsvis hamtakommuner eller hamtaallalan
                               alder = c(as.character(25:64)), # antingen "tot16-74" eller annat intervall, exempelvis c(as.character(25:64)), "*" ger alla år
                               utbildningsniva_klartext = "*", # För alternativ, se text nedan
                               kon_klartext = c("män","kvinnor"), # c("män","kvinnor") eller var och en uppdelad. "*" funkar inte
                               output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/", # Outputmapp
                               filnamn = "utbildningsniva.xlsx", # Filnamn, om man vill spara data
                               spara_data = TRUE, # True om man vill spara data till Excel
                               returnera_data = TRUE, # Om man vill returnera data
                               tid ="9999" # Sätts till "9999" om man enbart vill ha senaste år,"*" för alla alternativt intervall
){
  
  # ===========================================================================================================
  #
  # Skript för att hämta data för utbildningsnivå 
  # Källa https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__UF__UF0506__UF0506B/UtbSUNBef/
  # Förklaring av vissa variabler
  # utbildningsniva_klartext:
  #   förgymnasial utbildning kortare än 9 år
  #   förgymnasial utbildning, 9 (10) år
  #   gymnasial utbildning, högst 2 år
  #   gymnasial utbildning, 3 år
  #   eftergymnasial utbildning, mindre än 3 år
  #   eftergymnasial utbildning, 3 år eller mer
  #   forskarutbildning
  #   uppgift om utbildningsnivå saknas
  # Generellt gäller "*" om man vill ha alla variabler
  # Skapad av Jon Frank
  # Uppdaterad senast 2023-12-14
  # ===========================================================================================================
  
  
  # ========================================== Läser in data ============================================
  # Skapa en lista med information som vi vill ha hem -- skapas lättast via pxweb_interactive()
  
  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
         tidyverse,
         openxlsx)
  
  # "Adresser" till SCBs databas
  url_uttag <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/UF/UF0506/UF0506B/Utbildning"
  
  # Gör om från klartext till något som PXweb förstår
  kon_vekt <- hamta_kod_med_klartext(url_uttag, kon_klartext, skickad_fran_variabel = "kon")
  
  if (utbildningsniva_klartext == "*"){
    
    utbildningsniva_vekt = "*"
    
  }else utbildningsniva_vekt <- hamta_kod_med_klartext(url_uttag, utbildningsniva_klartext, skickad_fran_variabel = "utbildningsniva")
  
  if("9999" %in% tid) tid = max(hamta_giltiga_varden_fran_tabell(url_uttag, "tid"))
  
  pxweb_query_list <- 
    list("Region" = region,
         "Kon" = kon_vekt,
         "Alder" = c(as.character(25:64)),
         "UtbildningsNiva" = utbildningsniva_vekt,
         "ContentsCode"= "UF0506A1" ,
         "Tid" = tid)
  
  # Download data 
  px_data <- 
    pxweb_get(url=url,
              query = pxweb_query_list)
  
  # Convert to data.frame 
  px_df <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
  
  if (spara_data==TRUE){
    write.xlsx(px_df,paste0(output_mapp,filnamn))
  }
  
  # Data returneras som en DF om användaren vill det
  if(returnera_data == TRUE) return(px_df) 
  
}



}
