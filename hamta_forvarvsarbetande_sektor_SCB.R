hamta_data_forv_sektor <- function(region = hamtakommuner("20",tamedlan = TRUE,tamedriket = TRUE), # Använd förslagsvis hamtakommuner eller hamtaallalan
                                   alder_klartext = "*", # Andra val: 16-19 år, 20-24 år, 25-34 år, 35-44 år, 45-54 år, 55-59 år, 60-64 år, 65+ år
                                   arbetssektor_klartext = "*", # För alternativa val använd pxvardelist("https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0207/AM0207Z/DagSektAldKN", "ArbetsSektor")
                                   kon_klartext = c("män","kvinnor"), # c("män","kvinnor") eller var och en uppdelad (totalt saknas)
                                   output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/", # Outputmapp
                                   filnamn = "forvarvsarbetande_sektor.xlsx", # Filnamn
                                   spara_data = TRUE, # True om man vill spara data till Excel
                                   returnera_data = TRUE, # Om man vill returnera data
                                   tid = c("*") # Sätts till "99" om man enbart vill ha senaste år,"*" för alla alternativt intervall
){
  
  # ===========================================================================================================
  #
  # Skript för att hämta data för förvärvsarbetande 16-74år med arbetsplats i regionen. Uppdelat på olika sektorer 
  # 
  # För att få en djupare förklaring av vad som de olika kategorierna under varje variabel betyder, använd: 
  # pxvardelist("https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0207/AM0207Z/DagSektAldKN", "ArbetsSektor"), 
  # där man byter mot den variabel man är intresserad av.
  # 
  # Generellt gäller c("*) om man vill ha alla variabler
  # ===========================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
         tidyverse,
         openxlsx)
  
  # "Adresser" till SCBs databas
  url_uttag <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0207/AM0207Z/DagSektAldKN"
  
  # Data som sourcas från Region Dalarna
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  # Gör om från klartext
  kon_vekt <- hamta_kod_med_klartext(url_uttag, kon_klartext, skickad_fran_variabel = "kon")
  
  if (alder_klartext == "*"){
    
    alder_vekt = "*"
    
  }else alder_vekt <- hamta_kod_med_klartext(url_uttag, alder_klartext, skickad_fran_variabel = "alder")
  
  if (arbetssektor_klartext == "*"){
    
    arbetssektor_vekt = "*"
    
  }else arbetssektor_vekt <- hamta_kod_med_klartext(url_uttag, arbetssektor_klartext, skickad_fran_variabel = "alder")
  
  # Om användaren bara vill ha senaste år
  if(tid == "99") tid = max(hamta_giltiga_varden_fran_tabell(url_uttag, "tid"))
  
  varlista <- list(Region = region,
                   ArbetsSektor = arbetssektor_vekt,
                   Alder = alder_vekt,
                   Kon= kon_vekt,
                   ContentsCode = "00000545",
                   Tid = tid)
  
  px_uttag <- pxweb_get(url = url_uttag,query = varlista)
  
  # Gör uttaget samt diverse justeringar och grupperingar av data.
  antal_sektor_df <- as.data.frame(px_uttag) %>% 
    cbind(as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>%
            select(regionkod = Region)) %>%
    relocate(regionkod, .before = region) 
  
  
  if (spara_data==TRUE){
    write.xlsx(antal_sektor_df,paste0(output_mapp,filnamn))
  }
  
  # Data returneras som en DF om användaren vill det
  if(returnera_data == TRUE) return(antal_sektor_df) 
  
}


