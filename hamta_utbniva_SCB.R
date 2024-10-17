#test_list=hamta_data_utbniva()
hamta_data_utbniva <- function(region = hamtakommuner("20",tamedlan = TRUE,tamedriket = TRUE), # Använd förslagsvis hamtakommuner eller hamtaallalan
                               alder = c(as.character(25:64)), # antingen "tot16-74" eller annat intervall, exempelvis c(as.character(25:64)), "*" ger alla år
                               utbildningsniva_klartext = "*", # För alternativ, se text nedan
                               kon_klartext = c("män","kvinnor"), # c("män","kvinnor") eller var och en uppdelad. "*" funkar inte
                               output_mapp = NA, # Här hamnar data. Måste väljas om man vill spara data
                               filnamn = "utbildningsniva.xlsx", # Filnamn, om man vill spara data
                               returnera_data = TRUE, # Om man vill returnera data
                               tid ="9999" # Sätts till "9999" om man enbart vill ha senaste år,"*" för alla. Se nedan för ytterligare förklaringar (intervall)
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
  #
  # tid: För ett intervall som slutar med sista tillgängliga år, skriv startår:"9999". Exempelvis 2010:"9999".
  # Funkar även med enstaka år c(2010,2015,"9999")
  #
  # Skapad av Jon Frank
  # Uppdaterad senast 2024-10-17 av Peter Möller. Lagt till funktionalitet
  # ===========================================================================================================
  
  
  # ========================================== Läser in data ============================================
  # Skapa en lista med information som vi vill ha hem -- skapas lättast via pxweb_interactive()
  
  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
         tidyverse,
         openxlsx)
  
  # Funktioner som sourcas från Region Dalarna
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  # "Adresser" till SCBs databas
  url_uttag <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/UF/UF0506/UF0506B/Utbildning"
  
  # Gör om från klartext till något som PXweb förstår
  kon_vekt <- hamta_kod_med_klartext(url_uttag, kon_klartext, skickad_fran_variabel = "kon")
  
  if (utbildningsniva_klartext == "*") utbildningsniva_vekt = "*" else utbildningsniva_vekt <- hamta_kod_med_klartext(url_uttag, utbildningsniva_klartext, skickad_fran_variabel = "utbildningsniva")
  
  giltiga_ar <- hamta_giltiga_varden_fran_tabell(url_uttag, "tid")
  if (all(tid != "*")) tid <- tid %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar] %>% unique()
  
  pxweb_query_list <- 
    list("Region" = region,
         "Kon" = kon_vekt,
         "Alder" = alder,
         "UtbildningsNiva" = utbildningsniva_vekt,
         "ContentsCode"= "UF0506A1" ,
         "Tid" = tid)
  
  if (all(is.na(alder))) pxweb_query_list <- pxweb_query_list[names(pxweb_query_list) != "Alder"]
  
  # Download data 
  px_uttag <- 
    pxweb_get(url = url_uttag,
              query = pxweb_query_list)
  
  # Convert to data.frame 
  px_df <- as.data.frame(px_uttag) %>% 
    cbind(as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>% 
            select(regionkod = Region)) %>% 
    relocate(regionkod, .before = 1)
  
  if (!is.na(output_mapp) & !is.na(filnamn)){
    write.xlsx(px_df,paste0(output_mapp,filnamn))
  }
  
  # Data returneras som en DF om användaren vill det
  if(returnera_data == TRUE) return(px_df) 
  
}
