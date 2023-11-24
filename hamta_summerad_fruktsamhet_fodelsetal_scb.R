# ====================================================================================================
#
# Hämtar data från SCB:s öppna databas, från befolkning-födda och är summerad fruktsamhet per region
# och kön. 
#
# Skapat av: Peter Möller, Region Dalarna
# November 2023
#
# ====================================================================================================
library(tidyverse)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)


hamta_summerad_fruktsamhet_fodelsetal_scb <- function(
    region_vekt = "20",  
    kon_klartext = "kvinnor",                     # finns: "män", "kvinnor"
    tid_vekt = NA                                     # 9999 = senaste år, NA = alla år
) {
  
 url_uttag <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101H/FruktsamhetSum"
   

 giltig_tid <- hamta_giltiga_varden_fran_tabell(url_uttag, "tid")
 tid_var <- if (all(is.na(tid_vekt))) giltig_tid else if (all(tid_vekt == "9999")) giltig_tid %>% max() else tid_vekt           # ge tid_var värde utifrån om det är NA, "9999" eller har medskickade år
 tid_var <- tid_var[tid_var %in% giltig_tid]         # behåll bara år som finns i tabellen
 
 kon_var <- hamta_kod_med_klartext(url_uttag, kon_klartext, skickad_fran_variabel = "kon")
 
 
 # API-uttag 
 px_uttag <- pxweb_get(url = url_uttag,
                       query = list(
                         Region = region_vekt,
                         Kon = kon_var,
                         ContentsCode = "*",
                         Tid = tid_var
                       )
 ) 
 
 # Lägg API-uttaget i px_df, lägg på ytterligare ett uttag men med koder istället för klartext,
 # välj ut bara regionkolumnen i det andra uttaget, döp om den till regionkod och lägg den först av kolumnerna
 px_df <- as.data.frame(px_uttag) %>% 
   cbind(regionkod = as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>% 
           select(Region)) %>% rename(regionkod = Region) %>% relocate(regionkod, .before = region)
 
 return(px_df)
} # slut funktion      
