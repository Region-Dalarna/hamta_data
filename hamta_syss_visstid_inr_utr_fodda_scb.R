
hamta_syss_vistelsetid_inr_utr_fodda_scb <- function(
    region_vekt = "20",  
    cont_klartext = "Andel förvärvsarbetande (ny definition från och med 2019)",
    kon_klartext = "män och kvinnor",                     # finns: "män och kvinnor", "män", "kvinnor"
    utbniva_klartext = "samtliga utbildningsnivåer",      # finns: "samtliga utbildningsnivåer", "utbildningsnivå: förgymnasial utbildning", "utbildningsnivå: gymnasial utbildning", "utbildningsnivå: eftergymnasial utbildning", "utbildningsnivå: uppgift saknas"
    bakgr_klartext = c("vistelsetid 0-1 år", "vistelsetid 2-3 år", "vistelsetid 4-9 år",                # finns: "samtliga 20-64 år", "ålder: 20-24 år", "ålder: 25-34 år", "ålder: 35-44 år", "ålder: 45-54 år", "ålder: 55-64 år", "samtliga", "födelseregion: Sverige", "födelseregion: Norden exkl. Sverige", "födelseregion: EU/EFTA exkl. Norden", "födelseregion: övriga världen", "samtliga utrikes födda invandrare", 
                       "vistelsetid 10- år", "födelseregion: Sverige"),                                                            # "samtliga utrikes födda invandrare", "skäl till invandring: skyddsbehövande och deras anhöriga", "skäl till invandring: övriga utrikes födda invandrare", "samtliga utrikes födda", "vistelsetid 0-1 år", "vistelsetid 2-3 år", "vistelsetid 4-9 år", "vistelsetid 10- år"
    tid_vekt = "9999",                                     # 9999 = senaste år, NA = alla år
    long_format = TRUE
) {
  
  # ====================================================================================================
  #
  # Hämtar data från SCB:s öppna databas, från registerdata för integration (ämnesövergripande statistik) 
  # i tabellerna med sysselsättning för vistelsetid etc., utbildningsnivå, kön etc.
  # De ligger i 3 olika tabeller, en för riket, en för län och en för kommuner. Detta skript hämtar data
  # för de nivåer man skickar med, så det kan hämta riket, län och kommuner
  #
  # Skapat av: Peter Möller, Region Dalarna i november 2023
  # Ändrat senast: 21 december 2023
  # - lagt till paket som inte låg med samt lagt in pacman, flyttat allt till innanför funktionen så det
  #   syns när man source:ar i github. 
  #
  # ====================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 pxweb)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  
  url_adress <- c("https://api.scb.se/OV0104/v1/doris/sv/ssd/AA/AA0003/AA0003B/IntGr1RikKonUtb",
                  "https://api.scb.se/OV0104/v1/doris/sv/ssd/AA/AA0003/AA0003B/IntGr1LanKonUtb",
                  "https://api.scb.se/OV0104/v1/doris/sv/ssd/AA/AA0003/AA0003B/IntGr1KomKonUtb")
  
  giltig_tid <- hamta_giltiga_varden_fran_tabell(url_adress[1], "tid")
  
  # ge tid_var värde utifrån om det är NA, "9999" eller har medskickade år
  tid_var <- if (all(is.na(tid_vekt))) giltig_tid else if (all(tid_vekt == "9999")) giltig_tid %>% max() else tid_vekt
    
  tid_var <- tid_var[tid_var %in% giltig_tid]         # behåll bara år som finns i tabellen
  
  if (length(tid_vekt) > 0) {              # 
  # funktion för att hämta data i de tre olika tabellerna (en för riket, en för län och en för kommuner)
    hamta_data_url <- function(vald_url) {
      
      giltiga_regioner <- hamta_giltiga_varden_fran_tabell(vald_url, "region")
      region_var <- region_vekt[region_vekt %in% giltiga_regioner]
      
      if (length(region_var) > 0) {
        
        cont_var <- hamta_kod_med_klartext(vald_url, cont_klartext, skickad_fran_variabel = "contentscode")
        kon_var <- hamta_kod_med_klartext(vald_url, kon_klartext, skickad_fran_variabel = "kon")
        utbniva_var <- hamta_kod_med_klartext(vald_url, utbniva_klartext, skickad_fran_variabel = "utbniv")
        bakgr_var <- hamta_kod_med_klartext(vald_url, bakgr_klartext, skickad_fran_variabel = "BakgrVar") 
        
        # API-uttag 
        px_uttag <- pxweb_get(url = vald_url,
                              query = list(
                                Region = region_var,
                                Kon = kon_var,
                                UtbNiv = utbniva_var,
                                BakgrVar = bakgr_var,
                                ContentsCode = cont_var,
                                Tid = tid_var
                              )
        ) 
        
        # Lägg API-uttaget i px_df, lägg på ytterligare ett uttag men med koder istället för klartext,
        # välj ut bara regionkolumnen i det andra uttaget, döp om den till regionkod och lägg den först av kolumnerna
        px_df <- as.data.frame(px_uttag) %>% 
          cbind(regionkod = as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>% 
                  select(Region)) %>% rename(regionkod = Region) %>% relocate(regionkod, .before = region)
        
        # man kan välja bort long-format, då låter vi kolumnerna vara wide om det finns fler innehållsvariabler, annars
        # pivoterar vi om till long-format, dock ej om det bara finns en innehållsvariabel
        if (long_format & length(cont_var) > 1) {
          px_df <- px_df %>% 
            konvertera_till_long_for_contentscode_variabler(vald_url)
          
        } # slut if-sats som kontrollera om vi vill ha df i long-format
        
        return(px_df)
      } # slut if-sats om aktuell region finns i tabellen      
     } # slut hamta_data_url-funktion    

      
      retur_df <- map_dfr(url_adress, ~hamta_data_url(.x))
      
    } else { # slut if-sats som testar om åren man skickat med som parameter finns i tabellerna
      warning(paste0("Valda år finns inte med i tabellen. Du har valt:\n", tid_vekt %>% paste0(collapse = ", "), "\n\nGiltiga år i denna tabell",
            " är:\n", giltig_tid %>% paste0(collapse = ", ")))
    } # slut else-sats för test om tid finns i tabellen
  
  return(retur_df)
} # slut funktion
  