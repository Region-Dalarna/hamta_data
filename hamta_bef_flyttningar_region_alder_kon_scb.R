
hamta_bef_flyttningar_region_alder_kon_scb <- function(
    region_vekt = "20",                       # Dalarna defaultvärde
    kon_klartext = NA,                        # finns "kvinnor", "män", c("män", "kvinnor"), NA = så skippar vi denna variabel
    alder_koder = NA,                         # NA så skippar vi denna variabel, finns 1 årsgrupper, 1 - 100+ samt "tot" för alla åldrar - dock, se upp för dubbelräkning med "tot"
    cont_klartext = "*",                      # finns: "Folkmängd", "Folkökning"
    tid_koder = "*",                          # alla år, finns från 1997, "9999" = senaste år  (koder och klartext är samma sak i denna tabell)
    returnera_df = TRUE,                      # FALSE om man inte vill returnera en df
    mapp_excelfil = NA,                       # var man vill spara sin Excelfil om man vill spara en sådan
    filnamn_excelfil = NA,                    # filnamn på sin excelfil 
    long_format = TRUE                        # TRUE om vi vill ha df i long-format, annars kommer alla innehållsvariabler i wide-format, om man bara har  
    # med en innehållsvariabel så blir det wide-format
) {
  
  # ===========================================================================================================
  #
  # Skript för att hämta data från SCB för flyttningar årsvis. 
  # Denna tabell: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__BE__BE0101__BE0101J/Flyttningar97/  
  #
  # Möjlig och ganska enkel utveckling av funktionen vore att plocka med tabell för flyttningar år 1968-1996. 
  # 
  # Parametrar som skickas med (= variabler i SCB-tabellen) är:
  # - Innehåll                                                    # Folkmängd (standard) och/eller Folkökning
  # - Region                                                      # tabellen innehåller kommuner, län och riket, det är regionkoder som skickas med
  # - Kön                                                         # finns enbart kvinnor och män (inte totalt)
  # - Ålder                                                       # NA = att variabeln utelämnas (standard), finns i ettårsgrupper tom 100+ år
  # - tid (dvs. år)                                               # * = alla år (standard). "9999" = senaste tillgängliga år i tabellen. År från och med 1968
  #
  # Skapat av: Peter Möller, Region Dalarna
  #            januari 2024
  #
  # Senast uppdaterat:  
  #                     
  # 
  # ===========================================================================================================
  
  pacman::p_load(tidyverse,
                 pxweb,
                 writexl)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R", encoding = "utf-8", echo = FALSE)
  options(dplyr.summarise.inform = FALSE)
  
  skriv_excelfil <- if (!is.na(mapp_excelfil) & !is.na(filnamn_excelfil)) TRUE else FALSE
  
  if (returnera_df | skriv_excelfil) {                       # skriptet körs bara om användaren valt att returnera en dataframe och/eller excelfil
    
    # url till tabellen i SCB:s statistikdatabas
    url_uttag <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101J/Flyttningar97"
    
    cont_koder <- if (cont_klartext == "*") cont_klartext else hamta_kod_med_klartext(url_uttag, cont_klartext, skickad_fran_variabel = "contentscode")        #        hamta_kod_med_klartext(url_uttag, cont_klartext_vekt)                            # vi använder klartext i parametrar för innehållsvariabel, koder i övriga
    kon_koder <- if (!is.na(kon_klartext)) hamta_kod_med_klartext(url_uttag, kon_klartext, skickad_fran_variabel = "kon") else  "*"
    alder_koder <- alder_koder %>% as.character()       # säkerställ att alder_koder är character och inte numeric
    
    # hantering av tid (i detta fall år) och att kunna skicka med "9999" som senaste år
    senaste_ar <- hamta_giltiga_varden_fran_tabell(url_uttag, "tid") %>% max()
    if (max(tid_koder) == "9999") {
      min_ar <- min(tid_koder)
      if (min_ar == "9999") min_ar <- senaste_ar
    }
    tid_koder <- tid_koder %>% as.character()
    
    # variabler som vi vill ha med i uttaget
    varlista <- list(
      Region = region_vekt,
      Kon = kon_koder,
      Alder = alder_koder,
      ContentsCode = cont_koder,
      Tid = tid_koder
    )
    
    if (all(is.na(kon_klartext))) varlista <- varlista[names(varlista) != "Kon"]
    if (all(is.na(alder_koder))) varlista <- varlista[names(varlista) != "Alder"]
    # =============================================== API-uttag ===============================================
    
    px_uttag <- pxweb_get(url = url_uttag, query = varlista) 
    
    # Lägg API-uttaget i px_df, lägg på ytterligare ett uttag men med koder istället för klartext,
    # välj ut bara regionkolumnen i det andra uttaget, döp om den till regionkod och lägg den först av kolumnerna
    retur_df <- as.data.frame(px_uttag) %>% 
      cbind(as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>%
              select(Region)) %>% 
      rename(regionkod = Region) %>% relocate(regionkod, .before = region)
    
    antal_cont <- sum(names(retur_df) %in% pxvardelist(url_uttag, "contentscode")$klartext)        # räkna antal contentsvariabler
    
    # man kan välja bort long-format, då låter vi kolumnerna vara wide om det finns fler innehållsvariabler, annars
    # pivoterar vi om till long-format, dock ej om det bara finns en innehållsvariabel
    if (long_format & antal_cont > 1) {
      retur_df <- retur_df %>% 
        konvertera_till_long_for_contentscode_variabler(url_uttag)
      
    } # slut if-sats som kontrollera om vi vill ha df i long-format
    
    if (returnera_df) return(retur_df)
    if (skriv_excelfil) write_xlsx(retur_df, paste0(mapp_excelfil, filnamn_excelfil))
  } else print("Varken 'returnera_df' eller mapp och filnamn för excelfil har valts så då körs inte skriptet då inget returneras. Välj någon av dessa eller båda och kör skriptet igen.") # slut if-sats där vi testar att användaren valt att spara en dataframe och/eller Excelfil
  
} # slut funktion
