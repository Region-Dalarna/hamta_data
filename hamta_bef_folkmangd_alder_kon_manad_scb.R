hamta_bef_folkmangd_alder_kon_manad_scb <- function(
    region_vekt = "20",                       # Dalarna defaultvärde
    kon_klartext = c("män", "kvinnor"),       # finns "kvinnor", "män"
    alder_koder = NA,                         # NA så skippar vi denna variabel, finns 1 årsgrupper, 1 - 100+ samt "tot" för alla åldrar - dock, se upp för dubbelräkning med "tot"
    tid_koder = "*",                          # alla år, finns från 1968, "9999" = senaste år  (koder och klartext är samma sak i denna tabell)
    returnera_df = TRUE,                      # FALSE om man inte vill returnera en df
    mapp_excelfil = NA,                       # var man vill spara sin Excelfil om man vill spara en sådan
    filnamn_excelfil = NA                    # filnamn på sin excelfil 
    ) {
  
  # ===========================================================================================================
  #
  # Skript för att hämta data från SCB för befolkning sista december årsvis.  
  # 
  # Parametrar som skickas med (= variabler i SCB-tabellen) är:
  # - Innehåll                                                    # Folkmängd (standard) och/eller Folkökning
  # - Region                                                      # tabellen innehåller kommuner, län och riket, det är regionkoder som skickas med
  # - Kön                                                         # finns enbart kvinnor och män (inte totalt)
  # - Ålder                                                       # NA = att variabeln utelämnas (standard), finns i ettårsgrupper tom 100+ år
  # - tid (dvs. år)                                               # * = alla år (standard). "9999" = senaste tillgängliga år i tabellen. År från och med 1968
  #
  # Skapat av: Peter Möller, Region Dalarna
  #            februari 2024
  # Senast uppdaterat:  
  #                     
  # ===========================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 pxweb,
                 writexl)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R", encoding = "utf-8", echo = FALSE)
  options(dplyr.summarise.inform = FALSE)
  
  skriv_excelfil <- if (!is.na(mapp_excelfil) & !is.na(filnamn_excelfil)) TRUE else FALSE
  
  if (returnera_df | skriv_excelfil) {                       # skriptet körs bara om användaren valt att returnera en dataframe och/eller excelfil
    
    # url till tabellen i SCB:s statistikdatabas
    url_uttag <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkManad/"
    px_meta <- pxweb_get(url_uttag)
    
    if (!all(is.na(alder_koder))) alder_koder <- alder_koder %>% as.character()
    if (!all(is.na(kon_klartext))) kon_koder <- hamta_kod_med_klartext(url_uttag, kon_klartext, skickad_fran_variabel = "kon") else kon_koder <- NA

    # hantering av tid (i detta fall år) och att kunna skicka med "9999" som senaste år
    giltiga_ar <- hamta_kod_eller_klartext_fran_lista(px_meta, "*", "tid")
    tid_koder <- tid_koder %>% 
      as.character() %>% 
      str_replace("9999", max(giltiga_ar)) %>%
      str_replace("\\*", giltiga_ar) %>% 
      .[. %in% giltiga_ar] %>% 
      unique()
    
    # variabler som vi vill ha med i uttaget
    varlista <- list(
      Region = region_vekt,
      Kon = kon_koder,
      Alder = alder_koder,
      ContentsCode = "*",
      Tid = tid_koder
      )
    
    if (all(is.na(kon_koder))) varlista <- varlista[names(varlista) != "Kon"]
    if (all(is.na(alder_koder))) varlista <- varlista[names(varlista) != "Alder"]
    # =============================================== API-uttag ===============================================
    
    px_uttag <- pxweb_get(url = url_uttag, query = varlista) 
    
    # Lägg API-uttaget i px_df, lägg på ytterligare ett uttag men med koder istället för klartext,
    # välj ut bara regionkolumnen i det andra uttaget, döp om den till regionkod och lägg den först av kolumnerna
    retur_df <- as.data.frame(px_uttag) %>% 
      cbind(as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>%
              select(Region)) %>% 
      rename(regionkod = Region) %>% relocate(regionkod, .before = region) %>% 
      manader_bearbeta_scbtabeller()
    
    if (returnera_df) return(retur_df)
    if (skriv_excelfil) write_xlsx(retur_df, paste0(mapp_excelfil, filnamn_excelfil))
  } else print("Varken 'returnera_df' eller mapp och filnamn för excelfil har valts så då körs inte skriptet då inget returneras. Välj någon av dessa eller båda och kör skriptet igen.") # slut if-sats där vi testar att användaren valt att spara en dataframe och/eller Excelfil
  
} # slut funktion
