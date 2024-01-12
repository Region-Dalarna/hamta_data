
hamta_bef_forandringar_region_alder_kon_scb <- function(
    region_vekt = "20",                       # Dalarna defaultvärde
    forandringar_klartext = "*",              # finns: "folkmängd", "folkökning", "födda", "döda", "födelseöverskott", "samtliga inflyttningar", "samtliga utflyttningar", "inflyttningar från kommuner inom länet", "inflyttningar från övriga län", "invandringar", "utflyttningar till kommuner inom länet", "utflyttningar till övriga län", "utvandringar", "flyttningsöverskott totalt", "flyttningsöverskott eget län", "flyttningsöverskott övriga Sverige", "invandringsöverskott", "justeringspost"
    kon_klartext = NA,                        # finns "män", "kvinnor", "totalt", NA = så skippar vi denna variabel
    period_klartext = "hela året",            # NA så skippar vi denna variabel, finns 1 årsgrupper, 1 - 100+ samt "tot" för alla åldrar - dock, se upp för dubbelräkning med "tot"
    tid_koder = "*",                          # alla år, finns från 1997, "9999" = senaste år  (koder och klartext är samma sak i denna tabell)
    returnera_df = TRUE,                      # FALSE om man inte vill returnera en df
    mapp_excelfil = NA,                       # var man vill spara sin Excelfil om man vill spara en sådan
    filnamn_excelfil = NA,                    # filnamn på sin excelfil 
    long_format = TRUE                        # TRUE om vi vill ha df i long-format, annars kommer alla innehållsvariabler i wide-format, om man bara har  
    # med en innehållsvariabel så blir det wide-format
) {
  
  # ===========================================================================================================
  #
  # Skript för att hämta data från SCB för befolkningsförändringar års- halvårs-, eller kvartalsvis från år 2000. 
  # Denna tabell: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__BE__BE0101__BE0101G/BefforandrKvRLK/  
  #
  # 
  # Parametrar som skickas med (= variabler i SCB-tabellen) är:
  # - Innehåll                                                    # Finns bara "Befolkningsstatistik, antal personer" så därför är det inte valbart
  # - Förändringar                                                # "*" är standard. Finns: finns: "folkmängd", "folkökning", "födda", "döda", "födelseöverskott", "samtliga inflyttningar", "samtliga utflyttningar", "inflyttningar från kommuner inom länet", "inflyttningar från övriga län", "invandringar", "utflyttningar till kommuner inom länet", "utflyttningar till övriga län", "utvandringar", "flyttningsöverskott totalt", "flyttningsöverskott eget län", "flyttningsöverskott övriga Sverige", "invandringsöverskott", "justeringspost"  
  # - Region                                                      # tabellen innehåller kommuner, län och riket, det är regionkoder som skickas med
  # - Kön                                                         # finns enbart kvinnor och män (inte totalt)
  # - Period                                                      # finns: "hela året", "första halvåret", "andra halvåret", "kvartal 1", "kvartal 2", "kvartal 3", "kvartal 4"
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
    url_uttag <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101G/BefforandrKvRLK"
    cont_kod <- "000002Z9"
    
    kon_koder <- if (!is.na(kon_klartext) & !all(kon_klartext == "*")) hamta_kod_med_klartext(url_uttag, kon_klartext, skickad_fran_variabel = "kon") else "*"
    period_koder <- if (!is.na(period_klartext)) hamta_kod_med_klartext(url_uttag, period_klartext, skickad_fran_variabel = "period") else  "*"
    forandringar_koder <- if (forandringar_klartext == "*") forandringar_klartext else hamta_kod_med_klartext(url_uttag, forandringar_klartext, skickad_fran_variabel = "forandringar")        
    
    # hantering av tid (i detta fall år) och att kunna skicka med "9999" som senaste år
    giltiga_ar <- hamta_giltiga_varden_fran_tabell(url_uttag, "tid")
    if (all(tid_koder != "*")) tid_koder <- tid_koder %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar] 
    
    # variabler som vi vill ha med i uttaget
    varlista <- list(
      Region = region_vekt,
      Kon = kon_koder,
      Period = period_koder,
      Forandringar = forandringar_koder,
      ContentsCode = cont_kod,
      Tid = tid_koder
    )
    
    if (all(is.na(kon_klartext))) varlista <- varlista[names(varlista) != "Kon"]
    # =============================================== API-uttag ===============================================
    
    cont_var_klartext <- hamta_klartext_med_kod(url_uttag, cont_kod, "contentscode")
    
    px_uttag <- pxweb_get(url = url_uttag, query = varlista) 
    
    # Lägg API-uttaget i px_df, lägg på ytterligare ett uttag men med koder istället för klartext,
    # välj ut bara regionkolumnen i det andra uttaget, döp om den till regionkod och lägg den först av kolumnerna
    retur_df <- suppressWarnings(                             # för att slippa felmmedelande om att det finns NA-värden
      as.data.frame(px_uttag) %>% 
      cbind(as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>%
              select(Region)) %>% 
      rename(regionkod = Region) %>% relocate(regionkod, .before = region) %>% 
        rename(personer = cont_var_klartext)
    )
    
    if (returnera_df) return(retur_df)
    if (skriv_excelfil) write_xlsx(retur_df, paste0(mapp_excelfil, filnamn_excelfil))
  } else print("Varken 'returnera_df' eller mapp och filnamn för excelfil har valts så då körs inte skriptet då inget returneras. Välj någon av dessa eller båda och kör skriptet igen.") # slut if-sats där vi testar att användaren valt att spara en dataframe och/eller Excelfil
  
} # slut funktion
