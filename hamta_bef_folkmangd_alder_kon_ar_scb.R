hamta_bef_folkmangd_alder_kon_ar_scb <- function(
    region_vekt = "20",                       # Dalarna defaultvärde
    civilstand_klartext = NA,                 # NA så skippar vi denna variabel i uttaget, finns: "ogifta", "gifta", "skilda", "änkor/änklingar"
    kon_klartext = c("män", "kvinnor"),       # finns "kvinnor", "män"
    alder_koder = NA,                         # NA så skippar vi denna variabel, finns 1 årsgrupper, 1 - 100+ samt "tot" för alla åldrar - dock, se upp för dubbelräkning med "tot"
    cont_klartext = "Folkmängd",              # finns: "Folkmängd", "Folkökning"
    tid_koder = "*",                          # alla år, finns från 1968, "9999" = senaste år  (koder och klartext är samma sak i denna tabell)
    returnera_df = TRUE,                      # FALSE om man inte vill returnera en df
    mapp_excelfil = NA,                       # var man vill spara sin Excelfil om man vill spara en sådan
    filnamn_excelfil = NA,                    # filnamn på sin excelfil 
    wide_om_en_contvar = TRUE,                # TRUE om vi vill ha df i wide-format om det bara finns en innehållsvariabel, annars blir det long-format om det är valt
    long_format = TRUE,                       # TRUE om vi vill ha df i long-format, annars kommer alla innehållsvariabler i wide-format, om man bara har  
                                              # med en innehållsvariabel så blir det wide-format
    hamta_url = FALSE                         # TRUE om vi enbart vill ha url:en till tabellen
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
  # - Civilstånd                                                  # NA = att variabeln utelämnas (standard), annars finns "ogifta", "gifta", "skilda", "änkor/änklingar"
  # - tid (dvs. år)                                               # * = alla år (standard). "9999" = senaste tillgängliga år i tabellen. År från och med 1968
  #
  # Skapat av: Peter Möller, Region Dalarna
  #            november 2023
  # Senast uppdaterat:  februari 2026
  #                     lagt ihop med nya CKM-tabellen
  #                     (mindre justeringar)
  #                     Uppdaterat kod så att det går att ta med "9999" som senaste år. Tidigare kod bortkommenterad. Jon 2024-01-19
  #                     Buggfix på kon_klartext så att både "*" och NA hanteras samt lagt till möjlighet att bara hämta url till tabellen. Peter 2025-05-06
  # ===========================================================================================================
  
  # url till tabellen i SCB:s statistikdatabas
  url_list <- c("https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy",
                "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningCKM")
  
  #url_uttag <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
  if (hamta_url) return(url_uttag) # om vi bara vill ha url:en till tabellen så returnerar vi den och avslutar funktionen)
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 pxweb,
                 writexl)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R", encoding = "utf-8", echo = FALSE)
  options(dplyr.summarise.inform = FALSE)
  
  skriv_excelfil <- if (!is.na(mapp_excelfil) & !is.na(filnamn_excelfil)) TRUE else FALSE
  
  giltig_tid <- map(url_list, ~ hamta_giltiga_varden_fran_tabell(.x, "tid")) %>% unlist() %>% unique()      # hämta alla giltiga år som finns i alla medskickade tabeller
  senaste_ar <- giltig_tid %>% max()      # hämta senaste år som finns i alla medskickade tabeller
  hamta_tid <- if (all(tid_koder == "*")) hamta_tid <- giltig_tid else {
    tid_koder <- tid_koder %>% str_replace("9999", senaste_ar)
    hamta_tid <- tid_koder[tid_koder %in% giltig_tid]
  }
  
  if (returnera_df | skriv_excelfil) {                       # skriptet körs bara om användaren valt att returnera en dataframe och/eller excelfil
    
    hamta_data <- function(url_uttag) {
        
        px_meta <- pxweb_get(url_uttag)
        ar_ckm <- str_detect(tolower(url_uttag), "ckm")
      
      # hantering av ålder
        alder_koder <- alder_koder %>% as.character()
        if (!all(is.na(alder_koder))) {
          alder_giltiga_varden <- hamta_giltiga_varden_fran_tabell(px_meta, "alder") %>% 
            .[!str_detect(., "-")] %>% 
            .[!(str_detect(tolower(.), "tot") & . != "TOT1") | tolower(.) == "tot"] %>% 
            .[!(str_detect(tolower(.), "100") & . != "100+1") | . == "100+"] %>% 
            as.character()      # hämta giltiga värden för ålder och gör om till character
          # om det finns fler total ålder-grupper, behåller vi bara en
          if (sum(str_count(tolower(alder_giltiga_varden), "tot")) > 1) {
            tot_traffar <- alder_giltiga_varden[str_detect(tolower(alder_giltiga_varden), "tot")]
            tot_tabort <- tot_traffar[2:length(tot_traffar)]
            alder_giltiga_varden <- alder_giltiga_varden %>%
              .[!. %in% tot_tabort]
          }
          alder_hamta <- if (all(alder_koder == "*")) alder_giltiga_varden else alder_koder
          if (ar_ckm) {
            alder_hamta <- alder_hamta %>%                             # byt ut till gitliga koder om man skickat med 100 eller totalt 
              as.character() %>%                                       # säkerställ att alder_koder är character och inte numeric
              .[. %in% alder_giltiga_varden]
          } else alder_hamta <- alder_hamta[alder_hamta %in% alder_giltiga_varden]
        } else {
          alder_hamta <- if (ar_ckm) "TotSA" else NA
        }

      # hantering av civilstånd
      civilstand_giltiga <- hamta_giltiga_varden_fran_tabell(url_uttag, "civilstand", klartext = TRUE)           # hämta alla giltiga värden för civilstånd i klartext, oavsett vad användaren skickat med, för att kunna hantera "*" i klartext som innebär att alla giltiga värden ska vara med
      civilstand_hamta <- if (any(civilstand_klartext == "*", na.rm = TRUE)) civilstand_giltiga else civilstand_klartext       # hämta alla värden om användaren skickat med "*" i klartext, annars det som användaren skickat med
      # ta ut koder för civilstånd, ta bort "SC" som är kod för "totalt" i CKM-tabeller i uttag, men lägg dit det om det är CKM och man valt NA, i övriga tabeller NA (då elimination är FALSE i CKM-tabellen)
      civilstand_koder <- if (!all(is.na(civilstand_klartext))) hamta_kod_med_klartext(url_uttag, civilstand_hamta, skickad_fran_variabel = "civilstand") %>% .[!. %in% "SC"]  else {
        if(ar_ckm) "SC" else NA
      }
      
      cont_koder <- hamta_kod_med_klartext(url_uttag, cont_klartext, skickad_fran_variabel = "contentscode")        #        hamta_kod_med_klartext(url_uttag, cont_klartext_vekt)                            # vi använder klartext i parametrar för innehållsvariabel, koder i övriga
      
      kon_koder <- if (all(!is.na(kon_klartext))) hamta_kod_med_klartext(px_meta, kon_klartext, skickad_fran_variabel = "kon") else NA
      kon_koder <- if (all(!is.na(kon_klartext))) kon_koder[kon_koder %in% c("1", "2")]        # ta bara med koder för kvinnor och män, inte totalt som bara finns i CKM-tabellerna
      
      # hantering av tid (i detta fall år) och att kunna skicka med "9999" som senaste år
      giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "tid")
      tabell_tid <- hamta_tid[hamta_tid %in% giltiga_ar]
      
      if (length(tabell_tid) > 0) {
      # variabler som vi vill ha med i uttaget
      varlista <- list(
        Region = region_vekt,
        Kon = kon_koder,
        Alder = alder_hamta,
        Civilstand = civilstand_koder,
        ContentsCode = cont_koder,
        Tid = tabell_tid
        )
      
      if (all(is.na(civilstand_klartext)) & !ar_ckm) varlista <- varlista[names(varlista) != "Civilstand"]
      if (all(is.na(alder_koder)) & !ar_ckm) varlista <- varlista[names(varlista) != "Alder"]
      if (all(is.na(kon_koder))) varlista <- varlista[names(varlista) != "Kon"]
      # =============================================== API-uttag ===============================================
      
      px_uttag <- pxweb_get(url = url_uttag, query = varlista) 
      
      # Lägg API-uttaget i px_df, lägg på ytterligare ett uttag men med koder istället för klartext,
      # välj ut bara regionkolumnen i det andra uttaget, döp om den till regionkod och lägg den först av kolumnerna
      retur_df <- suppress_specific_warning({
        as.data.frame(px_uttag) %>% 
        cbind(as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>%
                select(Region)) %>% 
        rename(regionkod = Region) %>% relocate(regionkod, .before = region) 
      })  
        if ("ålder" %in% names(retur_df)) {
          retur_df <- retur_df %>% 
          mutate(ålder = ifelse(ålder == "totalt, samtliga åldrar", "totalt ålder", ålder))
        }
      
      
      # ta bort kolumner om vi valt bort civilstånd
      if (all(is.na(civilstand_klartext)) & ar_ckm) retur_df <- retur_df %>% select(-civilstånd)
      if (all(is.na(alder_koder)) & ar_ckm) retur_df <- retur_df %>% select(-ålder)
    
      # man kan välja bort long-format, då låter vi kolumnerna vara wide om det finns fler innehållsvariabler, annars
      # pivoterar vi om till long-format, dock ej om det bara finns en innehållsvariabel
      if (wide_om_en_contvar & length(cont_klartext) == 1) long_format <- FALSE
      
      if (long_format) {
        retur_df <- retur_df %>% 
          konvertera_till_long_for_contentscode_variabler(url_uttag)
        
      } # slut if-sats som kontrollera om vi vill ha df i long-format
      return(retur_df)
     } # slut if-sats för att kontrollera att det finns år att hämta ut i just denna tabell
    } # slut hamta_data-funktion
  
  px_df <- map(url_list, ~hamta_data(url_uttag = .x)) %>% 
    list_rbind()
    
    if (returnera_df) return(px_df)
    if (skriv_excelfil) write_xlsx(px_df, paste0(mapp_excelfil, filnamn_excelfil))
  } else print("Varken 'returnera_df' eller mapp och filnamn för excelfil har valts så då körs inte skriptet då inget returneras. Välj någon av dessa eller båda och kör skriptet igen.") # slut if-sats där vi testar att användaren valt att spara en dataframe och/eller Excelfil
  
} # slut funktion
