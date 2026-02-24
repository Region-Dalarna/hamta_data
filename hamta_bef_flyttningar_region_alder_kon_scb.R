
hamta_bef_flyttningar_region_alder_kon_scb <- function(
    region_vekt = "20",                       # Dalarna defaultvärde, tabellen innehåller kommuner, län och riket, det är regionkoder som skickas med
    kon_klartext = NA,                        # finns "kvinnor", "män", c("män", "kvinnor"), NA = så skippar vi denna variabel
    alder_koder = NA,                         # NA så skippar vi denna variabel, finns 1 årsgrupper, 1 - 100+ samt "tot" för alla åldrar - dock, se upp för dubbelräkning med "tot"
    cont_klartext = "*",                      # finns: "Inflyttningar", "Utflyttningar", "Invandringar", "Utvandringar", "Flyttningsöverskott", "Invandringsöverskott", "Inrikes inflyttningar", "Inrikes utflyttningar", "Inrikes flyttningsöverskott"
    tid_koder = "*",                          # * = alla år, finns från 1997, "9999" = senaste år  (koder och klartext är samma sak i denna tabell)
    returnera_df = TRUE,                      # FALSE om man inte vill returnera en df
    mapp_excelfil = NA,                       # var man vill spara sin Excelfil om man vill spara en sådan
    filnamn_excelfil = NA,                    # filnamn på sin excelfil 
    long_format = TRUE                        # TRUE om vi vill ha df i long-format, annars kommer alla innehållsvariabler i wide-format
) {
  
  # ===========================================================================================================
  #
  # Skript för att hämta data från SCB för flyttningar årsvis. 
  # Denna tabell: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__BE__BE0101__BE0101J/Flyttningar97/  
  #
  # Möjlig och ganska enkel utveckling av funktionen vore att plocka med tabell för flyttningar år 1968-1996. 
  #
  # Skapat av: Peter Möller, Region Dalarna
  #            januari 2024
  #
  # Senast uppdaterat:  
  #                     
  # 
  # ===========================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
                 pxweb,
                 writexl)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R", encoding = "utf-8", echo = FALSE)
  options(dplyr.summarise.inform = FALSE)
  
  skriv_excelfil <- if (!is.na(mapp_excelfil) & !is.na(filnamn_excelfil)) TRUE else FALSE
  
  url_list <- c("https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101J/Flyttningar97",
                "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101J/Flyttningar97CKM")
  
  if (returnera_df | skriv_excelfil) {                       # skriptet körs bara om användaren valt att returnera en dataframe och/eller excelfil
    
    giltig_tid <- map(url_list, ~ hamta_giltiga_varden_fran_tabell(.x, "tid")) %>% unlist() %>% unique()      # hämta alla giltiga år som finns i alla medskickade tabeller
    senaste_ar <- giltig_tid %>% max()      # hämta senaste år som finns i alla medskickade tabeller
    hamta_tid <- if (all(tid_koder == "*")) hamta_tid <- giltig_tid else {
      tid_koder <- tid_koder %>% str_replace("9999", senaste_ar)
      hamta_tid <- tid_koder[tid_koder %in% giltig_tid]
    } 
      
    # url till tabellen i SCB:s statistikdatabas
    hamta_data <- function(url_uttag) {
      
      px_meta <- pxweb_get(url_uttag)
      ar_ckm <- str_detect(tolower(url_uttag), "ckm")
        
      cont_koder <- if (all(cont_klartext == "*")) cont_klartext else hamta_kod_med_klartext(px_meta, cont_klartext, skickad_fran_variabel = "contentscode")        #        hamta_kod_med_klartext(url_uttag, cont_klartext_vekt)                            # vi använder klartext i parametrar för innehållsvariabel, koder i övriga
      # vi tar bara med kvinnor eller män, alternativt NA om man väljer bort kön
      kon_koder <- if (all(!is.na(kon_klartext))) hamta_kod_med_klartext(px_meta, kon_klartext, skickad_fran_variabel = "kon") else NA
      kon_koder <- kon_koder[kon_koder %in% c("1", "2")]        # ta bara med koder för kvinnor och män, inte totalt som bara finns i CKM-tabellerna
      
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
      }
      # hantering av tid (i detta fall år) och att kunna skicka med "9999" som senaste år
      giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "tid")
      tabell_tid <- hamta_tid[hamta_tid %in% giltiga_ar]
      
      if (length(tabell_tid) > 0) {
        # variabler som vi vill ha med i uttaget
        varlista <- list(
          Region = region_vekt,
          Kon = kon_koder,
          Alder = alder_hamta,
          ContentsCode = cont_koder,
          Tid = tabell_tid
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
