if (!require("pacman")) install.packages("pacman")
p_load(pxweb,
       tidyverse)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R", encoding = "utf-8", echo = FALSE)
options(dplyr.summarise.inform = FALSE)


# funktion för att lista vilka befolkningsförändringsvariabler som finns i tabellen
lista_beffor_manad_vars <- function(){
  url_beffor <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101G/ManadBefStatRegion"
  beffor_vars <- hamta_giltiga_varden_fran_tabell(url_beffor, "Forandringar", klartext = TRUE)
  return(beffor_vars)
}

# här är själva huvudfunktionen
hamta_befolkningsforandringar_manad <- function(region_vekt = "20",
                                                befforandr_klartext = c("folkmängd", "folkökning"),    # finns: "folkmängd", "folkökning", "födda", "döda", "födelseöverskott", "samtliga inflyttningar", "samtliga utflyttningar", "samtliga inrikes inflyttningar", "inflyttningar från kommuner inom länet", "inflyttningar från övriga län", "invandringar", "samtliga inrikes utflyttningar", "utflyttningar till kommuner inom länet", "utflyttningar till övriga län", "utvandringar", "flyttningsöverskott totalt", "flyttningsöverskott inrikes totalt", "flyttningsöverskott eget län", "flyttningsöverskott övriga Sverige", "invandringsöverskott", "justeringspost"
                                                kon_klartext = "totalt",                # finns: "män", "kvinnor", "totalt"
                                                tid_koder = "*",                         # "9999" = senaste år
                                                returnera_df = TRUE,                      # FALSE om man inte vill returnera en df
                                                mapp_excelfil = NA,                       # var man vill spara sin Excelfil om man vill spara en sådan
                                                filnamn_excelfil = NA,                    # filnamn på sin excelfil 
                                                long_format = TRUE                       # om man vill formatera till long-format
) { 
  
  # ===========================================================================================================
  #
  # Skript för att hämta data från SCB för befolkning månadsvis. ContentsCode är enbart ett värde (befolkning)
  # och därför inte valbar som parameter.
  # 
  # Parametrar som skickas med (= variabler i SCB-tabellen) är:
  # - Innehåll                                                    # Folkmängd (standard) och/eller Folkökning
  # - Region                                                      # tabellen innehåller kommuner, län och riket, det är regionkoder som skickas med
  # - Kön                                                         # finns enbart kvinnor och män (inte totalt)
  # - Förändringar                                                # finns: "folkmängd", "folkökning", "födda", "döda", "födelseöverskott", "samtliga inflyttningar", "samtliga utflyttningar", "samtliga inrikes inflyttningar", "inflyttningar från kommuner inom länet", "inflyttningar från övriga län", "invandringar", "samtliga inrikes utflyttningar", "utflyttningar till kommuner inom länet", "utflyttningar till övriga län", "utvandringar", "flyttningsöverskott totalt", "flyttningsöverskott inrikes totalt", "flyttningsöverskott eget län", "flyttningsöverskott övriga Sverige", "invandringsöverskott", "justeringspost"
  # - tid (dvs. år)                                               # * = alla år (standard). "9999" = senaste tillgängliga år i tabellen. År från och med 1968
  #
  # Skapat av: Peter Möller, Region Dalarna
  #            februari 2024
  # Senast uppdaterat:  
  #                     
  # ===========================================================================================================

  skriv_excelfil <- if (!is.na(mapp_excelfil) & !is.na(filnamn_excelfil)) TRUE else FALSE
  
  if (returnera_df | skriv_excelfil) {                       # skriptet körs bara om användaren valt att returnera en dataframe och/eller excelfil
    
    url_beffor <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101G/ManadBefStatRegion"
    px_meta <- pxweb_get(url = url_beffor)
    
    kon_koder <- hamta_kod_eller_klartext_fran_lista(px_meta, kon_klartext, skickad_fran_variabel = "kon")
    befforandr_koder <- hamta_kod_eller_klartext_fran_lista(px_meta, befforandr_klartext, skickad_fran_variabel = "forandringar")
    
    # hantering av tid (i detta fall år) och att kunna skicka med "9999" som senaste år
    giltiga_ar <- hamta_kod_eller_klartext_fran_lista(px_meta, "*", "tid")
    tid_koder <- tid_koder %>% 
      as.character() %>% 
      str_replace("9999", max(giltiga_ar)) %>%
      str_replace("\\*", giltiga_ar) %>% 
      .[. %in% giltiga_ar] %>% 
      unique()
    
    # ================================ API-uttag befolkningsförändringar ===============================================
    
    varlista <- list(
      Region = region_vekt,
      Kon = kon_koder,
      Forandringar = befforandr_koder,
      ContentsCode = "*",
      Tid = tid_koder
    )
  
    px_uttag <- pxweb_get(url = url_beffor, query = varlista)              # hämta data från pxweb
    
    px_df <- suppressWarnings(as.data.frame(px_uttag) %>%                        # spara i en dataframe, plocka med regionkoder 
                                cbind(as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>%
                                        select(Region))) %>% 
      rename(regionkod = Region) %>% relocate(regionkod, .before = region) %>% 
      manader_bearbeta_scbtabeller()
  
    if (returnera_df) return(px_df)
    if (skriv_excelfil) write_xlsx(px_df, paste0(mapp_excelfil, filnamn_excelfil))
    

  } else print("Varken 'returnera_df' eller mapp och filnamn för excelfil har valts så då körs inte skriptet då inget returneras. Välj någon av dessa eller båda och kör skriptet igen.") # slut if-sats där vi testar att användaren valt att spara en dataframe och/eller Excelfil
} # slut funktion
