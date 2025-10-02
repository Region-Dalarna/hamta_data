hamta_etableringstid_mm_region_kon_utbniv_bakgrvar_tid_scb_ny <- function(
    region_vekt = "20",			   # Val av region. Finns: "01", "03", "04", "05", "06", "07", "08", "09", "10", "12", "13", "14", "17", "18", "19", "20", "21", "22", "23", "24", "25"
    kon_klartext = "*",			 #  Finns: "män och kvinnor", "män", "kvinnor"
    utbniv_klartext = "*",			 #  Finns: "samtliga utbildningsnivåer", "utbildningsnivå: förgymnasial utbildning", "utbildningsnivå: gymnasial utbildning", "utbildningsnivå: eftergymnasial utbildning", "utbildningsnivå: uppgift saknas"
    bakgrvar_klartext = "*",			 #  Finns: "ålder: 20-24 år", "ålder: 25-34 år", "ålder: 35-44 år", "ålder: 45-54 år", "ålder: 55-64 år", "ålder: 55-65 år", "samtliga", "födelseregion: Sverige", "födelseregion: Norden exkl. Sverige", "födelseregion: EU/EFTA exkl. Norden", "födelseregion: övriga världen", "samtliga utrikes födda invandrare", "skäl till invandring: skyddsbehövande och deras anhöriga", "skäl till invandring: övriga utrikes födda invandrare", "samtliga utrikes födda", "vistelsetid 0-1 år", "vistelsetid 2-3 år", "vistelsetid 4-9 år", "vistelsetid 10- år"
    cont_klartext = "*",			 #  Finns: "Andel sysselsatta", "Andel företagare av de sysselsatta", "Andel inskrivna arbetslösa", "Andel öppet arbetslösa", "Andel sökande i program med aktivitetsstöd", "Andel långtidsarbetslösa", "Andel fortfarande sysselsatta efter ett år", "Andel fortfarande företagare efter ett år", "Andel i chefsposition", "Andel med eftergymn. utb. med arbete inom yrkesomr. 2-3 enligt SSYK", "Andel med eftergymn. utb. med arbete på kval.nivå 3-4 enligt SSYK"
    tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2022", "2023"
    long_format = TRUE,			# TRUE = konvertera innehållsvariablerna i datasetet till long-format 
    wide_om_en_contvar = TRUE,			# TRUE = om man vill behålla wide-format om det bara finns en innehållsvariabel, FALSE om man vill konvertera till long-format även om det bara finns en innehållsvariabel
    output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
    excel_filnamn = "etableringstid_mm.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
    returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
){
  
  # ====================================================================================================
  #
  # Funktion för att hämta data från SCB:s API med hjälp av pxweb-paketet
  # Automatgenererat av en funktion i R som skrivits av Peter Möller, Region Dalarna
  #
  # Skapad av: frkjon den 8 januari 2025
  # Senast uppdaterad: 2 oktober 2025
  #
  # url till tabellens API: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AA__AA0003__AA0003B/IntGr1LanUtbBAS/
  #
  # Lagt till kommuntabellen i samma skript så att både kommuner och län kan hämtas med samma skript.
  #
  # Notera att skript bara hämtar data från den nya tabellen (bas), med data för 2022 och 2023. Finns även en gammal tabell (rams) med äldre data.
  # Blir konstiga resultat om man slår ihop tabellerna, då vissa variabler byter namn. Den äldre tabellen finns i statistikdatabasen under ämnesövergripande statistik/statistik mot arbetsmarknaden/äldre tabeller som inte uppdateras.
  # Det har inte skapats något hämta data-skript för den äldre tabellen ännu.
  #
  # ====================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
         tidyverse,
         writexl)
  
  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  # Url till databas
  url_list <- c("https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AA/AA0003/AA0003B/IntGr1LanUtbBAS",
                "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AA/AA0003/AA0003B/IntGr1KomUtbBAS")
  
  hamta_data_tabell <- function(url_uttag) {
    
    px_meta <- pxweb_get(url_uttag)
    
    varlist_koder <- pxvarlist(px_meta)$koder
    varlist_bada <- pxvarlist(px_meta)
    
    giltiga_regioner <- hamta_giltiga_varden_fran_tabell(px_meta, "region")
    hamta_region <- giltiga_regioner[giltiga_regioner %in% region_vekt]
    
    # Gör om från klartext till kod som databasen förstår
    kon_vekt <- hamta_kod_med_klartext(px_meta, kon_klartext, skickad_fran_variabel = "kon")
    utbniv_vekt <- hamta_kod_med_klartext(px_meta, utbniv_klartext, skickad_fran_variabel = "utbniv")
    bakgrvar_vekt <- hamta_kod_med_klartext(px_meta, bakgrvar_klartext, skickad_fran_variabel = "bakgrvar")
    
    cont_vekt <-  hamta_kod_med_klartext(px_meta, cont_klartext, "contentscode")
    if (length(cont_vekt) > 1) wide_om_en_contvar <- FALSE
    
    # Hantera tid-koder
    giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "tid")
    tid_vekt <- if (all(tid_koder != "*")) tid_koder %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar] %>% unique() else giltiga_ar
    
    if (length(hamta_region) > 0) {
      # query-lista till pxweb-uttag
      varlista <- list(
        "Region" = hamta_region,
        "Kon" = kon_vekt,
        "UtbNiv" = utbniv_vekt,
        "BakgrVar" = bakgrvar_vekt,
        "ContentsCode" = cont_vekt,
        "Tid" = tid_vekt)
      
      # Hämta data med varlista
      px_uttag <- pxweb_get(url = url_uttag, query = varlista)
      
      var_vektor <- c(regionkod = "Region")
      var_vektor_klartext <- "region"
      
      # gör om pxweb-uttaget till en dataframe
      px_df <- as.data.frame(px_uttag)
      if (!all(is.na(var_vektor))) {
        # om man vill ha med koder också för variabler utöver klartext så läggs de på här (om det finns värden i var_vektor)
        px_df <- px_df %>%
          cbind(as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>%
                  select(any_of(var_vektor)))
        
        # kolumnerna med koder läggs framför motsvarande kolumner med klartext
        for (varflytt_index in 1:length(var_vektor)) {
          px_df <- px_df %>%
            relocate(any_of(names(var_vektor)[varflytt_index]), .before = any_of(var_vektor_klartext[varflytt_index]))
        }
      }
      return(px_df)
    } # slut if-sats där vi testar om det finns någon region att hämta i denna tabell
  } # slut hamta_data_tabell-funktion
  
  retur_df <- map(url_list, ~hamta_data_tabell(.x)) %>% 
    list_rbind()
  
  # man kan välja bort long-format, då låter vi kolumnerna vara wide om det finns fler innehållsvariabler, annars
  # pivoterar vi om till long-format, dock ej om det bara finns en innehållsvariabel
  if (long_format & !wide_om_en_contvar) retur_df <- retur_df %>% konvertera_till_long_for_contentscode_variabler(url_uttag)
  
  # Om användaren vill spara data till en Excel-fil
  if (!is.na(output_mapp) & !is.na(excel_filnamn)){
    write.xlsx(retur_df, paste0(output_mapp, excel_filnamn))
  }
  
  # Returnera data som en dataframe om användern valt det
  if (returnera_df) return(retur_df)
} # slut hämta data-funktion
