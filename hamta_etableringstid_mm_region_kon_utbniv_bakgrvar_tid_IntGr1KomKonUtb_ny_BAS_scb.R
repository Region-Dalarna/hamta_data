hamta_etableringstid_mm_region_kon_utbniv_bakgrvar_tid_scb_ny <- function(
    region_vekt = "20",			   # Val av region. Finns: "01", "03", "04", "05", "06", "07", "08", "09", "10", "12", "13", "14", "17", "18", "19", "20", "21", "22", "23", "24", "25"
    kon_klartext = "*",			 #  Finns: "män och kvinnor", "män", "kvinnor"
    utbniv_klartext = "*",			 #  Finns: "samtliga utbildningsnivåer", "utbildningsnivå: förgymnasial utbildning", "utbildningsnivå: gymnasial utbildning", "utbildningsnivå: eftergymnasial utbildning", "utbildningsnivå: uppgift saknas"
    bakgrvar_klartext = "*",			 #  Finns: "samtliga 20-64 år", "ålder: 20-24 år", "ålder: 25-34 år", "ålder: 35-44 år", "ålder: 45-54 år", "ålder: 55-64 år", "samtliga", "födelseregion: Sverige", "födelseregion: Norden exkl. Sverige", "födelseregion: EU/EFTA exkl. Norden", "födelseregion: övriga världen", "samtliga utrikes födda invandrare", "skäl till invandring: skyddsbehövande och deras anhöriga", "skäl till invandring: övriga utrikes födda invandrare", "samtliga utrikes födda", "vistelsetid 0-1 år", "vistelsetid 2-3 år", "vistelsetid 4-9 år", "vistelsetid 10- år", "ålder: 55-65 år"
    cont_klartext = "*",			 #  Finns: "Andel förvärvsarbetande (ny definition från och med 2019)", "Andel företagare av de förvärvsarbetande (ny definition från och med 2019)", "Andel inskrivna arbetslösa, procent", "Andel öppet arbetslösa, procent", "Andel sökande i program med aktivitetsstöd, procent", "Andel långtidsarbetslösa, procent", "Andel fortfarande förvärvsarbetande efter ett år (ny def. från och med 2019)", "Andel fortfarande företagare efter ett år (ny definition från och med 2019)", "Andel i chefsposition, procent", "Andel med eftergymn. utb. med arbete inom yrkesomr. 2-3 enligt SSYK, procent", "Andel med eftergymn. utb. med arbete på kval.nivå 3-4 enligt SSYK, procent", "Andel sysselsatta", "Andel företagare av de sysselsatta", "Andel inskrivna arbetslösa", "Andel öppet arbetslösa", "Andel sökande i program med aktivitetsstöd", "Andel långtidsarbetslösa", "Andel fortfarande sysselsatta efter ett år", "Andel fortfarande företagare efter ett år", "Andel i chefsposition", "Andel med eftergymn. utb. med arbete inom yrkesomr. 2-3 enligt SSYK", "Andel med eftergymn. utb. med arbete på kval.nivå 3-4 enligt SSYK"
    tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023"
    long_format = TRUE,			# TRUE = konvertera innehållsvariablerna i datasetet till long-format 
    wide_om_en_contvar = TRUE,			# TRUE = om man vill behålla wide-format om det bara finns en innehållsvariabel, FALSE om man vill konvertera till long-format även om det bara finns en innehållsvariabel
    output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
    excel_filnamn = "nystartade_bransch.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
    returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
){
  
  # ====================================================================================================
  #
  # Funktion för att hämta data från SCB:s API med hjälp av pxweb-paketet
  # Automatgenererat av en funktion i R som skrivits av Peter Möller, Region Dalarna
  #
  # Skapad av: frkjon den 07 januari 2025
  # Senast uppdaterad: 07 januari 2025
  #
  # url till tabellens API: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AA__AA0003__AA0003X/IntGr1LanKonUtb/
  #												https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AA__AA0003__AA0003B/IntGr1LanUtbBAS/
  #
  # Notera att data slås ihop från två olika tabeller, där den första använder RAMS och den andra BAS. Ålder har sannolikt ändrats från 20-64 år till 20-65 år.
  # Notera även att vissa kategorier, exempelvis sysselsatta byter namn från förvärvsarbetande till sysselsatta.
  # För mer info, se länkarna till statistikdatabasen ovan.
  # ====================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
         tidyverse,
         writexl)
  
  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  # Url till SCB:s databas
  url_list <- c("https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AA/AA0003/AA0003X/IntGr1LanKonUtb",
                "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AA/AA0003/AA0003B/IntGr1LanUtbBAS")
  
  hamta_data <- function(url_uttag) {
    
    px_meta <- pxweb_get(url_uttag)
    
    varlist_koder <- pxvarlist(px_meta)$koder
    varlist_bada <- pxvarlist(px_meta)
    
    # Gör om från klartext till kod som databasen förstår
    kon_vekt <- hamta_kod_med_klartext(px_meta, kon_klartext, skickad_fran_variabel = "kon")
    utbniv_vekt <- hamta_kod_med_klartext(px_meta, utbniv_klartext, skickad_fran_variabel = "utbniv")
    bakgrvar_vekt <- hamta_kod_med_klartext(px_meta, bakgrvar_klartext, skickad_fran_variabel = "bakgrvar")
    
    cont_vekt <-  hamta_kod_med_klartext(px_meta, cont_klartext, "contentscode")
    if (length(cont_vekt) > 1) wide_om_en_contvar <- FALSE
    
    # Hantera tid-koder
    giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "tid")
    tid_vekt <- if (all(tid_koder != "*")) tid_koder %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar] %>% unique() else giltiga_ar
    
    # query-lista till pxweb-uttag
    varlista <- list(
      "Region" = region_vekt,
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
    
    # man kan välja bort long-format, då låter vi kolumnerna vara wide om det finns fler innehållsvariabler, annars
    # pivoterar vi om till long-format, dock ej om det bara finns en innehållsvariabel
    if (long_format & !wide_om_en_contvar) px_df <- px_df %>% konvertera_till_long_for_contentscode_variabler(url_uttag)
    
    return(px_df)
  } # slut hämta data-funktion 
  
  px_alla <- map(url_list, ~ hamta_data(.x)) %>% list_rbind()
  
  # Om användaren vill spara data till en Excel-fil
  if (!is.na(output_mapp) & !is.na(excel_filnamn)){
    write.xlsx(px_alla, paste0(output_mapp, excel_filnamn))
  }
  
  # Returnera data som en dataframe om användern valt det
  if (returnera_df) return(px_alla)
} # slut hämta data-funktion
