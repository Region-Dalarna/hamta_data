hamta_antagna_yh_region_utbildngrupp_kon_tid_scb <- function(
    region_vekt = "20",			# Val av region. Finns: "00", "FA00", "FA01", "FA02", "FA03", "FA04", "FA05", "FA06", "FA07", "FA08", "FA09", "FA10", "FA11", "FA12", "FA13", "FA14", "FA15", "FA16", "FA17", "FA18", "FA19", "FA20", "FA21", "FA22", "FA23", "FA24", "FA25", "FA26", "FA27", "FA28", "FA29", "FA30", "FA31", "FA32", "FA33", "FA34", "FA35", "FA36", "FA37", "FA38", "FA39", "FA40", "FA41", "FA42", "FA43", "FA44", "FA45", "FA46", "FA47", "FA48", "FA49", "FA50", "FA51", "FA52", "FA53", "FA54", "FA55", "FA56", "FA57", "FA58", "FA59", "FA60", "01", "03", "04", "05", "06", "07", "08", "09", "FA99", "10", "12", "13", "14", "17", "18", "19", "20", "21", "22", "23", "24", "25", "999"
    utbildngrupp_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "samtliga utbildningsgrupper", "yrkeslärarutbildning ", "övrig utbildning inom pedagogik / lärarutbildning, eftergymnasial", "konstnärlig utbildning, eftergymnasial nivå ", "utbildning inom medieproduktion, eftergymnasial nivå ", "övrig utbildning inom humaniora och konst, eftergymnasial nivå ", "journalistik och medievetenskaplig utbildning, eftergymnasial nivå ", "övrig utb. i samhällsvetenskap, juridik, handel, admin., eftergymnasial", "medicinsk sekreterarutbildning", "Yh-utbildning i företagsekonomi, handel, administration", "datautbildning, eftergymnasial nivå ", "data- och it-utbildning, eftergymnasial (kortare än 3 år)", "teknikutbildning, yrkeshögskolan", "agronom- och hortonomutbildning ", "övrig utb. inom lant- och skogsbruk, djursjukvård, eftergymnasial", "barn- och fritidsutbildning, gymnasial nivå ", "vård- och omsorgsutb.; övrig gymn. utb. i hälso- och sjukvård", "tandsköterskeutbildning ", "social omsorgsutbildning, eftergymnasial nivå ", "övrig utb. inom hälso- och sjukvård, social omsorg, eftergymnasial", "transportutbildning, eftergymnasial nivå ", "övrig utbildning inom tjänsteområdet, eftergymnasial nivå ", "eftergymnasial utbildning, ospecificerad "
    kon_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "män", "kvinnor", "totalt"
    cont_klartext = "*",			 #  Finns: "Antagna"
    tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2018", "2019", "2020", "2021", "2022", "2023"
    long_format = TRUE,			# TRUE = konvertera innehållsvariablerna i datasetet till long-format 
    wide_om_en_contvar = TRUE,			# TRUE = om man vill behålla wide-format om det bara finns en innehållsvariabel, FALSE om man vill konvertera till long-format även om det bara finns en innehållsvariabel
    output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
    excel_filnamn = "antagna_yh.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
    returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
){
  
  # ====================================================================================================
  #
  # Funktion för att hämta data från SCB:s API med hjälp av pxweb-paketet
  # Automatgenererat av en funktion i R som skrivits av Peter Möller, Region Dalarna
  #
  # Skapad av: frkjon den 03 oktober 2024
  # Senast uppdaterad: 03 oktober 2024
  #
  # url till tabellens API: https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM9906/AM9906O/RegionInd19U5a
  #												https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM9906/AM9906B/RegionInd19U5aN
  #
  # ====================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
         tidyverse,
         writexl)
  
  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  # Url till SCB:s databas
  url_list <- c("https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM9906/AM9906O/RegionInd19U5a",
                "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM9906/AM9906B/RegionInd19U5aN")
  
  hamta_data <- function(url_uttag) {
    
    px_meta <- pxweb_get(url_uttag)
    
    varlist_koder <- pxvarlist(px_meta)$koder
    varlist_bada <- pxvarlist(px_meta)
    
    # Gör om från klartext till kod som databasen förstår
    utbildngrupp_vekt <- if (!all(is.na(utbildngrupp_klartext))) hamta_kod_med_klartext(px_meta, utbildngrupp_klartext, skickad_fran_variabel = "utbildngrupp") else NA
    kon_vekt <- if (!all(is.na(kon_klartext))) hamta_kod_med_klartext(px_meta, kon_klartext, skickad_fran_variabel = "kon") else NA
    
    cont_vekt <-  hamta_kod_med_klartext(px_meta, cont_klartext, "contentscode")
    if (length(cont_vekt) > 1) wide_om_en_contvar <- FALSE
    
    # Hantera tid-koder
    giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "tid")
    tid_vekt <- if (all(tid_koder != "*")) tid_koder %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar] %>% unique() else giltiga_ar
    
    # query-lista till pxweb-uttag
    varlista <- list(
      "Region" = region_vekt,
      "Utbildngrupp" = utbildngrupp_vekt,
      "Kon" = kon_vekt,
      "ContentsCode" = cont_vekt,
      "Tid" = tid_vekt)
    
    if (all(is.na(utbildngrupp_klartext))) varlista <- varlista[names(varlista) != "Utbildngrupp"]
    if (all(is.na(kon_klartext))) varlista <- varlista[names(varlista) != "Kon"]
    
    px_uttag <- pxweb_get(url = url_uttag, query = varlista)
    
    var_vektor <- c(regionkod = "Region", utbildngruppkod = "Utbildngrupp")
    var_vektor_klartext <- c("region", "utbildning")
    
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
  
}
hamta_antagna_yh_region_utbildngrupp_kon_tid_scb <- function(
    region_vekt = "20",			# Val av region. Finns: "00", "FA00", "FA01", "FA02", "FA03", "FA04", "FA05", "FA06", "FA07", "FA08", "FA09", "FA10", "FA11", "FA12", "FA13", "FA14", "FA15", "FA16", "FA17", "FA18", "FA19", "FA20", "FA21", "FA22", "FA23", "FA24", "FA25", "FA26", "FA27", "FA28", "FA29", "FA30", "FA31", "FA32", "FA33", "FA34", "FA35", "FA36", "FA37", "FA38", "FA39", "FA40", "FA41", "FA42", "FA43", "FA44", "FA45", "FA46", "FA47", "FA48", "FA49", "FA50", "FA51", "FA52", "FA53", "FA54", "FA55", "FA56", "FA57", "FA58", "FA59", "FA60", "01", "03", "04", "05", "06", "07", "08", "09", "FA99", "10", "12", "13", "14", "17", "18", "19", "20", "21", "22", "23", "24", "25", "999"
    utbildngrupp_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "samtliga utbildningsgrupper", "yrkeslärarutbildning ", "övrig utbildning inom pedagogik / lärarutbildning, eftergymnasial", "konstnärlig utbildning, eftergymnasial nivå ", "utbildning inom medieproduktion, eftergymnasial nivå ", "övrig utbildning inom humaniora och konst, eftergymnasial nivå ", "journalistik och medievetenskaplig utbildning, eftergymnasial nivå ", "övrig utb. i samhällsvetenskap, juridik, handel, admin., eftergymnasial", "medicinsk sekreterarutbildning", "Yh-utbildning i företagsekonomi, handel, administration", "datautbildning, eftergymnasial nivå ", "data- och it-utbildning, eftergymnasial (kortare än 3 år)", "teknikutbildning, yrkeshögskolan", "agronom- och hortonomutbildning ", "övrig utb. inom lant- och skogsbruk, djursjukvård, eftergymnasial", "barn- och fritidsutbildning, gymnasial nivå ", "vård- och omsorgsutb.; övrig gymn. utb. i hälso- och sjukvård", "tandsköterskeutbildning ", "social omsorgsutbildning, eftergymnasial nivå ", "övrig utb. inom hälso- och sjukvård, social omsorg, eftergymnasial", "transportutbildning, eftergymnasial nivå ", "övrig utbildning inom tjänsteområdet, eftergymnasial nivå ", "eftergymnasial utbildning, ospecificerad "
    kon_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "män", "kvinnor", "totalt"
    cont_klartext = "*",			 #  Finns: "Antagna"
    tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2018", "2019", "2020", "2021", "2022"
    output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
    excel_filnamn = "antagna_yh.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
    returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
){
  
  # ====================================================================================================
  #
  # Funktion för att hämta data från SCB:s API med hjälp av pxweb-paketet
  # Automatgenererat av en funktion i R som skrivits av Peter Möller, Region Dalarna
  #
  # Skapad av: frkjon den 03 oktober 2024
  # Senast uppdaterad: 03 oktober 2024
  #
  # url till tabellens API: https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM9906/AM9906O/RegionInd19U5a
  #
  # ====================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
         tidyverse,
         writexl)
  
  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  # Url till SCB:s databas
  url_uttag <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM9906/AM9906O/RegionInd19U5a"
  px_meta <- pxweb_get(url_uttag)
  
  varlist_koder <- pxvarlist(px_meta)$koder
  varlist_bada <- pxvarlist(px_meta)
  
  # Gör om från klartext till kod som databasen förstår
  utbildngrupp_vekt <- if (!all(is.na(utbildngrupp_klartext))) hamta_kod_med_klartext(px_meta, utbildngrupp_klartext, skickad_fran_variabel = "utbildngrupp") else NA
  kon_vekt <- if (!all(is.na(kon_klartext))) hamta_kod_med_klartext(px_meta, kon_klartext, skickad_fran_variabel = "kon") else NA
  
  cont_vekt <-  hamta_kod_med_klartext(px_meta, cont_klartext, "contentscode")
  if (length(cont_vekt) > 1) wide_om_en_contvar <- FALSE
  
  # Hantera tid-koder
  giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "tid")
  tid_vekt <- if (all(tid_koder != "*")) tid_koder %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar] %>% unique() else giltiga_ar
  
  # query-lista till pxweb-uttag
  varlista <- list(
    "Region" = region_vekt,
    "Utbildngrupp" = utbildngrupp_vekt,
    "Kon" = kon_vekt,
    "ContentsCode" = cont_vekt,
    "Tid" = tid_vekt)
  
  if (all(is.na(utbildngrupp_klartext))) varlista <- varlista[names(varlista) != "Utbildngrupp"]
  if (all(is.na(kon_klartext))) varlista <- varlista[names(varlista) != "Kon"]
  
  px_uttag <- pxweb_get(url = url_uttag, query = varlista)
  
  var_vektor <- c(regionkod = "Region", utbildngruppkod = "Utbildngrupp")
  var_vektor_klartext <- c("region", "utbildning")
  
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
  
  # Om användaren vill spara data till en Excel-fil
  if (!is.na(output_mapp) & !is.na(excel_filnamn)){
    write.xlsx(px_df, paste0(output_mapp, excel_filnamn))
  }
  
  # Returnera data som en dataframe om användern valt det
  if (returnera_df) return(px_df)
  
}
