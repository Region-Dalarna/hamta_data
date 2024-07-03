hamta_folkhogskola_kon_kurstyp_nationellbakgrund_regionutb_aldersgrupp_tid_scb <- function(
    kon_klartext = "*",			 #  Finns: "totalt", "kvinnor", "män"
    kurstyp_klartext = "*",			 #  Finns: "allmän kurs, 15 dagar eller längre", "särskild kurs, 15 dagar eller längre", "kort kurs, mindre än 15 dagar"
    nationellbakgrund_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "Totalt", "Inrikes född", "Utrikes född", "Uppgift saknas", "Svensk bakgrund", "Utländsk bakgrund"
    regionutb_vekt = "20",			# Val av region. Finns: "00", "01", "03", "04", "05", "06", "07", "08", "09", "10", "12", "13", "14", "17", "18", "19", "20", "21", "22", "23", "24", "25", "400", "402", "404", "406", "408", "410", "412", "414", "416", "418", "420", "422", "424", "426", "428", "430", "432", "434", "436", "438", "440", "442", "444", "446", "448", "450", "452", "454", "456", "458", "460", "462", "464", "466", "468", "470", "472", "474", "476", "478", "480", "482", "484", "486", "488", "490", "492", "494", "496", "498", "500", "502", "504", "506", "508", "510", "512", "514", "516", "518", "520", "522", "524", "526", "528", "530", "532", "534", "536", "538", "540", "542", "544", "546", "548", "550", "552", "554", "556", "558", "560", "562", "564", "566", "568", "570", "572", "574", "576", "577", "578", "580", "582", "584", "586", "588", "590", "592", "594", "596", "598", "600", "602", "604", "606", "608", "610", "612", "614", "616", "618", "620", "622", "624", "626", "628", "630", "632", "634", "636", "638", "640", "642", "644", "645", "646", "648", "650", "652", "654", "656", "657", "658", "660", "662", "664", "666", "668", "670", "672", "674", "676", "678", "680", "682", "684", "686", "688", "690", "692", "694", "696", "698", "700", "702", "704", "706", "708", "710", "SKLA1", "SKLA2", "SKLB3", "SKLB4", "SKLB5", "SKLC6", "SKLC7", "SKLC8", "SKLC9"
    aldersgrupp_klartext = "*",			 #  Finns: "-17 år", "18-19 år", "18-24 år", "20-24 år", "25-34 år", "25-44 år", "35-44 år", "45-54 år", "45-64 år", "55-64 år", "65+ år", "totalt ålder", "totalt"
    cont_klartext = "*",			 #  Finns: "Antal deltagare i folkhögskolekurser"
    tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2017", "2018", "2019", "2020", "2021", "2022", "2023"
    output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
    excel_filnamn = ".xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
    returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
){
  
  # ====================================================================================================
  #
  # Funktion för att hämta data från SCB:s API med hjälp av pxweb-paketet
  # Automatgenererat av en funktion i R som skrivits av Peter Möller, Region Dalarna
  #
  # Skapad av: frkjon den 03 juli 2024
  # Senast uppdaterad: 03 juli 2024
  #
  # url till tabellens API: https://api.scb.se/OV0104/v1/doris/sv/ssd/START/UF/UF0601/UF0601A/UF0601T01b
  #
  # ====================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
         tidyverse,
         writexl)
  
  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  # Url till SCB:s databas
  url_uttag <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/UF/UF0601/UF0601A/UF0601T01b"
  px_meta <- pxweb_get(url_uttag)
  
  varlist_koder <- pxvarlist(px_meta)$koder
  varlist_bada <- pxvarlist(px_meta)
  
  # Gör om från klartext till kod som databasen förstår
  kon_vekt <- hamta_kod_med_klartext(px_meta, kon_klartext, skickad_fran_variabel = "kon")
  kurstyp_vekt <- hamta_kod_med_klartext(px_meta, kurstyp_klartext, skickad_fran_variabel = "kurstyp")
  nationellbakgrund_vekt <- if (!all(is.na(nationellbakgrund_klartext))) hamta_kod_med_klartext(px_meta, nationellbakgrund_klartext, skickad_fran_variabel = "nationellbakgrund") else NA
  aldersgrupp_vekt <- hamta_kod_med_klartext(px_meta, aldersgrupp_klartext, skickad_fran_variabel = "aldersgrupp")
  
  cont_vekt <-  hamta_kod_med_klartext(px_meta, cont_klartext, "contentscode")
  if (length(cont_vekt) > 1) wide_om_en_contvar <- FALSE
  
  # Hantera tid-koder
  giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "tid")
  if (all(tid_koder != "*")) tid_koder <- tid_koder %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar] %>% unique()
  
  # query-lista till pxweb-uttag
  varlista <- list(
    "Kon" = kon_vekt,
    "KursTyp" = kurstyp_vekt,
    "NationellBakgrund" = nationellbakgrund_vekt,
    "RegionUtb" = regionutb_vekt,
    "Aldersgrupp" = aldersgrupp_vekt,
    "ContentsCode" = cont_vekt,
    "Tid" = tid_koder)
  
  if (all(is.na(nationellbakgrund_klartext))) varlista <- varlista[names(varlista) != "NationellBakgrund"]
  
  px_uttag <- pxweb_get(url = url_uttag, query = varlista)
  
  var_vektor <- c(regionutbkod = "RegionUtb")
  var_vektor_klartext <- "region, där utbildningen bedrivs"
  
  px_df <- as.data.frame(px_uttag)
  if (!all(is.na(var_vektor))) {
    # om man vill ha med koder också för variabler utöver klartext så läggs de på här (om det finns värden i var_vektor)
    px_df <- px_df %>%
      cbind(as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>%
              select(any_of(var_vektor)))
    
    # kolumnerna med koder läggs framför motsvarande kolumner med klartext
    for (varflytt_index in 1:length(var_vektor)) {
      px_df <- px_df %>%
        relocate(all_of(names(var_vektor)[varflytt_index]), .before = all_of(var_vektor_klartext[varflytt_index]))
    }
  }
  
  # Om användaren vill spara data till en Excel-fil
  if (!is.na(output_mapp) & !is.na(excel_filnamn)){
    write.xlsx(px_df, paste0(output_mapp, excel_filnamn))
  }
  
  # Returnera data som en dataframe om användern valt det
  if (returnera_df) return(px_df)
  
}
