hamta_utbildningsniva_kon_alder_nationellbakgrund_utbinriktnsun2020_tid_scb <- function(
    kon_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "män", "kvinnor"
    alder_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "16-24 år", "25-34 år", "35-44 år", "45-54 år", "55-64 år", "65-74 år"
    nationellbakgrund_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "född i Sverige", "född utomlands"
    utbildningsniva_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "förgymnasial utbildning kortare än 9 år", "förgymnasial utbildning, 9 (10) år", "gymnasial utbildning, högst 2 år", "gymnasial utbildning, 3 år", "eftergymnasial utbildning, mindre än 3 år", "eftergymnasial utbildning, 3 år eller mer", "forskarutbildning", "uppgift om utbildningsnivå saknas"
    utbinriktnsun2020_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "Allmän utbildning", "Pedagogik och lärarutbildning", "Humaniora och konst", "Samhällsvetenskap, juridik, handel, administration ", "Naturvetenskap, matematik och informations- och kommunikationsteknik (IKT)", "Teknik och tillverkning", "Lant- och skogsbruk samt djursjukvård", "Hälso- och sjukvård samt social omsorg", "Tjänster", "Okänd "
    cont_klartext = "*",			 #  Finns: "Befolkning"
    tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2019", "2020", "2021", "2022", "2023"
    output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
    excel_filnamn = ".xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
    returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
){
  
  # ====================================================================================================
  #
  # Funktion för att hämta data från SCB:s API med hjälp av pxweb-paketet
  # Automatgenererat av en funktion i R som skrivits av Peter Möller, Region Dalarna
  #
  # Skapad av: frkjon den 10 juni 2024
  # Senast uppdaterad: 10 juni 2024
  #
  # url till tabellens API: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__UF__UF0506__UF0506B/UtbSUNBefN/
  #
  # ====================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
         tidyverse,
         writexl)
  
  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  # Url till SCB:s databas
  url_uttag <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/UF/UF0506/UF0506B/UtbSUNBefN"
  px_meta <- pxweb_get(url_uttag)
  
  varlist_koder <- pxvarlist(px_meta)$koder
  varlist_bada <- pxvarlist(px_meta)
  
  # Gör om från klartext till kod som databasen förstår
  kon_vekt <- if (!all(is.na(kon_klartext))) hamta_kod_med_klartext(px_meta, kon_klartext, skickad_fran_variabel = "kon") else NA
  alder_vekt <- if (!all(is.na(alder_klartext))) hamta_kod_med_klartext(px_meta, alder_klartext, skickad_fran_variabel = "alder") else NA
  nationellbakgrund_vekt <- if (!all(is.na(nationellbakgrund_klartext))) hamta_kod_med_klartext(px_meta, nationellbakgrund_klartext, skickad_fran_variabel = "nationellbakgrund") else NA
  utbildningsniva_vekt <- if (!all(is.na(utbildningsniva_klartext))) hamta_kod_med_klartext(px_meta, utbildningsniva_klartext, skickad_fran_variabel = "utbildningsniva") else NA
  utbinriktnsun2020_vekt <- if (!all(is.na(utbinriktnsun2020_klartext))) hamta_kod_med_klartext(px_meta, utbinriktnsun2020_klartext, skickad_fran_variabel = "utbinriktnsun2020") else NA
  
  alder_vekt <- if (!all(is.na(alder_klartext))) hamta_kod_med_klartext(px_meta, alder_klartext, skickad_fran_variabel = "alder") else NA
  cont_vekt <-  hamta_kod_med_klartext(px_meta, cont_klartext, "contentscode")
  if (length(cont_vekt) > 1) wide_om_en_contvar <- FALSE
  
  # Hantera tid-koder
  giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "tid")
  if (all(tid_koder != "*")) tid_koder <- tid_koder %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar] %>% unique()
  
  # query-lista till pxweb-uttag
  varlista <- list(
    "Kon" = kon_vekt,
    "Alder" = alder_vekt,
    "NationellBakgrund" = nationellbakgrund_vekt,
    "UtbildningsNiva" = utbildningsniva_vekt,
    "UtbinriktnSUN2020" = utbinriktnsun2020_vekt,
    "ContentsCode" = cont_vekt,
    "Tid" = tid_koder)
  
  if (all(is.na(kon_klartext))) varlista <- varlista[names(varlista) != "Kon"]
  if (all(is.na(alder_klartext))) varlista <- varlista[names(varlista) != "Alder"]
  if (all(is.na(nationellbakgrund_klartext))) varlista <- varlista[names(varlista) != "NationellBakgrund"]
  if (all(is.na(utbildningsniva_klartext))) varlista <- varlista[names(varlista) != "UtbildningsNiva"]
  if (all(is.na(utbinriktnsun2020_klartext))) varlista <- varlista[names(varlista) != "UtbinriktnSUN2020"]
  
  px_uttag <- pxweb_get(url = url_uttag, query = varlista)
  
  var_vektor <- NA
  var_vektor_klartext <- NA
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
