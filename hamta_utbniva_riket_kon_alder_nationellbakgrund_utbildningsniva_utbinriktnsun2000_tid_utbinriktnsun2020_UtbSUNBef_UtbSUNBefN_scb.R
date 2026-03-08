hamta_utbniva_riket_kon_alder_nationellbakgrund_utbildningsniva_utbinriktnsun2000_tid_utbinriktnsun2020_scb <- function(
			kon_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "män", "kvinnor"
			alder_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "16-24 år", "25-34 år", "35-44 år", "45-54 år", "55-64 år", "65-74 år"
			nationellbakgrund_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "född i Sverige", "född utomlands"
			utbildningsniva_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "förgymnasial utbildning kortare än 9 år", "förgymnasial utbildning, 9 (10) år", "gymnasial utbildning, högst 2 år", "gymnasial utbildning, 3 år", "eftergymnasial utbildning, mindre än 3 år", "eftergymnasial utbildning, 3 år eller mer", "forskarutbildning", "uppgift om utbildningsnivå saknas"
			utbinriktning_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "allmän utbildning", "pedagogik och lärarutbildning", "humaniora och konst", "samhällsvetenskap, juridik, handel, administration", "naturvetenskap, matematik och data", "teknik och tillverkning", "lant- och skogsbruk samt djursjukvård", "hälso- och sjukvård samt social omsorg", "tjänster", "okänd utbildningsinriktning"
			cont_klartext = "*",			 #  Finns: "Befolkning"
			tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024"
			long_format = TRUE,			# TRUE = konvertera innehållsvariablerna i datasetet till long-format 
			wide_om_en_contvar = TRUE,			# TRUE = om man vill behålla wide-format om det bara finns en innehållsvariabel, FALSE om man vill konvertera till long-format även om det bara finns en innehållsvariabel
			output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
			excel_filnamn = "utbniva_riket.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
			returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
){

  # ====================================================================================================
  #
  # Funktion för att hämta data från SCB:s API med hjälp av pxweb-paketet
  # Automatgenererat av en funktion i R som skrivits av Peter Möller, Region Dalarna
  #
  # Skapad av: moepet den 07 mars 2026
  # Senast uppdaterad: 07 mars 2026
  #
  # url till tabellens API: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__UF__UF0506__UF0506B/UtbSUNBef/
  #												https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__UF__UF0506__UF0506B/UtbSUNBefN/
  #
  # ====================================================================================================

  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
    			tidyverse,
    			writexl)

  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

  # Url till databas
  url_list <- c("https://api.scb.se/OV0104/v1/doris/sv/ssd/START/UF/UF0506/UF0506B/UtbSUNBef",
						"https://api.scb.se/OV0104/v1/doris/sv/ssd/START/UF/UF0506/UF0506B/UtbSUNBefN")

 hamta_data <- function(url_uttag) {

  px_meta <- pxweb_get(url_uttag)

  varlist_koder <- pxvarlist(px_meta)$koder
  varlist_bada <- pxvarlist(px_meta)

  utbinr_kol <- varlist_koder[str_detect(tolower(varlist_koder), "utbinriktnsun")]
  
  # Gör om från klartext till kod som databasen förstår
  kon_vekt <- if (!all(is.na(kon_klartext))) hamta_kod_med_klartext(px_meta, kon_klartext, skickad_fran_variabel = "kon") else NA
  alder_vekt <- if (!all(is.na(alder_klartext))) hamta_kod_med_klartext(px_meta, alder_klartext, skickad_fran_variabel = "alder") else NA
  nationellbakgrund_vekt <- if (!all(is.na(nationellbakgrund_klartext))) hamta_kod_med_klartext(px_meta, nationellbakgrund_klartext, skickad_fran_variabel = "nationellbakgrund") else NA
  utbildningsniva_vekt <- if (!all(is.na(utbildningsniva_klartext))) hamta_kod_med_klartext(px_meta, utbildningsniva_klartext, skickad_fran_variabel = "utbildningsniva") else NA
  utbinriktning_vekt <- if (!all(is.na(utbinriktning_klartext))) hamta_kod_med_klartext(px_meta, utbinriktning_klartext, skickad_fran_variabel = utbinr_kol) else NA
  
  alder_vekt <- if (!all(is.na(alder_klartext))) hamta_kod_med_klartext(px_meta, alder_klartext, skickad_fran_variabel = "alder") else NA
  cont_vekt <-  hamta_kod_med_klartext(px_meta, cont_klartext, "contentscode")
  if (length(cont_vekt) > 1) wide_om_en_contvar <- FALSE

  # Hantera tid-koder
  giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "tid")
  tid_vekt <- if (all(tid_koder != "*")) tid_koder %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar] %>% unique() else giltiga_ar

  if (length(tid_vekt) > 0) {
    # query-lista till pxweb-uttag
    varlista <- c(
      list(
        "Kon" = kon_vekt,
        "Alder" = alder_vekt,
        "NationellBakgrund" = nationellbakgrund_vekt,
        "UtbildningsNiva" = utbildningsniva_vekt,
        "ContentsCode" = cont_vekt,
        "Tid" = tid_vekt
      ),
      setNames(list(utbinriktning_vekt), utbinr_kol)
    )
  
    if (all(is.na(kon_klartext))) varlista <- varlista[names(varlista) != "Kon"]
    if (all(is.na(alder_klartext))) varlista <- varlista[names(varlista) != "Alder"]
    if (all(is.na(nationellbakgrund_klartext))) varlista <- varlista[names(varlista) != "NationellBakgrund"]
    if (all(is.na(utbildningsniva_klartext))) varlista <- varlista[names(varlista) != "UtbildningsNiva"]
    if (all(is.na(utbinriktning_klartext))) varlista <- varlista[names(varlista) != utbinr_kol]
  
    # Hämta data med varlista
    px_uttag <- pxweb_get(url = url_uttag, query = varlista)
  
    var_vektor <- c(utbniva_kod = "UtbildningsNiva")
    var_vektor_klartext <- "UtbildningsNiva"
  
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
   } # slut test om det finns något giltigt år i denna tabell av de användaren valt
  } # slut hämta data-funktion 

  px_alla <- map(url_list, ~ hamta_data(.x)) %>% list_rbind()

  # Om användaren vill spara data till en Excel-fil
  if (!is.na(output_mapp) & !is.na(excel_filnamn)){
    write.xlsx(px_alla, paste0(output_mapp, excel_filnamn))
  }

  # Returnera data som en dataframe om användern valt det
  if (returnera_df) return(px_alla)
} # slut hämta data-funktion
