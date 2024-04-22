hamta_medellivslangd_lan_utbildningsniva_kon_alder_tid_scb <- function(
			region_vekt = "20",			# Val av region.
			utbildningsniva_klartext = c("förgymnasial utbildning", "gymnasial utbildning", "eftergymnasial utbildning"),			 #  Finns: "samtliga utbildningsnivåer", "förgymnasial utbildning", "gymnasial utbildning", "eftergymnasial utbildning", "uppgift om utbildningsnivå saknas"
			kon_klartext = c("män", "kvinnor"),			 #  Finns: "samtliga män och kvinnor", "män", "kvinnor"
			alder_klartext = "30 år",			 #  Finns: "30 år", "31 år", "32 år", "33 år", "34 år", "35 år", "36 år", "37 år", "38 år", "39 år", "40 år", "41 år", "42 år", "43 år", "44 år", "45 år", "46 år", "47 år", "48 år", "49 år", "50 år", "51 år", "52 år", "53 år", "54 år", "55 år", "56 år", "57 år", "58 år", "59 år", "60 år", "61 år", "62 år", "63 år", "64 år", "65 år", "66 år", "67 år", "68 år", "69 år", "70 år", "71 år", "72 år", "73 år", "74 år", "75 år", "76 år", "77 år", "78 år", "79 år", "80 år", "81 år", "82 år", "83 år", "84 år", "85 år", "86 år", "87 år", "88 år", "89 år", "90 år", "91 år", "92 år", "93 år", "94 år", "95+ år"
			cont_klartext = "Antal återstående år",			 #  Finns: "Medelfolkmängd", "Antal döda", "Antal döda efter födelsedagen", "Dödstal per 1 000", "Dödsrisk per 1 000", "Kvarlevande av 100 000 i 30 års ålder", "Döda i livslängdstabell", "Tid i ålder", "Tid i ålder och däröver", "Antal återstående år"
			tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2012-2016", "2013-2017", "2014-2018", "2015-2019", "2016-2020", "2017-2021", "2018-2022"
			long_format = TRUE,			# TRUE = konvertera innehållsvariablerna i datasetet till long-format 
			wide_om_en_contvar = TRUE,			# TRUE = om man vill behålla wide-format om det bara finns en innehållsvariabel, FALSE om man vill konvertera till long-format även om det bara finns en innehållsvariabel
			output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
			excel_filnamn = "medellivslangd.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
			returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
){

  # ====================================================================================================
  #
  # Funktion för att hämta data från SCB:s API med hjälp av pxweb-paketet
  # Automatgenererat av en funktion i R som skrivits av Peter Möller, Region Dalarna
  #
  # Skapad av: moepet den 22 april 2024
  # Senast uppdaterad: 22 april 2024
  #
  # url till tabellens API: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__BE__BE0701/LivslUtbLan/
  #
  # ====================================================================================================

  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
    			tidyverse,
    			writexl)

  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

  # Url till SCB:s databas
  url_uttag <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0701/LivslUtbLan"
  px_meta <- pxweb_get(url_uttag)

  varlist_koder <- pxvarlist(px_meta)$koder
  varlist_bada <- pxvarlist(px_meta)

  # Gör om från klartext till kod som databasen förstår
  utbildningsniva_vekt <- hamta_kod_med_klartext(px_meta, utbildningsniva_klartext, skickad_fran_variabel = "utbildningsniva")
  kon_vekt <- hamta_kod_med_klartext(px_meta, kon_klartext, skickad_fran_variabel = "kon")

  alder_vekt <- if (!all(is.na(alder_klartext))) hamta_kod_med_klartext(px_meta, alder_klartext, skickad_fran_variabel = "alder") else NA
  cont_vekt <-  hamta_kod_med_klartext(px_meta, cont_klartext, "contentscode")
  if (length(cont_vekt) > 1) wide_om_en_contvar <- FALSE

  # Hantera tid-koder
  giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "tid")
  if (all(tid_koder != "*")) tid_koder <- tid_koder %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar] %>% unique()

  # query-lista till pxweb-uttag
  varlista <- list(
  "Region" = region_vekt,
  "UtbildningsNiva" = utbildningsniva_vekt,
  "Kon" = kon_vekt,
  "Alder" = alder_vekt,
  "ContentsCode" = cont_vekt,
  "Tid" = tid_koder)



  px_uttag <- pxweb_get(url = url_uttag, query = varlista)

  var_vektor <- c(regionkod = "Region")
  var_vektor_klartext <- "region"

  px_df <- as.data.frame(px_uttag)
  if (!is.na(var_vektor)) {      px_df <- px_df %>%
            cbind(as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>%
            select(any_of(var_vektor)))

      px_df <- map2(names(var_vektor), var_vektor_klartext, ~ px_df %>% relocate(all_of(.x), .before = all_of(.y))) %>% list_rbind()
  }

  # man kan välja bort long-format, då låter vi kolumnerna vara wide om det finns fler innehållsvariabler, annars
  # pivoterar vi om till long-format, dock ej om det bara finns en innehållsvariabel
  if (long_format & !wide_om_en_contvar) px_df <- px_df %>% konvertera_till_long_for_contentscode_variabler(url_uttag)

  # Om användaren vill spara data till en Excel-fil
  if (!is.na(output_mapp) & !is.na(excel_filnamn)){
    write.xlsx(px_df, paste0(output_mapp, excel_filnamn))
  }

  # Returnera data som en dataframe om användern valt det
  if (returnera_df) return(px_df)

}
