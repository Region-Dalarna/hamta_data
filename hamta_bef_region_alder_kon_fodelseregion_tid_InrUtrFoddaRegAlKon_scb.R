hamta_bef_region_alder_kon_fodelseregion_tid_scb <- function(
			region_vekt = "20",			# Val av region.
			alder_koder = "*",			 #  NA = tas inte med i uttaget,  Finns: "0 år", "1 år", "2 år", "3 år", "4 år", "5 år", "6 år", "7 år", "8 år", "9 år", "10 år", "11 år", "12 år", "13 år", "14 år", "15 år", "16 år", "17 år", "18 år", "19 år", "20 år", "21 år", "22 år", "23 år", "24 år", "25 år", "26 år", "27 år", "28 år", "29 år", "30 år", "31 år", "32 år", "33 år", "34 år", "35 år", "36 år", "37 år", "38 år", "39 år", "40 år", "41 år", "42 år", "43 år", "44 år", "45 år", "46 år", "47 år", "48 år", "49 år", "50 år", "51 år", "52 år", "53 år", "54 år", "55 år", "56 år", "57 år", "58 år", "59 år", "60 år", "61 år", "62 år", "63 år", "64 år", "65 år", "66 år", "67 år", "68 år", "69 år", "70 år", "71 år", "72 år", "73 år", "74 år", "75 år", "76 år", "77 år", "78 år", "79 år", "80 år", "81 år", "82 år", "83 år", "84 år", "85 år", "86 år", "87 år", "88 år", "89 år", "90 år", "91 år", "92 år", "93 år", "94 år", "95 år", "96 år", "97 år", "98 år", "99 år", "100+ år"
			kon_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "män", "kvinnor"
			fodelseregion_klartext = "*",			#  NA = tas inte med i uttaget,  Finns: "Född i Sverige", "Utrikes född"
			cont_klartext = "*",			 #  Finns: "Folkmängd"
			tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023"
			output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
			excel_filnamn = "bef.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
			returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
){

  # ====================================================================================================
  #
  # Funktion för att hämta data från SCB:s API med hjälp av pxweb-paketet
  # Automatgenererat av en funktion i R som skrivits av Peter Möller, Region Dalarna
  #
  # Skapad av: moepet den 20 mars 2024
  # Senast uppdaterad: 20 mars 2024
  #
  # url till tabellens API: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__BE__BE0101__BE0101E/InrUtrFoddaRegAlKon/
  #
  # ====================================================================================================

if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
    			tidyverse,
    			writexl)

  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

  # Url till SCB:s databas
  url_uttag <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101E/InrUtrFoddaRegAlKon"
  px_meta <- pxweb_get(url_uttag)

  varlist_koder <- pxvarlist(px_meta)$koder
  varlist_bada <- pxvarlist(px_meta)

  # Gör om från klartext till kod som databasen förstår
  kon_vekt <- if (!all(is.na(kon_klartext))) hamta_kod_med_klartext(px_meta, kon_klartext, skickad_fran_variabel = "kon") else NA
  fodelseregion_vekt <- if (!all(is.na(fodelseregion_klartext))) hamta_kod_med_klartext(px_meta, fodelseregion_klartext, skickad_fran_variabel = "fodelseregion") else NA

  alder_vekt <- if (all(!is.na(alder_koder))) alder_koder %>% as.character() %>% ifelse(. == "100", "-100+", .) %>% ifelse(. == "tot", "totalt ålder", .) else NA
  cont_vekt <-  hamta_kod_med_klartext(px_meta, cont_klartext, "contentscode")
  if (length(cont_vekt) > 1) wide_om_en_contvar <- FALSE

  # Hantera tid-koder
  giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "tid")
  if (all(tid_koder != "*")) tid_koder <- tid_koder %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar] %>% unique()

# query-lista till pxweb-uttag
  varlista <- list(
  "Region" = region_vekt,
  "Alder" = alder_vekt,
  "Kon" = kon_vekt,
  "Fodelseregion" = fodelseregion_vekt,
  "ContentsCode" = cont_vekt,
  "Tid" = tid_koder)

  if (all(is.na(alder_koder))) varlista <- varlista[names(varlista) != "Alder"]
  if (all(is.na(kon_klartext))) varlista <- varlista[names(varlista) != "Kon"]
  if (all(is.na(fodelseregion_klartext))) varlista <- varlista[names(varlista) != "Fodelseregion"]

  px_uttag <- pxweb_get(url = url_uttag, query = varlista)

var_vektor <- c(regionkod = "Region")
var_vektor_klartext <- "region"
  px_df <- as.data.frame(px_uttag)
  if (!is.na(var_vektor)) {      px_df <- px_df %>%
            cbind(as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>%
            select(any_of(var_vektor)))

      px_df <- map2(names(var_vektor), var_vektor_klartext, ~ px_df %>% relocate(all_of(.x), .before = all_of(.y))) %>% list_rbind()
  }


  # Om användaren vill spara data till en Excel-fil
  if (!is.na(output_mapp) & !is.na(excel_filnamn)){
    write.xlsx(px_df, paste0(output_mapp, excel_filnamn))
  }

  # Returnera data som en dataframe om användern valt det
  if (returnera_df) return(px_df)

}
