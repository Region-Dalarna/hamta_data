hamta_befprogn_inr_utr_fodda_region_inrikesutrikes_kon_alder_tid_scb <- function(
			region_vekt = "20",			# Val av region.
			inrikesutrikes_klartext = c("inrikes födda", "utrikes födda"),			 #  NA = tas inte med i uttaget,  Finns: "inrikes födda", "utrikes födda", "inrikes och utrikes födda"
			kon_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "män", "kvinnor"
			alder_koder = "*",			 #  NA = tas inte med i uttaget,  Finns: "0 år", "1 år", "2 år", "3 år", "4 år", "5 år", "6 år", "7 år", "8 år", "9 år", "10 år", "11 år", "12 år", "13 år", "14 år", "15 år", "16 år", "17 år", "18 år", "19 år", "20 år", "21 år", "22 år", "23 år", "24 år", "25 år", "26 år", "27 år", "28 år", "29 år", "30 år", "31 år", "32 år", "33 år", "34 år", "35 år", "36 år", "37 år", "38 år", "39 år", "40 år", "41 år", "42 år", "43 år", "44 år", "45 år", "46 år", "47 år", "48 år", "49 år", "50 år", "51 år", "52 år", "53 år", "54 år", "55 år", "56 år", "57 år", "58 år", "59 år", "60 år", "61 år", "62 år", "63 år", "64 år", "65 år", "66 år", "67 år", "68 år", "69 år", "70 år", "71 år", "72 år", "73 år", "74 år", "75 år", "76 år", "77 år", "78 år", "79 år", "80 år", "81 år", "82 år", "83 år", "84 år", "85 år", "86 år", "87 år", "88 år", "89 år", "90 år", "91 år", "92 år", "93 år", "94 år", "95 år", "96 år", "97 år", "98 år", "99 år", "100+ år"
			cont_klartext = "*",			 #  Finns: "Folkmängd"
			tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2022", "2023", "2024", "2025", "2026", "2027", "2028", "2029", "2030", "2031", "2032", "2033", "2034", "2035", "2036", "2037", "2038", "2039", "2040", "2041", "2042", "2043", "2044", "2045", "2046", "2047", "2048", "2049", "2050", "2051", "2052", "2053", "2054", "2055", "2056", "2057", "2058", "2059", "2060", "2061", "2062", "2063", "2064", "2065", "2066", "2067", "2068", "2069", "2070"
			output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
			excel_filnamn = "befprogn_inr_utr_fodda.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
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
  # url till tabellens API: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__BE__BE0401__BE0401A/BefProgRegFakN/
  #
  # ====================================================================================================

if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
    			tidyverse,
    			writexl)

  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

  # Url till SCB:s databas
  url_uttag <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0401/BE0401A/BefProgRegFakN"
  px_meta <- pxweb_get(url_uttag)

  varlist_koder <- pxvarlist(px_meta)$koder
  varlist_bada <- pxvarlist(px_meta)

  # Gör om från klartext till kod som databasen förstår
  inrikesutrikes_vekt <- if (!all(is.na(inrikesutrikes_klartext))) hamta_kod_med_klartext(px_meta, inrikesutrikes_klartext, skickad_fran_variabel = "inrikesutrikes") else NA
  kon_vekt <- if (!all(is.na(kon_klartext))) hamta_kod_med_klartext(px_meta, kon_klartext, skickad_fran_variabel = "kon") else NA

  alder_vekt <- if (all(!is.na(alder_koder))) alder_koder %>% as.character() %>% ifelse(. == "100", "-100+", .) %>% ifelse(. == "tot", "totalt ålder", .) else NA
  cont_vekt <-  hamta_kod_med_klartext(px_meta, cont_klartext, "contentscode")
  if (length(cont_vekt) > 1) wide_om_en_contvar <- FALSE

  # Hantera tid-koder
  giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "tid")
  if (all(tid_koder != "*")) tid_koder <- tid_koder %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar] %>% unique()

# query-lista till pxweb-uttag
  varlista <- list(
  "Region" = region_vekt,
  "InrikesUtrikes" = inrikesutrikes_vekt,
  "Kon" = kon_vekt,
  "Alder" = alder_vekt,
  "ContentsCode" = cont_vekt,
  "Tid" = tid_koder)

  if (all(is.na(inrikesutrikes_klartext))) varlista <- varlista[names(varlista) != "InrikesUtrikes"]
  if (all(is.na(kon_klartext))) varlista <- varlista[names(varlista) != "Kon"]
  if (all(is.na(alder_koder))) varlista <- varlista[names(varlista) != "Alder"]

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
