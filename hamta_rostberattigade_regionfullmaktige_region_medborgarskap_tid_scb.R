hamta_rostberattigade_regionfullmaktige_region_medborgarskap_tid_scb <- function(
			region_vekt = "*",			# Val av region.
			medborgarskap_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "svenska medborgare", "utländska medborgare"
			cont_klartext = "*",			 #  Finns: "Regionfullmäktigval - röstberättigade"
			tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "1973", "1976", "1979", "1982", "1985", "1988", "1991", "1994", "1998", "2002", "2006", "2010", "2014", "2018", "2022"
			output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
			excel_filnamn = "rostberattigade_regionfullmaktige.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
			returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
){

  # ====================================================================================================
  #
  # Funktion för att hämta data från SCB:s API med hjälp av pxweb-paketet
  # Automatgenererat av en funktion i R som skrivits av Peter Möller, Region Dalarna
  #
  # Skapad av: frkjon den 06 mars 2024
  # Senast uppdaterad: 06 mars 2024
  #
  # url till tabellens API: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__ME__ME0104__ME0104B/Landstingsmed/
  #
  # ====================================================================================================

if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
    			tidyverse,
    			writexl)

  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

  # Url till SCB:s databas
  url_uttag <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/ME/ME0104/ME0104B/Landstingsmed"
  px_meta <- pxweb_get(url_uttag)

  varlist_koder <- pxvarlist(px_meta)$koder
  varlist_bada <- pxvarlist(px_meta)

  # Gör om från klartext till kod som databasen förstår
  medborgarskap_vekt <- if (!all(is.na(medborgarskap_klartext))) hamta_kod_med_klartext(px_meta, medborgarskap_klartext, skickad_fran_variabel = "medborgarskap") else NA


  cont_vekt <-  hamta_kod_med_klartext(px_meta, cont_klartext, "contentscode")
  if (length(cont_vekt) > 1) wide_om_en_contvar <- FALSE

  # Hantera tid-koder
  giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "tid")
  if (all(tid_koder != "*")) tid_koder <- tid_koder %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar] %>% unique()

# query-lista till pxweb-uttag
  varlista <- list(
  "Region" = region_vekt,
  "Medborgarskap" = medborgarskap_vekt,
  "ContentsCode" = cont_vekt,
  "Tid" = tid_koder)

  if (all(is.na(medborgarskap_klartext))) varlista <- varlista[names(varlista) != "Medborgarskap"]

  px_uttag <- pxweb_get(url = url_uttag, query = varlista)

var_vektor <- c(regionkoder = "Region")
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
