hamta_mejeri_sjv <- function(
			variabel_klartext = "Kvantitet invägd mjölk, 1 000 ton",			 #  Finns: "Kvantitet invägd mjölk, 1 000 ton", "Genomsnittlig fetthalt på invägd mjölk, procent", "Genomsnittlig proteinhalt på invägd mjölk, procent", "Produktion av konsumtionsmjölk, 1 000 ton", "Produktion av syrade produkter, 1 000 ton", "Produktion av konsumtionsmjölk och syrade produkter totalt, 1 000 ton", "Produktion av konsumtionsmjölk och syrade produkter >2,0 %, 1 000 ton", "Produktion av konsumtionsmjölk och syrade produkter 1,0–2,0 %, 1 000 ton", "Produktion av konsumtionsmjölk och syrade produkter <1,0 %, 1 000 ton", "Produktion av grädde, 1 000 ton", "Produktion av ost, 1 000 ton", "Produktion av smör, 1 000 ton", "Produktion av mjölkpulver, 1 000 ton"
			år_klartext = "*",			 #  Finns: "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023"
			output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
			excel_filnamn = "jbv_mjolk.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
			returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
){

  # ====================================================================================================
  #
  # Funktion för att hämta data från SCB:s API med hjälp av pxweb-paketet
  # Automatgenererat av en funktion i R som skrivits av Peter Möller, Region Dalarna
  #
  # Skapad av: moepet den 06 mars 2024
  # Senast uppdaterad: 06 mars 2024
  #
  # url till tabellens API: https://statistik.sjv.se/PXWeb/api/v1/sv/Jordbruksverkets statistikdatabas/Animalieproduktion/Mejeriproduktion/JO0604A1.px
  #
  # ====================================================================================================

if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
    			tidyverse,
    			writexl)

  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

  # Url till SCB:s databas
  url_uttag <- "https://statistik.sjv.se/PXWeb/api/v1/sv/Jordbruksverkets statistikdatabas/Animalieproduktion/Mejeriproduktion/JO0604A1.px"
  px_meta <- pxweb_get(url_uttag)

  varlist_koder <- pxvarlist(px_meta)$koder
  varlist_bada <- pxvarlist(px_meta)

  # Gör om från klartext till kod som databasen förstår
  variabel_vekt <- hamta_kod_med_klartext(px_meta, variabel_klartext, skickad_fran_variabel = "variabel")
  år_vekt <- hamta_kod_med_klartext(px_meta, år_klartext, skickad_fran_variabel = "år")


# query-lista till pxweb-uttag
  varlista <- list(
  "Variabel" = variabel_vekt,
  "År" = år_vekt)



  px_uttag <- pxweb_get(url = url_uttag, query = varlista)

var_vektor <- NA
var_vektor_klartext <- NA
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
