hamta_bygglov_region_hustyp_tid_scb <- function(
			region_vekt = "20",			# Val av region.
			hustyp_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "småhus", "flerbostadshus exkl. specialbostäder", "specialbostäder", "fritidshus"
			cont_klartext = "*",			 #  Finns: "Bygglov för nybyggnad, lägenheter"
			tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "1996K1", "1996K2", "1996K3", "1996K4", "1997K1", "1997K2", "1997K3", "1997K4", "1998K1", "1998K2", "1998K3", "1998K4", "1999K1", "1999K2", "1999K3", "1999K4", "2000K1", "2000K2", "2000K3", "2000K4", "2001K1", "2001K2", "2001K3", "2001K4", "2002K1", "2002K2", "2002K3", "2002K4", "2003K1", "2003K2", "2003K3", "2003K4", "2004K1", "2004K2", "2004K3", "2004K4", "2005K1", "2005K2", "2005K3", "2005K4", "2006K1", "2006K2", "2006K3", "2006K4", "2007K1", "2007K2", "2007K3", "2007K4", "2008K1", "2008K2", "2008K3", "2008K4", "2009K1", "2009K2", "2009K3", "2009K4", "2010K1", "2010K2", "2010K3", "2010K4", "2011K1", "2011K2", "2011K3", "2011K4", "2012K1", "2012K2", "2012K3", "2012K4", "2013K1", "2013K2", "2013K3", "2013K4", "2014K1", "2014K2", "2014K3", "2014K4", "2015K1", "2015K2", "2015K3", "2015K4", "2016K1", "2016K2", "2016K3", "2016K4", "2017K1", "2017K2", "2017K3", "2017K4", "2018K1", "2018K2", "2018K3", "2018K4", "2019K1", "2019K2", "2019K3", "2019K4", "2020K1", "2020K2", "2020K3", "2020K4", "2021K1", "2021K2", "2021K3", "2021K4", "2022K1", "2022K2", "2022K3", "2022K4", "2023K1", "2023K2", "2023K3", "2023K4"
			output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
			excel_filnamn = "bygglov.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
			returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
){

  # ====================================================================================================
  #
  # Funktion för att hämta data från SCB:s API med hjälp av pxweb-paketet
  # Automatgenererat av en funktion i R som skrivits av Peter Möller, Region Dalarna
  #
  # Skapad av: frkjon den 15 april 2024
  # Senast uppdaterad: 15 april 2024
  #
  # url till tabellens API: https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BO/BO0101/BO0101G/LghHustypKv
  #
  # ====================================================================================================

  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
    			tidyverse,
    			writexl)

  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

  # Url till SCB:s databas
  url_uttag <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BO/BO0101/BO0101G/LghHustypKv"
  px_meta <- pxweb_get(url_uttag)

  varlist_koder <- pxvarlist(px_meta)$koder
  varlist_bada <- pxvarlist(px_meta)

  # Gör om från klartext till kod som databasen förstår
  hustyp_vekt <- if (!all(is.na(hustyp_klartext))) hamta_kod_med_klartext(px_meta, hustyp_klartext, skickad_fran_variabel = "hustyp") else NA

  cont_vekt <-  hamta_kod_med_klartext(px_meta, cont_klartext, "contentscode")
  if (length(cont_vekt) > 1) wide_om_en_contvar <- FALSE

  # Hantera tid-koder
  giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "tid")
  if (all(tid_koder != "*")) tid_koder <- tid_koder %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar] %>% unique()

  # query-lista till pxweb-uttag
  varlista <- list(
  "Region" = region_vekt,
  "Hustyp" = hustyp_vekt,
  "ContentsCode" = cont_vekt,
  "Tid" = tid_koder)

  if (all(is.na(hustyp_klartext))) varlista <- varlista[names(varlista) != "Hustyp"]

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
