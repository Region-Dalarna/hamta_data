hamta_syss_aku_arbetskraftstillh_inrikesutrikes_kon_alder_tid_scb <- function(
			arbetskraftstillh_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "totalt", "arbetslösa", "ej i arbetskraften", "sysselsatta"
			inrikesutrikes_klartext = c("inrikes födda", "utrikes födda"),			 #  NA = tas inte med i uttaget,  Finns: "inrikes födda", "utrikes födda", "inrikes och utrikes födda"
			kon_klartext = c("män", "kvinnor"),			 #  NA = tas inte med i uttaget,  Finns: "män", "kvinnor", "totalt"
			alder_klartext = "totalt 20-64 år",			 #  NA = tas inte med i uttaget,  Finns: "15-24 år", "25-54 år", "55-74 år", "totalt 15-74 år", "totalt 16-64 år", "totalt 16-65 år", "totalt 16-66 år", "totalt 20-64 år", "totalt 20-65 år", "totalt 20-66 år"
			cont_klartext = "Procent",			 #  Finns: "1000-tal", "Felmarginal ±, 1000-tal", "Procent", "Felmarginal ±, procent"
			tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2005M01", "2005M02", "2005M03", "2005M04", "2005M05", "2005M06", "2005M07", "2005M08", "2005M09", "2005M10", "2005M11", "2005M12", "2006M01", "2006M02", "2006M03", "2006M04", "2006M05", "2006M06", "2006M07", "2006M08", "2006M09", "2006M10", "2006M11", "2006M12", "2007M01", "2007M02", "2007M03", "2007M04", "2007M05", "2007M06", "2007M07", "2007M08", "2007M09", "2007M10", "2007M11", "2007M12", "2008M01", "2008M02", "2008M03", "2008M04", "2008M05", "2008M06", "2008M07", "2008M08", "2008M09", "2008M10", "2008M11", "2008M12", "2009M01", "2009M02", "2009M03", "2009M04", "2009M05", "2009M06", "2009M07", "2009M08", "2009M09", "2009M10", "2009M11", "2009M12", "2010M01", "2010M02", "2010M03", "2010M04", "2010M05", "2010M06", "2010M07", "2010M08", "2010M09", "2010M10", "2010M11", "2010M12", "2011M01", "2011M02", "2011M03", "2011M04", "2011M05", "2011M06", "2011M07", "2011M08", "2011M09", "2011M10", "2011M11", "2011M12", "2012M01", "2012M02", "2012M03", "2012M04", "2012M05", "2012M06", "2012M07", "2012M08", "2012M09", "2012M10", "2012M11", "2012M12", "2013M01", "2013M02", "2013M03", "2013M04", "2013M05", "2013M06", "2013M07", "2013M08", "2013M09", "2013M10", "2013M11", "2013M12", "2014M01", "2014M02", "2014M03", "2014M04", "2014M05", "2014M06", "2014M07", "2014M08", "2014M09", "2014M10", "2014M11", "2014M12", "2015M01", "2015M02", "2015M03", "2015M04", "2015M05", "2015M06", "2015M07", "2015M08", "2015M09", "2015M10", "2015M11", "2015M12", "2016M01", "2016M02", "2016M03", "2016M04", "2016M05", "2016M06", "2016M07", "2016M08", "2016M09", "2016M10", "2016M11", "2016M12", "2017M01", "2017M02", "2017M03", "2017M04", "2017M05", "2017M06", "2017M07", "2017M08", "2017M09", "2017M10", "2017M11", "2017M12", "2018M01", "2018M02", "2018M03", "2018M04", "2018M05", "2018M06", "2018M07", "2018M08", "2018M09", "2018M10", "2018M11", "2018M12", "2019M01", "2019M02", "2019M03", "2019M04", "2019M05", "2019M06", "2019M07", "2019M08", "2019M09", "2019M10", "2019M11", "2019M12", "2020M01", "2020M02", "2020M03", "2020M04", "2020M05", "2020M06", "2020M07", "2020M08", "2020M09", "2020M10", "2020M11", "2020M12", "2021M01", "2021M02", "2021M03", "2021M04", "2021M05", "2021M06", "2021M07", "2021M08", "2021M09", "2021M10", "2021M11", "2021M12", "2022M01", "2022M02", "2022M03", "2022M04", "2022M05", "2022M06", "2022M07", "2022M08", "2022M09", "2022M10", "2022M11", "2022M12", "2023M01", "2023M02", "2023M03", "2023M04", "2023M05", "2023M06", "2023M07", "2023M08", "2023M09", "2023M10", "2023M11", "2023M12", "2024M01"
			long_format = TRUE,			# TRUE = konvertera innehållsvariablerna i datasetet till long-format 
			wide_om_en_contvar = TRUE,			# TRUE = om man vill behålla wide-format om det bara finns en innehållsvariabel, FALSE om man vill konvertera till long-format även om det bara finns en innehållsvariabel
			output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
			excel_filnamn = "syss_aku.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
			returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
){

  # ====================================================================================================
  #
  # Funktion för att hämta data från SCB:s API med hjälp av pxweb-paketet
  # Automatgenererat av en funktion i R som skrivits av Peter Möller, Region Dalarna
  #
  # Skapad av: moepet den 05 mars 2024
  # Senast uppdaterad: 05 mars 2024
  #
  # url till tabellens API: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0401__AM0401R/NAKUBefInrUtrJmfM/
  #
  # ====================================================================================================

if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
    			tidyverse,
    			writexl)

  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

  # Url till SCB:s databas
  url_uttag <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0401/AM0401R/NAKUBefInrUtrJmfM"
  px_meta <- pxweb_get(url_uttag)

  varlist_koder <- pxvarlist(px_meta)$koder
  varlist_bada <- pxvarlist(px_meta)

  # Gör om från klartext till kod som databasen förstår
  arbetskraftstillh_vekt <- if (!all(is.na(arbetskraftstillh_klartext))) hamta_kod_med_klartext(px_meta, arbetskraftstillh_klartext, skickad_fran_variabel = "arbetskraftstillh") else NA
  inrikesutrikes_vekt <- if (!all(is.na(inrikesutrikes_klartext))) hamta_kod_med_klartext(px_meta, inrikesutrikes_klartext, skickad_fran_variabel = "inrikesutrikes") else NA
  kon_vekt <- if (!all(is.na(kon_klartext))) hamta_kod_med_klartext(px_meta, kon_klartext, skickad_fran_variabel = "kon") else NA

  alder_vekt <- if (!all(is.na(alder_klartext))) hamta_kod_med_klartext(px_meta, alder_klartext, skickad_fran_variabel = "alder") else NA
  cont_vekt <-  hamta_kod_med_klartext(px_meta, cont_klartext, "contentscode")
  if (length(cont_vekt) > 1) wide_om_en_contvar <- FALSE

  # Hantera tid-koder
  giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "tid")
  if (all(tid_koder != "*")) tid_koder <- tid_koder %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar] %>% unique()

# query-lista till pxweb-uttag
  varlista <- list(
  "Arbetskraftstillh" = arbetskraftstillh_vekt,
  "InrikesUtrikes" = inrikesutrikes_vekt,
  "Kon" = kon_vekt,
  "Alder" = alder_vekt,
  "ContentsCode" = cont_vekt,
  "Tid" = tid_koder)

  if (all(is.na(arbetskraftstillh_klartext))) varlista <- varlista[names(varlista) != "Arbetskraftstillh"]
  if (all(is.na(inrikesutrikes_klartext))) varlista <- varlista[names(varlista) != "InrikesUtrikes"]
  if (all(is.na(kon_klartext))) varlista <- varlista[names(varlista) != "Kon"]
  if (all(is.na(alder_klartext))) varlista <- varlista[names(varlista) != "Alder"]

  px_uttag <- pxweb_get(url = url_uttag, query = varlista)

var_vektor <- NA
var_vektor_klartext <- NA
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
