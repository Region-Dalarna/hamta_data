hamta_yrke_alder_kon_region_ssyk3_2012_alder_kon_tid_scb <- function(
			region_vekt = "20",			   # Val av region. Finns: "00", "01", "03", "04", "05", "06", "07", "08", "09", "10", "12", "13", "14", "17", "18", "19", "20", "21", "22", "23", "24", "25", "99"
			yrke2012_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: t.ex. "Officerare", "Specialistofficerare", "Soldater m.fl.", "Politiker och högre ämbetsmän", "Verkställande direktörer m.fl.", "Ekonomi- och finanschefer", "Personal- och HR-chefer", "Förvaltnings- och planeringschefer", "Informations-, kommunikations- och PR-chefer", "Försäljnings- och marknadschefer", "Tvättare, fönsterputsare och övriga rengöringsarbetare", "Bärplockare och plantörer m.fl.", "Grovarbetare inom bygg och anläggning", "Handpaketerare och andra fabriksarbetare", "Hamnarbetare och ramppersonal m.fl.", "Snabbmatspersonal, köks- och restaurangbiträden m.fl.", "Torg- och marknadsförsäljare", "Återvinningsarbetare", "Tidningsdistributörer, vaktmästare och övriga servicearbetare", "yrke okänt"
			alder_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "16-24 år", "25-29 år", "30-34 år", "35-39 år", "40-44 år", "45-49 år", "50-54 år", "55-59 år", "60-64 år", "65-69 år"
			kon_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "män", "kvinnor"
			cont_klartext = "*",			 #  Finns: "Antal"
			tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023"
			long_format = TRUE,			# TRUE = konvertera innehållsvariablerna i datasetet till long-format 
			wide_om_en_contvar = TRUE,			# TRUE = om man vill behålla wide-format om det bara finns en innehållsvariabel, FALSE om man vill konvertera till long-format även om det bara finns en innehållsvariabel
			output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
			excel_filnamn = "yrke_alder_kon.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
			returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
){

  # ====================================================================================================
  #
  # Funktion för att hämta data från SCB:s API med hjälp av pxweb-paketet
  # Automatgenererat av en funktion i R som skrivits av Peter Möller, Region Dalarna
  #
  # Skapad av: moepet den 15 december 2025
  # Senast uppdaterad: 15 december 2025
  #
  # url till tabellens API: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0208__AM0208D/YREG57BAS/
  #												https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0208__AM0208D/YREG57N/
  #												https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0208__AM0208D/YREG57/
  #
  # ====================================================================================================

  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
    			tidyverse,
    			writexl)

  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

  # Url till databas
  url_list <- c("https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0208/AM0208D/YREG57BAS",
						"https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0208/AM0208D/YREG57N",
						"https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0208/AM0208D/YREG57")

 hamta_data <- function(url_uttag) {

  px_meta <- pxweb_get(url_uttag)

  varlist_koder <- pxvarlist(px_meta)$koder
  varlist_bada <- pxvarlist(px_meta)

  # Gör om från klartext till kod som databasen förstår
  yrke2012_vekt <- if (!all(is.na(yrke2012_klartext))) hamta_kod_med_klartext(px_meta, yrke2012_klartext, skickad_fran_variabel = "yrke2012") else NA
  alder_vekt <- if (!all(is.na(alder_klartext))) hamta_kod_med_klartext(px_meta, alder_klartext, skickad_fran_variabel = "alder") else NA
  kon_vekt <- if (!all(is.na(kon_klartext))) hamta_kod_med_klartext(px_meta, kon_klartext, skickad_fran_variabel = "kon") else NA

  alder_vekt <- if (!all(is.na(alder_klartext))) hamta_kod_med_klartext(px_meta, alder_klartext, skickad_fran_variabel = "alder") else NA
  cont_vekt <-  hamta_kod_med_klartext(px_meta, cont_klartext, "contentscode")
  if (length(cont_vekt) > 1) wide_om_en_contvar <- FALSE

  # Hantera tid-koder
  giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "tid")
  tid_vekt <- if (all(tid_koder != "*")) tid_koder %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar] %>% unique() else giltiga_ar



	# special, ta bort överlappande värden mellan tabeller där de förekommer, värden från nyaste tabellerna behålls
	if (url_uttag == 'https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0208/AM0208D/YREG57N') {
		if (tid_koder == "*") tid_vekt <- giltiga_ar
		tid_vekt <- tid_vekt[!tid_vekt %in% c('2020', '2021')]
	}

	if (length(tid_vekt) > 0) {
	  # query-lista till pxweb-uttag
	  varlista <- 	list(
  	"Region" = region_vekt,
  	"Yrke2012" = yrke2012_vekt,
  	"Alder" = alder_vekt,
  	"Kon" = kon_vekt,
  	"ContentsCode" = cont_vekt,
  	"Tid" = tid_vekt)	

	  if (all(is.na(yrke2012_klartext))) varlista <- varlista[names(varlista) != "Yrke2012"]
  if (all(is.na(alder_klartext))) varlista <- varlista[names(varlista) != "Alder"]
  if (all(is.na(kon_klartext))) varlista <- varlista[names(varlista) != "Kon"]

	  # Hämta data med varlista
	  px_uttag <- pxweb_get(url = url_uttag, query = varlista)

	  var_vektor <- c(regionkod = "Region", yrkeskod = "Yrke2012")
	  var_vektor_klartext <- c("region", "yrke")
	
	  # gör om pxweb-uttaget till en dataframe
	  px_df <- as.data.frame(px_uttag) %>% 
	    rename_with(~ "yrke", .cols = contains("yrke"))
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
   } # test om det finns giltig(a) tid-kod(er) i aktuell tabell
  } # slut hämta data-funktion 

  px_alla <- map(url_list, ~ hamta_data(.x)) %>% list_rbind()

  # Om användaren vill spara data till en Excel-fil
  if (!is.na(output_mapp) & !is.na(excel_filnamn)){
    write.xlsx(px_alla, paste0(output_mapp, excel_filnamn))
  }

  # Returnera data som en dataframe om användern valt det
  if (returnera_df) return(px_alla)
} # slut hämta data-funktion
