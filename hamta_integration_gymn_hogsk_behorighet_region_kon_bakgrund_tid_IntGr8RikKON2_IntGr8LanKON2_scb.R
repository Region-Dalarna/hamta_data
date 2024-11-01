hamta_integration_gymn_hogsk_behorighet_region_kon_bakgrund_tid_scb <- function(
			region_vekt = "20",			   # Val av region. Finns: "00", "01", "03", "04", "05", "06", "07", "08", "09", "10", "12", "13", "14", "17", "18", "19", "20", "21", "22", "23", "24", "25"
			kon_klartext = "*",			 #  Finns: "män och kvinnor", "män", "kvinnor"
			bakgrund_klartext = "*",			 #  Finns: "samtliga", "födelseregion: Sverige", "födelseregion: Norden exkl. Sverige", "födelseregion: EU/EFTA exkl. Norden", "födelseregion: övriga världen", "samtliga utrikes födda invandrare", "skäl till invandring: skyddsbehövande och deras anhöriga", "skäl till invandring: övriga utrikes födda invandrare", "samtliga utrikes födda", "vistelsetid 0-1 år", "vistelsetid 2-3 år", "vistelsetid 4-9 år", "vistelsetid 10- år"
			cont_klartext = "*",			 #  Finns: "Andel behöriga till gymnasium, procent", "Andel behöriga till högskola, procent"
			tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022"
			long_format = TRUE,			# TRUE = konvertera innehållsvariablerna i datasetet till long-format 
			wide_om_en_contvar = TRUE,			# TRUE = om man vill behålla wide-format om det bara finns en innehållsvariabel, FALSE om man vill konvertera till long-format även om det bara finns en innehållsvariabel
			output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
			excel_filnamn = "integration.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
			returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
){

  # ====================================================================================================
  #
  # Funktion för att hämta data från SCB:s API med hjälp av pxweb-paketet
  # Automatgenererat av en funktion i R som skrivits av Peter Möller, Region Dalarna
  #
  # Skapad av: moepet den 30 oktober 2024
  # Senast uppdaterad: 30 oktober 2024
  #
  # url till tabellens API: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AA__AA0003__AA0003H/IntGr8RikKON2/
  #												https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AA__AA0003__AA0003H/IntGr8LanKON2/
  #
  # ====================================================================================================

  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
    			tidyverse,
    			writexl)

  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

  # Url till SCB:s databas
  url_list <- c("https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AA/AA0003/AA0003H/IntGr8RikKON2",
						"https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AA/AA0003/AA0003H/IntGr8LanKON2")

 hamta_data <- function(url_uttag) {

  px_meta <- pxweb_get(url_uttag)

  varlist_koder <- pxvarlist(px_meta)$koder
  varlist_bada <- pxvarlist(px_meta)
  
  # Hantera region-koder då vi har en tabell för riket och en för länen
  giltiga_regionkoder <- hamta_giltiga_varden_fran_tabell(px_meta, "region")
  region_giltig_vekt <- region_vekt[region_vekt %in% giltiga_regionkoder]

  # Gör om från klartext till kod som databasen förstår
  kon_vekt <- hamta_kod_med_klartext(px_meta, kon_klartext, skickad_fran_variabel = "kon")
  bakgrund_vekt <- hamta_kod_med_klartext(px_meta, bakgrund_klartext, skickad_fran_variabel = "bakgrund")

  cont_vekt <-  hamta_kod_med_klartext(px_meta, cont_klartext, "contentscode")
  if (length(cont_vekt) > 1) wide_om_en_contvar <- FALSE

  # Hantera tid-koder
  giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "tid")
  tid_vekt <- if (all(tid_koder != "*")) tid_koder %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar] %>% unique() else giltiga_ar

  if (length(region_giltig_vekt) > 0) {
	  # query-lista till pxweb-uttag
	  varlista <- 	list(
  	"Region" = region_giltig_vekt,
  	"Kon" = kon_vekt,
  	"Bakgrund" = bakgrund_vekt,
  	"ContentsCode" = cont_vekt,
  	"Tid" = tid_vekt)	

		  # Hämta data med varlista
	  px_uttag <- pxweb_get(url = url_uttag, query = varlista)

	  var_vektor <- c(regionkod = "Region")
	  var_vektor_klartext <- "region"
	
	  # gör om pxweb-uttaget till en dataframe
	  px_df <- suppress_specific_warning(as.data.frame(px_uttag), "NAs introduced by coercion")
	  if (!all(is.na(var_vektor))) {
	      # om man vill ha med koder också för variabler utöver klartext så läggs de på här (om det finns värden i var_vektor)
	      px_df <- suppress_specific_warning( px_df %>% cbind(as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>%
	            select(any_of(var_vektor))), "NAs introduced by coercion")

	      # kolumnerna med koder läggs framför motsvarande kolumner med klartext
	      for (varflytt_index in 1:length(var_vektor)) {
	        px_df <- px_df %>%
	            relocate(any_of(names(var_vektor)[varflytt_index]), .before = any_of(var_vektor_klartext[varflytt_index]))
	      }
	  }

	  # man kan välja bort long-format, då låter vi kolumnerna vara wide om det finns fler innehållsvariabler, annars
	  # pivoterar vi om till long-format, dock ej om det bara finns en innehållsvariabel
	  if (long_format & !wide_om_en_contvar) px_df <- px_df %>% konvertera_till_long_for_contentscode_variabler(url_uttag, content_var = "bakgrund")

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
