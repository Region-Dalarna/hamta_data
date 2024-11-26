hamta_rmi_region_amnematch_nivamatch_utbildning_tid_scb <- function(
			region_vekt = "20",			   # Val av region. Finns: "00", "FA00", "FA01", "FA02", "FA03", "FA04", "FA05", "FA06", "FA07", "FA08", "FA09", "FA10", "FA11", "FA12", "FA13", "FA14", "FA15", "FA16", "FA17", "FA18", "FA19", "FA20", "FA21", "FA22", "FA23", "FA24", "FA25", "FA26", "FA27", "FA28", "FA29", "FA30", "FA31", "FA32", "FA33", "FA34", "FA35", "FA36", "FA37", "FA38", "FA39", "FA40", "FA41", "FA42", "FA43", "FA44", "FA45", "FA46", "FA47", "FA48", "FA49", "FA50", "FA51", "FA52", "FA53", "FA54", "FA55", "FA56", "FA57", "FA58", "FA59", "FA60", "01", "03", "04", "05", "06", "07", "08", "09", "10", "12", "13", "14", "17", "18", "19", "20", "21", "22", "23", "24", "25"
			amnematch_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "utb. inriktning stämmer inte med yrket", "utb. inriktning stämmer delvis med yrket", "utb. inriktning stämmer helt med yrket", "inte tillräckliga uppgifter"
			nivamatch_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "utb. nivå lägre än vad yrket kräver", "utb. nivå högre än vad yrket kräver", "utb. nivå rätt för yrket", "inte tillräckliga uppgifter"
			utbildning_klartext = "*",			 #  Finns: "samtliga utbildningsgrupper", "förskollärarutbildning ", "fritidspedagogutbildning ", "lärarutbildning för grundskolans tidigare år ", "lärarutbildning grsk senare år och gymn., allmänna + praktiskt-estet.", "speciallärar- och specialpedagogutbildning ", "yrkeslärarutbildning ", "övrig utbildning inom pedagogik / lärarutbildning, eftergymnasial", "humanistisk utbildning, eftergymnasial nivå (minst 3 år) ", "konstnärlig utbildning, eftergymnasial nivå ", "utbildning inom medieproduktion, eftergymnasial nivå ", "teologisk utbildning, eftergymnasial nivå (minst 3 år) ", "övrig utbildning inom humaniora och konst, eftergymnasial nivå ", "handel- och administrationsutbildning, gymnasial nivå ", "biblioteks- och informationsvetensk. högskoleutbildning (minst 3 år) ", "ekonomutbildning, högskoleutbildning (minst 3 år) ", "personal- och beteendevetarutbildning, högskoleutb. (minst 3 år) ", "juristutbildning ", "journalistik och medievetenskaplig utbildning, eftergymnasial nivå ", "psykologutbildning ", "samhällsvetar- och förvaltningsutb. högskoleutbildning (minst 3 år) ", "övrig utb. i samhällsvetenskap, juridik, handel, admin., eftergymnasial", "medicinsk sekreterarutbildning", "YH-utbildning i företagsekonomi, handel, administration", "biologutbildning, högskoleutbildning (minst 3 år) ", "datautbildning, eftergymnasial nivå ", "data och IT-utbildning, eftergymnasial (minst 3 år)", "data och IT utbildning, eftergymnasial (kortare än 3år)", "fysik- och  matematikutbildning, eftergymnasial nivå (minst 3år)", "geovetenskaplig utbildning, högskoleutbildning (minst 3 år) ", "kemistutbildning, högskoleutbildning (minst 3 år) ", "övrig naturvetenskaplig högskoleutbildning (minst 3 år) ", "övrig utbildning inom naturvetenskap, matematik, data, eftergymnasial", "gymnasieingenjörsutbildning", "byggutbildning, gymnasial nivå ", "data-, el- och energiteknisk utbildning, gymnasial nivå ", "fordonsutbildning, gymnasial nivå ", "industriutbildning, gymnasial nivå ", "vvs- och fastighetsutbildning, gymnasial nivå ", "arkitektutbildning ", "civilingenjörsutbildning; industriell ekonomi", "civilingenjörsutbildning; väg- och vatten, byggnadsteknik, lantmäteri", "civilingenjörsutbildning; maskinteknik, fordons- och farkostteknik", "civilingenjörsutbildning; teknisk fysik, elektro- och datateknik", "civilingenjörsutbildning; kemi- och bioteknik, material- och geoteknik", "civilingenjörsutbildning; övrig/okänd inriktning", "högskoleingenjörsutb.; väg- och vatten, byggnadsteknik, lantmäteri", "högsk.ing.utb; maskinteknik, fordons- farkostteknik, industriell ekon.", "högskoleingenjörsutbildning; teknisk fysik, elektro- och datateknik", "högskoleingenjörsutb.; kemi- och bioteknik, material- och geoteknik", "högskoleingenjörsutbildning; övrig/okänd inriktning", "teknikutbildning, yrkeshögskolan", "övrig utbildning inom teknik och tillverkning, eftergymnasial nivå ", "naturbruksutbildning, gymnasial nivå ", "agronom- och hortonomutbildning ", "skogsvetenskaplig utbildning, högskoleutbildning (minst 3 år) ", "veterinärutbildning ", "övrig utb. inom lant- och skogsbruk, djursjukvård, eftergymnasial", "barn- och fritidsutbildning, gymnasial nivå ", "vård- och omsorgsutb.; övrig gymn. utb. i hälso- och sjukvård", "tandsköterskeutbildning ", "apotekarutbildning ", "arbetsterapeututbildning ", "biomedicinsk analytikerutbildning ", "fritidsledarutbildning, eftergymnasial nivå ", "läkarutbildning (exkl. disputerade som saknar läkarexamen) ", "läkarutbildning, med specialistkompetens", "receptarieutbildning ", "sjukgymnast-/fysioterapeututbildning ", "barnmorskeutbildning ", "sjuksköterskeutbildning, grundutbildning ", "social omsorgsutbildning, eftergymnasial nivå ", "socionomutbildning ", "röntgensjuksköterskeutbildning", "specialistsjuksköterskeutbildning", "specialistsjuksköterskeutbildning; anestesi-, intensiv-, operations- och ambulanssjukvård", "specialistsjuksköterskeutbildning; barn och ungdom", "specialistsjuksköterskeutbildning; distriktssköterska", "specialistsjuksköterskeutbildning; psykiatrisk vård", "specialistsjuksköterskeutbildning; övriga inriktningar", "tandhygienistutbildning ", "tandläkarutbildning ", "övrig utb. inom hälso- och sjukvård, social omsorg, eftergymnasial", "hotell- och turismutbildning, gymnasial", "restaurang- och livsmedelsutbildning, gymnasial nivå ", "transportutbildning, gymnasial nivå ", "polisutbildning ", "transportutbildning, eftergymnasial nivå ", "övrig utbildning inom tjänsteområdet, eftergymnasial nivå "
			cont_klartext = "*",			 #  Finns: "Anställda 20-64 år"
			tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022"
			long_format = TRUE,			# TRUE = konvertera innehållsvariablerna i datasetet till long-format 
			wide_om_en_contvar = TRUE,			# TRUE = om man vill behålla wide-format om det bara finns en innehållsvariabel, FALSE om man vill konvertera till long-format även om det bara finns en innehållsvariabel
			output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
			excel_filnamn = "rmi.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
			returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
){

  # ====================================================================================================
  #
  # Funktion för att hämta data från SCB:s API med hjälp av pxweb-paketet
  # Automatgenererat av en funktion i R som skrivits av Peter Möller, Region Dalarna
  #
  # Skapad av: moepet den 17 november 2024
  # Senast uppdaterad: 17 november 2024
  #
  # url till tabellens API: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM9906__AM9906A/RegionInd19E4N1/
  #												https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM9906__AM9906O/RegionInd19E4N/
  #
  # ====================================================================================================

  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
    			tidyverse,
    			writexl)

  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

  # Url till SCB:s databas
  url_list <- c("https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM9906/AM9906A/RegionInd19E4N1",
						"https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM9906/AM9906O/RegionInd19E4N")

 hamta_data <- function(url_uttag) {

  px_meta <- pxweb_get(url_uttag)

  varlist_koder <- pxvarlist(px_meta)$koder
  varlist_bada <- pxvarlist(px_meta)

  # Gör om från klartext till kod som databasen förstår
  amnematch_vekt <- if (!all(is.na(amnematch_klartext))) hamta_kod_med_klartext(px_meta, amnematch_klartext, skickad_fran_variabel = "amnematch") else NA
  nivamatch_vekt <- if (!all(is.na(nivamatch_klartext))) hamta_kod_med_klartext(px_meta, nivamatch_klartext, skickad_fran_variabel = "nivamatch") else NA
  utbildning_vekt <- hamta_kod_med_klartext(px_meta, utbildning_klartext, skickad_fran_variabel = "utbildning")

  cont_vekt <-  hamta_kod_med_klartext(px_meta, cont_klartext, "contentscode")
  if (length(cont_vekt) > 1) wide_om_en_contvar <- FALSE

  # Hantera tid-koder
  giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "tid")
  tid_vekt <- if (all(tid_koder != "*")) tid_koder %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar] %>% unique() else giltiga_ar



	# special, ta bort överlappande värden mellan tabeller där de förekommer, värden från nyaste tabellerna behålls
	if (url_uttag == 'https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM9906/AM9906O/RegionInd19E4N') {
		if (tid_koder == "*") tid_vekt <- giltiga_ar
		tid_vekt <- tid_vekt[!tid_vekt %in% c('2019', '2020', '2021')]
	}

	if (length(tid_vekt) > 0) {
	  # query-lista till pxweb-uttag
	  varlista <- 	list(
  	"Region" = region_vekt,
  	"AmneMatch" = amnematch_vekt,
  	"NivaMatch" = nivamatch_vekt,
  	"Utbildning" = utbildning_vekt,
  	"ContentsCode" = cont_vekt,
  	"Tid" = tid_vekt)	

	  if (all(is.na(amnematch_klartext))) varlista <- varlista[names(varlista) != "AmneMatch"]
  if (all(is.na(nivamatch_klartext))) varlista <- varlista[names(varlista) != "NivaMatch"]

	  # Hämta data med varlista
	  px_uttag <- pxweb_get(url = url_uttag, query = varlista)

	  var_vektor <- c(regionkod = "Region")
	  var_vektor_klartext <- "region"
	
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
