hamta_lediga_jobb_region_sektor_tid_scb <- function(
			region_vekt = "20",			# Val av region. Finns: "00", "01", "03", "04", "05", "06", "07", "08", "09", "10", "12", "13", "14", "17", "18", "19", "20", "21", "22", "23", "24", "25"
			sektor_klartext = "*",			 #  Finns: "offentlig sektor", "privat sektor", "totalt", "hela ekonomin", "näringslivet och hushållens icke-vinstdrivande organisationer", "offentlig förvaltning"
			cont_klartext = "*",			 #  Finns: "Lediga jobb", "Felmarginal ±", "Lediga jobb, totalt", "Lediga jobb, totalt, osäkerhetsmarginal", "Lediga jobb med omgående tillträde", "Lediga jobb med omgående tillträde, osäkerhetsmargnial"
			tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2006K1", "2006K2", "2006K3", "2006K4", "2007K1", "2007K2", "2007K3", "2007K4", "2008K1", "2008K2", "2008K3", "2008K4", "2009K1", "2009K2", "2009K3", "2009K4", "2010K1", "2010K2", "2010K3", "2010K4", "2011K1", "2011K2", "2011K3", "2011K4", "2012K1", "2012K2", "2012K3", "2012K4", "2013K1", "2013K2", "2013K3", "2013K4", "2014K1", "2014K2", "2014K3", "2014K4", "2015K1", "2015K2", "2015K3", "2015K4", "2016K1", "2016K2", "2016K3", "2016K4", "2017K1", "2017K2", "2017K3", "2017K4", "2018K1", "2018K2", "2018K3", "2018K4", "2019K1", "2019K2", "2019K3", "2019K4", "2020K1", "2020K2", "2020K3", "2020K4", "2021K1", "2021K2", "2021K3", "2021K4", "2022K1", "2022K2", "2022K3", "2022K4", "2023K1", "2023K2", "2023K3", "2023K4", "2024K1", "2024K2"
			kvartal_klartext = NA,      # NA = alla kvartal, "9999" = senaste tillgängliga kvartalet, 1,2,3,4 = kvartal 1,2,3,4    det går att blanda siffror med "9999" om man vill ha senaste kvartal + något eller ett par till
			long_format = TRUE,			# TRUE = konvertera innehållsvariablerna i datasetet till long-format 
			wide_om_en_contvar = TRUE,			# TRUE = om man vill behålla wide-format om det bara finns en innehållsvariabel, FALSE om man vill konvertera till long-format även om det bara finns en innehållsvariabel
			output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
			excel_filnamn = "lediga_jobb.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
			returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
){

  # ====================================================================================================
  #
  # Funktion för att hämta data från SCB:s API med hjälp av pxweb-paketet
  # Automatgenererat av en funktion i R som skrivits av Peter Möller, Region Dalarna
  #
  # Skapad av: frkjon den 04 oktober 2024
  # Senast uppdaterad: 04 oktober 2024
  #
  # url till tabellens API: https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM9906/AM9906O/RegionIndE1K
  #												https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM9906/AM9906A/RegionIndE1KN
  #
  # ====================================================================================================

  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
    			tidyverse,
    			writexl)

  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

  # Url till SCB:s databas
  url_list <- c("https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM9906/AM9906O/RegionIndE1K",
						"https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM9906/AM9906A/RegionIndE1KN")

  # om man vill ha specifikt kvartal så plockas det fram utifrån vilka kvartal som är tillgängliga i tabellerna 
  if (!is.na(kvartal_klartext)) {
    alla_tidkoder <- map(url_list, ~ pxvardelist(.x, "tid")) %>% list_rbind() %>% dplyr::pull(kod) %>% sort()  
    kvartal_senaste <- if ("9999" %in% kvartal_klartext) max(alla_tidkoder) %>% str_sub(., nchar(.))
    kvartal_siffror <- kvartal_klartext %>% as.character() %>% str_remove("K") %>% .[str_sub(., nchar(.)) %in% c("1", "2", "3", "4")]
    kvartal_varde <- c(kvartal_senaste, kvartal_siffror) %>% unique()
  }
  
  
 hamta_data <- function(url_uttag) {

  px_meta <- pxweb_get(url_uttag)

  varlist_koder <- pxvarlist(px_meta)$koder
  varlist_bada <- pxvarlist(px_meta)

  # Gör om från klartext till kod som databasen förstår
  sektor_vekt <- hamta_kod_med_klartext(px_meta, sektor_klartext, skickad_fran_variabel = "sektor")

  cont_vekt <-  hamta_kod_med_klartext(px_meta, cont_klartext, "contentscode")
  if (length(cont_vekt) > 1) wide_om_en_contvar <- FALSE

  # Hantera tid-koder
  giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "tid")
  tid_vekt <- if (all(tid_koder != "*")) tid_koder %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar] %>% unique() else giltiga_ar
  
  if (!is.na(kvartal_klartext)) {
    tid_vekt <- tid_vekt %>% .[str_sub(., nchar(.)) %in% kvartal_varde]
  }
  
  if (length(tid_vekt) > 0) {
      # query-lista till pxweb-uttag
    varlista <- list(
    	"Region" = region_vekt,
    	"Sektor" = sektor_vekt,
    	"ContentsCode" = cont_vekt,
    	"Tid" = tid_vekt)
  
    px_uttag <- pxweb_get(url = url_uttag, query = varlista)
  
    var_vektor <- c(regionkod = "Region")
    var_vektor_klartext <- "region"
  
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
    } # slut if-sats om tid-vekt har några värden
  } # slut hämta data-funktion 

  px_alla <- map(url_list, ~ hamta_data(.x)) %>% list_rbind()

  # Om användaren vill spara data till en Excel-fil
  if (!is.na(output_mapp) & !is.na(excel_filnamn)){
    write.xlsx(px_alla, paste0(output_mapp, excel_filnamn))
  }

  # Returnera data som en dataframe om användern valt det
  if (returnera_df) return(px_alla)

}
