hamta_skogsmark_region_former_tid_scb <- function(
			region_vekt = "20",			   # Val av region. Finns: "00", "01", "OGF", "NGF", "FJR", "NOR", "SYR", "BOR", "NEM", "03", "NON", "SON", "SVE", "GOT", "04", "05", "06", "07", "08", "09", "10", "12", "13", "14", "17", "18", "19", "20", "21", "22", "23", "24", "25"
			overlapp_klartext = "*",			 #  Finns: "Med överlappande arealer", "Utan överlappande arealer"
			typskogsmark_klartext = "*",			 #  Finns: "Produktiv skogsmark", "Skogsmark totalt"
			former_klartext = "*",			 #  Finns: "Formellt skyddad skogsmark", "Frivilliga avsättningar", "Hänsynsytor", "Improduktiv skogsmark"
			cont_klartext = "*",			 #  Finns: "Area i hektar", "Andel i procent"
			tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2018", "2019", "2020", "2021", "2022", "2023", "2024"
			long_format = TRUE,			# TRUE = konvertera innehållsvariablerna i datasetet till long-format 
			wide_om_en_contvar = TRUE,			# TRUE = om man vill behålla wide-format om det bara finns en innehållsvariabel, FALSE om man vill konvertera till long-format även om det bara finns en innehållsvariabel
			output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
			excel_filnamn = "skogsmarkd.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
			returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
){

  # ====================================================================================================
  #
  # Funktion för att hämta data från SCB:s API med hjälp av pxweb-paketet
  # Automatgenererat av en funktion i R som skrivits av Peter Möller, Region Dalarna
  #
  # Skapad av: frkjon den 10 november 2025
  # Senast uppdaterad: 10 november 2025
  #
  # url till tabellens API: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__MI__MI0605/SkyddSkogFrivillig/
  #
  # ====================================================================================================

  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
    			tidyverse,
    			writexl)

  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

  # Url till databas
  url_uttag <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/MI/MI0605/SkyddSkogFrivillig"
  px_meta <- pxweb_get(url_uttag)

  varlist_koder <- pxvarlist(px_meta)$koder
  varlist_bada <- pxvarlist(px_meta)

  # Gör om från klartext till kod som databasen förstår
  overlapp_vekt <- hamta_kod_med_klartext(px_meta, overlapp_klartext, skickad_fran_variabel = "overlapp")
  typskogsmark_vekt <- hamta_kod_med_klartext(px_meta, typskogsmark_klartext, skickad_fran_variabel = "typskogsmark")
  former_vekt <- hamta_kod_med_klartext(px_meta, former_klartext, skickad_fran_variabel = "former")

  cont_vekt <-  hamta_kod_med_klartext(px_meta, cont_klartext, "contentscode")
  if (length(cont_vekt) > 1) wide_om_en_contvar <- FALSE

  # Hantera tid-koder
  giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "tid")
  tid_vekt <- if (all(tid_koder != "*")) tid_koder %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar] %>% unique() else giltiga_ar

  # query-lista till pxweb-uttag
  varlista <- list(
  	"Region" = region_vekt,
  	"Overlapp" = overlapp_vekt,
  	"TypSkogsmark" = typskogsmark_vekt,
  	"Former" = former_vekt,
  	"ContentsCode" = cont_vekt,
  	"Tid" = tid_vekt)

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

  # Om användaren vill spara data till en Excel-fil
  if (!is.na(output_mapp) & !is.na(excel_filnamn)){
    write.xlsx(px_df, paste0(output_mapp, excel_filnamn))
  }

  # Returnera data som en dataframe om användern valt det
  if (returnera_df) return(px_df)
} # slut hämta data-funktion
