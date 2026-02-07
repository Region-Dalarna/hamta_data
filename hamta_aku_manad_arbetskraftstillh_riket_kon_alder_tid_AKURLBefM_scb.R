hamta_aku_manad_arbetskraftstillh_riket_kon_alder_tid_scb <- function(
			arbetskraftstillh_klartext = "*",			 #  Finns: "sysselsatta, 1000-tal", "sysselsättningsgrad, procent", "arbetslösa, 1000-tal", "arbetslöshetstal, procent", "i arbetskraften, 1000-tal", "arbetskraftstal, procent", "ej i arbetskraften, 1000-tal", "befolkningen, 1000-tal"
			typdata_klartext = "*",			 #  Finns: "icke säsongrensad", "icke säsongrensad, felmarginal ±", "icke säsongrensad, förändring ett år", "icke säsongrensad, felmarginal förändring ett år ±", "säsongrensad", "säsongrensad och utjämnad", "säsongrensad och utjämnad, förändringstakt ett år"
			kon_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "män", "kvinnor", "totalt"
			alder_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "15-19 år", "15-24 år", "20-24 år", "25-34 år", "25-54 år", "35-44 år", "45-54 år", "55-64 år", "55-74 år", "65-74 år", "totalt 15-74 år", "totalt 16-64 år", "totalt 16-65 år", "totalt 16-66 år", "totalt 20-64 år", "totalt 20-65 år", "totalt 20-66 år"
			cont_klartext = "*",			 #  Finns: "Befolkningen 15-74 år"
			tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: t.ex. "2001M01", "2001M02", "2001M03", "2001M04", "2001M05", "2001M06", "2001M07", "2001M08", "2001M09", "2001M10", "2025M03", "2025M04", "2025M05", "2025M06", "2025M07", "2025M08", "2025M09", "2025M10", "2025M11", "2025M12"
			output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
			excel_filnamn = "aku_manad.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
			returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
){

  # ====================================================================================================
  #
  # Funktion för att hämta data från SCB:s API med hjälp av pxweb-paketet
  # Automatgenererat av en funktion i R som skrivits av Peter Möller, Region Dalarna
  #
  # Skapad av: moepet den 07 februari 2026
  # Senast uppdaterad: 07 februari 2026
  #
  # url till tabellens API: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0401__AM0401A/AKURLBefM/
  #
  # ====================================================================================================

  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
    			tidyverse,
    			writexl)

  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

  # Url till databas
  url_uttag <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0401/AM0401A/AKURLBefM"
  px_meta <- pxweb_get(url_uttag)

  varlist_koder <- pxvarlist(px_meta)$koder
  varlist_bada <- pxvarlist(px_meta)

  # Gör om från klartext till kod som databasen förstår
  arbetskraftstillh_vekt <- hamta_kod_med_klartext(px_meta, arbetskraftstillh_klartext, skickad_fran_variabel = "arbetskraftstillh")
  typdata_vekt <- hamta_kod_med_klartext(px_meta, typdata_klartext, skickad_fran_variabel = "typdata")
  kon_vekt <- if (!all(is.na(kon_klartext))) hamta_kod_med_klartext(px_meta, kon_klartext, skickad_fran_variabel = "kon") else NA
  alder_vekt <- if (!all(is.na(alder_klartext))) hamta_kod_med_klartext(px_meta, alder_klartext, skickad_fran_variabel = "alder") else NA

  alder_vekt <- if (!all(is.na(alder_klartext))) hamta_kod_med_klartext(px_meta, alder_klartext, skickad_fran_variabel = "alder") else NA
  cont_vekt <-  hamta_kod_med_klartext(px_meta, cont_klartext, "contentscode")
  if (length(cont_vekt) > 1) wide_om_en_contvar <- FALSE

  # Hantera tid-koder
  giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "tid")
  tid_vekt <- if (all(tid_koder != "*")) tid_koder %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar] %>% unique() else giltiga_ar

  # query-lista till pxweb-uttag
  varlista <- list(
  	"Arbetskraftstillh" = arbetskraftstillh_vekt,
  	"TypData" = typdata_vekt,
  	"Kon" = kon_vekt,
  	"Alder" = alder_vekt,
  	"ContentsCode" = cont_vekt,
  	"Tid" = tid_vekt)

  if (all(is.na(kon_klartext))) varlista <- varlista[names(varlista) != "Kon"]
  if (all(is.na(alder_klartext))) varlista <- varlista[names(varlista) != "Alder"]

  # Hämta data med varlista
  px_uttag <- pxweb_get(url = url_uttag, query = varlista)

  var_vektor <- NA
  var_vektor_klartext <- NA

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

  # Om användaren vill spara data till en Excel-fil
  if (!is.na(output_mapp) & !is.na(excel_filnamn)){
    write.xlsx(px_df, paste0(output_mapp, excel_filnamn))
  }

  # Returnera data som en dataframe om användern valt det
  if (returnera_df) return(px_df)
} # slut hämta data-funktion
