hamta_fek_lve_region_sni2007_tid_scb <- function(
			region_vekt = "20",			# Val av region. Finns: "00", "01", "03", "04", "05", "06", "07", "08", "09", "10", "12", "13", "14", "17", "18", "19", "20", "21", "22", "23", "24", "25", "SE0", "SE00", "SE1", "SE11", "SE110", "SE12", "SE121", "SE122", "SE123", "SE124", "SE125", "SE2", "SE21", "SE211", "SE212", "SE213", "SE214", "SE22", "SE221", "SE224", "SE23", "SE231", "SE232", "SE3", "SE31", "SE311", "SE312", "SE313", "SE32", "SE321", "SE322", "SE33", "SE331", "SE332"
			sni2007_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "A-SexklK-O samtliga näringsgrenar (exkl. K+O+T+U)", "A-01-03 jordbruk, skogsbruk och fiske", "B-05-09 utvinning av mineral", "C-10-33 tillverkning", "D-35 försörjning av el, gas, värme och kyla", "E-36-39 vattenförsörjning; avloppsrening, avfallshantering och sanering", "F-41-43 byggverksamhet", "G-45-47 handel; reparation av motorfordon och motorcyklar", "H-49-53 transport och magasinering", "I-55-56 hotell- och restaurangverksamhet", "J-58-63 informations- och kommunikationsverksamhet", "L-68 fastighetsverksamhet", "M-69-75 verksamhet inom juridik, ekonomi, vetenskap och teknik", "N-77-82 uthyrning, fastighetsservice, resetjänster och andra stödtjänster", "P-85 utbildning", "Q-86-88 vård och omsorg; sociala tjänster", "R-90-93 kultur, nöje och fritid", "S-94-96 annan serviceverksamhet"
			cont_klartext = "*",			 #  Finns: "Antal arbetsställen (lokala verksamheter)", "Antal anställda", "Nettoomsättning exkl. merchantingkostnader, mnkr", "Produktionsvärde, mnkr", "Förädlingsvärde, mnkr", "Totala intäkter, mnkr", "Totala kostnader, mnkr"
			tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2022"
			long_format = TRUE,			# TRUE = konvertera innehållsvariablerna i datasetet till long-format 
			wide_om_en_contvar = TRUE,			# TRUE = om man vill behålla wide-format om det bara finns en innehållsvariabel, FALSE om man vill konvertera till long-format även om det bara finns en innehållsvariabel
			output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
			excel_filnamn = "fek_lve.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
			returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
){

  # ====================================================================================================
  #
  # Funktion för att hämta data från SCB:s API med hjälp av pxweb-paketet
  # Automatgenererat av en funktion i R som skrivits av Peter Möller, Region Dalarna
  #
  # Skapad av: moepet den 10 juni 2024
  # Senast uppdaterad: 10 juni 2024
  #
  # url till tabellens API: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__NV__NV0109__NV0109P/NSEBasfaktaLVEngs07/
  #
  # ====================================================================================================

  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
    			tidyverse,
    			writexl)

  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

  # Url till SCB:s databas
  url_uttag <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/NV/NV0109/NV0109P/NSEBasfaktaLVEngs07"
  px_meta <- pxweb_get(url_uttag)

  varlist_koder <- pxvarlist(px_meta)$koder
  varlist_bada <- pxvarlist(px_meta)

  # Gör om från klartext till kod som databasen förstår
  sni2007_vekt <- if (!all(is.na(sni2007_klartext))) hamta_kod_med_klartext(px_meta, sni2007_klartext, skickad_fran_variabel = "sni2007") else NA

  cont_vekt <-  hamta_kod_med_klartext(px_meta, cont_klartext, "contentscode")
  if (length(cont_vekt) > 1) wide_om_en_contvar <- FALSE

  # Hantera tid-koder
  giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "tid")
  if (all(tid_koder != "*")) tid_koder <- tid_koder %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar] %>% unique()

  # query-lista till pxweb-uttag
  varlista <- list(
  "Region" = region_vekt,
  "SNI2007" = sni2007_vekt,
  "ContentsCode" = cont_vekt,
  "Tid" = tid_koder)

  if (all(is.na(sni2007_klartext))) varlista <- varlista[names(varlista) != "SNI2007"]

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
            relocate(all_of(names(var_vektor)[varflytt_index]), .before = all_of(var_vektor_klartext[varflytt_index]))
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

}
