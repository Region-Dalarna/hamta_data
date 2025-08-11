hamta_yh_stud_exam_kon_utbildninriktn_regionutb_utbtakt_utbformyh_utblangd_utbexam_tid_scb <- function(
			kon_klartext = "*",			 #  Finns: "totalt", "kvinnor", "män"
			utbildninriktn_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "Totalt", "Data/It", "Ekonomi, administration och försäljning", "Friskvård och kroppsvård", "Hotell, restaurang och turism", "Hälso- och sjukvård samt socialt arbete", "Journalistik och information", "Juridik", "Kultur, media och design", "Lantbruk, djurvård, trädgård, skog och fiske", "Pedagogik och undervisning", "Samhällsbyggnad och byggteknik", "Säkerhetstjänster", "Teknik och tillverkning", "Transporttjänster", "Övrigt", "Totalt", "Pedagogik och lärarutbildning", "Humaniora och konst", "Samhällsvetenskap, juridik, handel, administration", "Naturvetenskap, matematik och informations- och kommunikationsteknik (IKT)", "Teknik och tillverkning", "Lant- och skogsbruk samt djursjukvård", "Hälso- och sjukvård samt social omsorg", "Tjänster", "Okänt", "Totalt"
			regionutb_vekt = "20",			# Val av region.
			utbtakt_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "Totalt", "Heltid", "Deltid"
			utbformyh_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "Totalt", "Bunden", "Distans"
			utblangd_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "Totalt", "Minst 0,5 år mindre än 1 år", "Minst 1 år mindre än 1,5 år", "Minst 1,5 år mindre än 2 år", "Minst 2 år mindre än 2,5 år", "Minst 2,5 år mindre än 3 år", "Minst 3 år maximalt 3,5 år"
			utbexam_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "Totalt", "Ingen examen (YH)", "Yrkeshögskoleexamen (YH)", "Kvalificerad yrkeshögskoleexamen (YH)", "Kvalificerad yrkesexamen (KY)"
			cont_klartext = "*",			 #  Finns: "Antal antagna som påbörjat studier", "Antal studerande", "Antal examinerade", "Examensgrad"
			tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023"
			long_format = TRUE,			# TRUE = konvertera innehållsvariablerna i datasetet till long-format 
			wide_om_en_contvar = TRUE,			# TRUE = om man vill behålla wide-format om det bara finns en innehållsvariabel, FALSE om man vill konvertera till long-format även om det bara finns en innehållsvariabel
			output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
			excel_filnamn = "yh_stud_exam.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
			returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
){

  # ====================================================================================================
  #
  # Funktion för att hämta data från SCB:s API med hjälp av pxweb-paketet
  # Automatgenererat av en funktion i R som skrivits av Peter Möller, Region Dalarna
  #
  # Skapad av: moepet den 21 mars 2024
  # Senast uppdaterad: 21 mars 2024
  #
  # url till tabellens API: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__UF__UF0701__UF0701B/YHStudT1dN/
  #
  # ====================================================================================================

if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
    			tidyverse,
    			writexl)

  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

  # Url till SCB:s databas
  url_uttag <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/UF/UF0701/UF0701B/YHStudT1dN"
  px_meta <- pxweb_get(url_uttag)

  varlist_koder <- pxvarlist(px_meta)$koder
  varlist_bada <- pxvarlist(px_meta)

  # Gör om från klartext till kod som databasen förstår
  kon_vekt <- hamta_kod_med_klartext(px_meta, kon_klartext, skickad_fran_variabel = "kon")
  utbildninriktn_vekt <- if (!all(is.na(utbildninriktn_klartext))) hamta_kod_med_klartext(px_meta, utbildninriktn_klartext, skickad_fran_variabel = "utbildninriktn") else NA
  utbtakt_vekt <- if (!all(is.na(utbtakt_klartext))) hamta_kod_med_klartext(px_meta, utbtakt_klartext, skickad_fran_variabel = "utbtakt") else NA
  utbformyh_vekt <- if (!all(is.na(utbformyh_klartext))) hamta_kod_med_klartext(px_meta, utbformyh_klartext, skickad_fran_variabel = "utbformyh") else NA
  utblangd_vekt <- if (!all(is.na(utblangd_klartext))) hamta_kod_med_klartext(px_meta, utblangd_klartext, skickad_fran_variabel = "utblangd") else NA
  utbexam_vekt <- if (!all(is.na(utbexam_klartext))) hamta_kod_med_klartext(px_meta, utbexam_klartext, skickad_fran_variabel = "utbexam") else NA


  cont_vekt <-  hamta_kod_med_klartext(px_meta, cont_klartext, "contentscode")
  if (length(cont_vekt) > 1) wide_om_en_contvar <- FALSE

  # Hantera tid-koder
  giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "tid")
  if (all(tid_koder != "*")) tid_koder <- tid_koder %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar] %>% unique()

# query-lista till pxweb-uttag
  varlista <- list(
  "Kon" = kon_vekt,
  "UtbildnInriktn" = utbildninriktn_vekt,
  "RegionUtb" = regionutb_vekt,
  "UtbTakt" = utbtakt_vekt,
  "UtbFormYH" = utbformyh_vekt,
  "UtbLangd" = utblangd_vekt,
  "UtbExam" = utbexam_vekt,
  "ContentsCode" = cont_vekt,
  "Tid" = tid_koder)

  if (all(is.na(utbildninriktn_klartext))) varlista <- varlista[names(varlista) != "UtbildnInriktn"]
  if (all(is.na(regionutb_vekt))) varlista <- varlista[names(varlista) != "RegionUtb"]
  if (all(is.na(utbtakt_klartext))) varlista <- varlista[names(varlista) != "UtbTakt"]
  if (all(is.na(utbformyh_klartext))) varlista <- varlista[names(varlista) != "UtbFormYH"]
  if (all(is.na(utblangd_klartext))) varlista <- varlista[names(varlista) != "UtbLangd"]
  if (all(is.na(utbexam_klartext))) varlista <- varlista[names(varlista) != "UtbExam"]

  px_uttag <- pxweb_get(url = url_uttag, query = varlista)

var_vektor <- c(regionutbkod = "RegionUtb")
var_vektor_klartext <- "region där utbildningen bedrivs"
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
