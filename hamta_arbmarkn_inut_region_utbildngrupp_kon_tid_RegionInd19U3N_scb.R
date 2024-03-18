hamta_arbmarkn_inut_region_utbildngrupp_kon_tid_scb <- function(
			region_vekt = "20",			# Val av region.
			utbildngrupp_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "samtliga utbildningsnivåer", "samtliga utbildningsinriktningar", "allmän utbildning", "samtliga utbildningsgrupper", "folkskoleutbildning och motsvarande utbildning", "grundskoleutbildning och motsvarande utbildning", "samhällsvetenskaplig el. humanistisk utbildning, gymnasial", "naturvetenskaplig utbildning, gymnasial nivå ", "teknisk utbildning, gymnasial nivå", "pedagogisk utbildning, gymnasial nivå ", "minst 30 hp inom pedagogik och lärarutbildning, ej examen ", "förskollärarutbildning ", "fritidspedagogutbildning ", "lärarutbildning för grundskolans tidigare år ", "lärarutbildning grsk senare år och gymn., allmänna + praktiskt-estet.", "speciallärar- och specialpedagogutbildning ", "yrkeslärarutbildning ", "övrig utbildning inom pedagogik / lärarutbildning, eftergymnasial", "estetisk utbildning, gymnasial nivå ", "utbildning inom humaniora och konst, gymnasial nivå ", "minst 30 hp inom humaniora och konst, ej examen ", "humanistisk utbildning, eftergymnasial nivå (minst 3 år) ", "konstnärlig utbildning, eftergymnasial nivå ", "utbildning inom medieproduktion, eftergymnasial nivå ", "teologisk utbildning, eftergymnasial nivå (minst 3 år) ", "övrig utbildning inom humaniora och konst, eftergymnasial nivå ", "ekonomisk utbildning, gymnasial nivå ", "handel- och administrationsutbildning, gymnasial nivå ", "minst 30 hp i samhällsvetenskap, juridik, handel, admin., ej examen ", "biblioteks- och informationsvetensk. högskoleutbildning (minst 3 år) ", "ekonomutbildning, högskoleutbildning (minst 3 år) ", "personal- och beteendevetarutbildning, högskoleutb. (minst 3 år) ", "juristutbildning ", "journalistik och medievetenskaplig utbildning, eftergymnasial nivå ", "psykologutbildning ", "samhällsvetar- och förvaltningsutb. högskoleutbildning (minst 3 år) ", "övrig utb. i samhällsvetenskap, juridik, handel, admin., eftergymnasial", "medicinsk sekreterarutbildning", "YH-utbildning i företagsekonomi, handel, administration", "yrkesinriktad utb. inom naturvetenskap, matematik, data, gymnasial nivå ", "minst 30 hp inom naturvetenskap, matematik, data, ej examen ", "biologutbildning, högskoleutbildning (minst 3 år) ", "datautbildning, eftergymnasial nivå ", "fysik- och  matematikutbildning, eftergymnasial nivå (minst 3år)", "geovetenskaplig utbildning, högskoleutbildning (minst 3 år) ", "kemistutbildning, högskoleutbildning (minst 3 år) ", "övrig naturvetenskaplig högskoleutbildning (minst 3 år) ", "övrig utbildning inom naturvetenskap, matematik, data, eftergymnasial", "gymnasieingenjörsutbildning", "byggutbildning, gymnasial nivå ", "data-, el- och energiteknisk utbildning, gymnasial nivå ", "fordonsutbildning, gymnasial nivå ", "industriutbildning, gymnasial nivå ", "vvs- och fastighetsutbildning, gymnasial nivå ", "övrig utbildning inom teknik och tillverkning, gymnasial nivå ", "minst 180 högskolepoäng inom teknik och tillverkning, ej examen", "30-179 högskolepoäng inom teknik och tillverkning, ej examen", "arkitektutbildning ", "civilingenjörsutbildning; industriell ekonomi", "civilingenjörsutbildning; väg- och vatten, byggnadsteknik, lantmäteri", "civilingenjörsutbildning; maskinteknik, fordons- och farkostteknik", "civilingenjörsutbildning; teknisk fysik, elektro- och datateknik", "civilingenjörsutbildning; kemi- och bioteknik, material- och geoteknik", "civilingenjörsutbildning; övrig/okänd inriktning", "högskoleingenjörsutb.; väg- och vatten, byggnadsteknik, lantmäteri", "högsk.ing.utb; maskinteknik, fordons- farkostteknik, industriell ekon.", "högskoleingenjörsutbildning; teknisk fysik, elektro- och datateknik", "högskoleingenjörsutb.; kemi- och bioteknik, material- och geoteknik", "högskoleingenjörsutbildning; övrig/okänd inriktning", "teknikutbildning, yrkeshögskolan", "övrig utbildning inom teknik och tillverkning, eftergymnasial nivå ", "naturbruksutbildning, gymnasial nivå ", "minst 30 hp inom lant- och skogsbruk, djursjukvård, ej examen ", "agronom- och hortonomutbildning ", "skogsvetenskaplig utbildning, högskoleutbildning (minst 3 år) ", "veterinärutbildning ", "övrig utb. inom lant- och skogsbruk, djursjukvård, eftergymnasial", "barn- och fritidsutbildning, gymnasial nivå ", "vård- och omsorgsutb.; övrig gymn. utb. i hälso- och sjukvård", "tandsköterskeutbildning ", "minst 30 hp inom hälso- och sjukvård, social omsorg, ej examen ", "apotekarutbildning ", "arbetsterapeututbildning ", "biomedicinsk analytikerutbildning ", "fritidsledarutbildning, eftergymnasial nivå ", "läkarutbildning (exkl. disputerade som saknar läkarexamen) ", "receptarieutbildning ", "sjukgymnast-/fysioterapeututbildning ", "barnmorskeutbildning ", "sjuksköterskeutbildning, grundutbildning ", "social omsorgsutbildning, eftergymnasial nivå ", "socionomutbildning ", "specialistsjuksköterskeutbildning", "tandhygienistutbildning ", "tandläkarutbildning ", "övrig utb. inom hälso- och sjukvård, social omsorg, eftergymnasial", "restaurang- och livsmedelsutbildning, gymnasial nivå ", "transportutbildning, gymnasial nivå ", "övrig utbildning inom tjänsteområdet, gymnasial nivå ", "minst 30 hp inom tjänsteområdet, ej examen ", "polisutbildning ", "transportutbildning, eftergymnasial nivå ", "övrig utbildning inom tjänsteområdet, eftergymnasial nivå ", "gymnasial utbildning, ospecificerad ", "eftergymnasial utbildning, ospecificerad ", "okänd utbildning ", "pedagogik och lärarutbildning", "förgymnasial utbildning", "humaniora och konst", "gymnasial utbildning", "samhällsvetenskap, juridik, handel, administration", "eftergymnasial utbildning, mindre än 3 år", "naturvetenskap, matematik och data", "teknik och tillverkning", "eftergymnasial utbildning, 3 år eller mer", "lant- och skogsbruk samt djursjukvård", "okänd utbildningsnivå", "hälso- och sjukvård samt social omsorg", "tjänster", "okänd utbildningsinriktning"
			kon_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "män", "kvinnor", "totalt"
			cont_klartext = "*",			 #  Finns: "Befolkning år 1, 18-64 år", "Befolkning år 2, 18-64 år", "Nettoförändring mellan år 1 och 2", "- Varav åldersinträden (18-åringar)", "- Varav åldersutträden (65-åringar)", "- Varav inflyttare till regionen", "- Varav utflyttare från regionen", "- Varav examinerade inom regionen", "- Varav vidareutbildade inom regionen"
			tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2006-2007", "2007-2008", "2008-2009", "2009-2010", "2010-2011", "2011-2012", "2012-2013", "2013-2014", "2014-2015", "2015-2016", "2016-2017", "2017-2018", "2018-2019", "2019-2020", "2020-2021"
			long_format = TRUE,			# TRUE = konvertera innehållsvariablerna i datasetet till long-format 
			wide_om_en_contvar = TRUE,			# TRUE = om man vill behålla wide-format om det bara finns en innehållsvariabel, FALSE om man vill konvertera till long-format även om det bara finns en innehållsvariabel
			output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
			excel_filnamn = "arbmarkn_inut.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
			returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
){

  # ====================================================================================================
  #
  # Funktion för att hämta data från SCB:s API med hjälp av pxweb-paketet
  # Automatgenererat av en funktion i R som skrivits av Peter Möller, Region Dalarna
  #
  # Skapad av: moepet den 18 mars 2024
  # Senast uppdaterad: 18 mars 2024
  #
  # url till tabellens API: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM9906__AM9906B/RegionInd19U3N/
  #
  # ====================================================================================================

if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
    			tidyverse,
    			writexl)

  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

  # Url till SCB:s databas
  url_uttag <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM9906/AM9906B/RegionInd19U3N"
  px_meta <- pxweb_get(url_uttag)

  varlist_koder <- pxvarlist(px_meta)$koder
  varlist_bada <- pxvarlist(px_meta)

  # Gör om från klartext till kod som databasen förstår
  utbildngrupp_vekt <- if (!all(is.na(utbildngrupp_klartext))) hamta_kod_med_klartext(px_meta, utbildngrupp_klartext, skickad_fran_variabel = "utbildngrupp") else NA
  kon_vekt <- if (!all(is.na(kon_klartext))) hamta_kod_med_klartext(px_meta, kon_klartext, skickad_fran_variabel = "kon") else NA


  cont_vekt <-  hamta_kod_med_klartext(px_meta, cont_klartext, "contentscode")
  if (length(cont_vekt) > 1) wide_om_en_contvar <- FALSE

  # Hantera tid-koder
  giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "tid")
  if (all(tid_koder != "*")) tid_koder <- tid_koder %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar] %>% unique()

# query-lista till pxweb-uttag
  varlista <- list(
  "Region" = region_vekt,
  "Utbildngrupp" = utbildngrupp_vekt,
  "Kon" = kon_vekt,
  "ContentsCode" = cont_vekt,
  "Tid" = tid_koder)

  if (all(is.na(utbildngrupp_klartext))) varlista <- varlista[names(varlista) != "Utbildngrupp"]
  if (all(is.na(kon_klartext))) varlista <- varlista[names(varlista) != "Kon"]

  px_uttag <- pxweb_get(url = url_uttag, query = varlista)

var_vektor <- c(regionkod = "Region")
var_vektor_klartext <- "region"
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
