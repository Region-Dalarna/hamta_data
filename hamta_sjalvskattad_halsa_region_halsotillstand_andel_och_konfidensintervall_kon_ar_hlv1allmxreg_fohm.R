hamta_sjalvskattad_halsa_region_halsotillstand_andel_och_konfidensintervall_kon_ar_fohm <- function(
			region_vekt = "20",			   # Val av region. Finns: "00", "01", "03", "04", "05", "06", "07", "08", "09", "10", "12", "13", "14", "17", "18", "19", "20", "21", "22", "23", "24", "25"
			halsotillstand_klartext = "*",			 #  Finns: "Bra eller mycket bra hälsa", "Dålig eller mycket dålig hälsa", "Långvarig sjukdom"
			andel_och_konfidensintervall_klartext = "*",			 #  Finns: "Andel", "Konfidensintervall nedre gräns", "Konfidensintervall övre gräns", "Antal svar"
			kon_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "Totalt", "Kvinnor", "Män"
			tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2004-2007", "2005-2008", "2006-2009", "2007-2010", "2008-2011", "2009-2012", "2010-2013", "2011-2014", "2012-2015", "2013-2016", "2015-2018", "2017-2020", "2018-2021", "2019-2022"
			output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
			excel_filnamn = "sjalvskattad_halsa.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
			returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
){

  # ====================================================================================================
  #
  # Funktion för att hämta data från Folkhälsomyndighetens API med hjälp av pxweb-paketet
  # Automatgenererat av en funktion i R som skrivits av Peter Möller, Region Dalarna
  #
  # Skapad av: moepet den 20 oktober 2024
  # Senast uppdaterad: 20 oktober 2024
  #
  # url till tabellens API: http://fohm-app.folkhalsomyndigheten.se/Folkhalsodata/pxweb/sv/A_Folkhalsodata/A_Folkhalsodata__B_HLV__bFyshals__bbaFyshalsallman/hlv1allmxreg.px/
  #
  # ====================================================================================================

  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
    			tidyverse,
    			writexl)

  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

  # Url till SCB:s databas
  url_uttag <- "http://fohm-app.folkhalsomyndigheten.se/Folkhalsodata/api/v1/sv/A_Folkhalsodata/B_HLV/bFyshals/bbaFyshalsallman/hlv1allmxreg.px"
  px_meta <- pxweb_get(url_uttag)

  varlist_koder <- pxvarlist(px_meta)$koder
  varlist_bada <- pxvarlist(px_meta)

  # Gör om från klartext till kod som databasen förstår
  halsotillstand_vekt <- hamta_kod_med_klartext(px_meta, halsotillstand_klartext, skickad_fran_variabel = "hälsotillstånd")
  andel_och_konfidensintervall_vekt <- hamta_kod_med_klartext(px_meta, andel_och_konfidensintervall_klartext, skickad_fran_variabel = "andel och konfidensintervall")
  kon_vekt <- if (!all(is.na(kon_klartext))) hamta_kod_med_klartext(px_meta, kon_klartext, skickad_fran_variabel = "kön") else NA

  # Hantera tid-koder
  px_meta$variables <- sortera_px_variabler(px_meta$variables, sorterings_vars = "År", sortera_pa_kod = FALSE)        # sortera om månader så att de kommer i kronologisk ordning
  tid_koder <- tid_koder %>%           # ersätt "9999" med senaste år
     str_replace_all("9999", hamta_giltiga_varden_fran_tabell(px_meta, "År", klartext = TRUE) %>% max())
  giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "År")

  if (all(tid_koder == "*")) {
      tid_vekt <- giltiga_ar
  } else {
     tid_vekt <- map(tid_koder, function(period) {
        if (str_detect(period, ":")){     # kontrollera om det finns ett kolon = intervall
           intervall <- map_chr(str_split(period, ":") %>% unlist(), ~ hamta_kod_med_klartext(px_meta, .x, "År"))
           retur_txt <- giltiga_ar[which(giltiga_ar == intervall[1]):which(giltiga_ar == intervall[2])]
        } else retur_txt <- hamta_kod_med_klartext(px_meta, period, "År")
     }) %>% unlist()
     index_period <- map_lgl(px_meta$variables, ~ .x$text == "År")          # hitta platsen i px_meta$variables där variabeln "År" finns
     period_varden <- px_meta$variables[[which(index_period)]]$values         # läs in alla värden för variabeln "År"
    tid_vekt <- tid_vekt[match(period_varden[period_varden %in% tid_vekt], tid_vekt)]        # sortera om tid_vekt utifrån ordningen i px_meta (som vi sorterade ovan) 
   }

  # query-lista till pxweb-uttag
  varlista <- list(
  	"Region" = region_vekt,
  	"Hälsotillstånd" = halsotillstand_vekt,
  	"Andel och konfidensintervall" = andel_och_konfidensintervall_vekt,
  	"Kön" = kon_vekt,
  	"År" = tid_vekt)

  if (all(is.na(kon_klartext))) varlista <- varlista[names(varlista) != "Kön"]

  # Hämta data med varlista
  px_uttag <- pxweb_get(url = url_uttag, query = varlista)

  var_vektor <- c(regionkod = "Region")
  var_vektor_klartext <- "Region"

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

  # speciallösning för Folkhälsomyndigheten där vi tar bort regionkoder som ligger i klartextkolumnen
  px_df <- region_kolumn_splitta_kod_klartext(px_df, "Region")

  # Om användaren vill spara data till en Excel-fil
  if (!is.na(output_mapp) & !is.na(excel_filnamn)){
    write.xlsx(px_df, paste0(output_mapp, excel_filnamn))
  }

  # Returnera data som en dataframe om användern valt det
  if (returnera_df) return(px_df)
} # slut hämta data-funktion
