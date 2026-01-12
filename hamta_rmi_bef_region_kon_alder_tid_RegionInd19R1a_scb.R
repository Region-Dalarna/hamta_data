hamta_rmi_bef_region_kon_alder_tid_scb <- function(
			region_vekt = "20",			   # Val av region. Finns: t.ex. "00", "FA00", "FA01", "FA59", "FA60", "01", "03", "04", "05", "06", "07", "08", "09", "10", "12", "13", "14", "17", "18", "19", "20", "21", "22", "23", "24", "25"
			kon_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "män", "kvinnor", "totalt"
			alder_koder = "*",			 #  NA = tas inte med i uttaget,  Finns: t.ex. "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "98", "99", "100+", "tot"
			cont_klartext = "*",			 #  Finns: "Folkmängd"
			tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024"
			output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
			excel_filnamn = "rmi_bef.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
			returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
){

  # ====================================================================================================
  #
  # Funktion för att hämta data från SCB:s API med hjälp av pxweb-paketet
  # Automatgenererat av en funktion i R som skrivits av Peter Möller, Region Dalarna
  #
  # Skapad av: moepet den 12 januari 2026
  # Senast uppdaterad: 12 januari 2026
  #
  # url till tabellens API: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM9906__AM9906D/RegionInd19R1a/
  #
  # ====================================================================================================

  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
    			tidyverse,
    			writexl)

  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

  # Url till databas
  url_uttag <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM9906/AM9906D/RegionInd19R1a"
  px_meta <- pxweb_get(url_uttag)

  varlist_koder <- pxvarlist(px_meta)$koder
  varlist_bada <- pxvarlist(px_meta)

  # Gör om från klartext till kod som databasen förstår
  kon_vekt <- if (!all(is.na(kon_klartext))) hamta_kod_med_klartext(px_meta, kon_klartext, skickad_fran_variabel = "kon") else NA

  alder_vekt <- if (all(!is.na(alder_koder))) alder_koder %>% as.character() %>% ifelse(. == "100", "-100+", .) %>% ifelse(. == "tot", "totalt ålder", .) else NA
  cont_vekt <-  hamta_kod_med_klartext(px_meta, cont_klartext, "contentscode")
  if (length(cont_vekt) > 1) wide_om_en_contvar <- FALSE

  # Hantera tid-koder
  giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "tid")
  tid_vekt <- if (all(tid_koder != "*")) tid_koder %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar] %>% unique() else giltiga_ar

  # query-lista till pxweb-uttag
  varlista <- list(
  	"Region" = region_vekt,
  	"Kon" = kon_vekt,
  	"Alder" = alder_vekt,
  	"ContentsCode" = cont_vekt,
  	"Tid" = tid_vekt)

  if (all(is.na(kon_klartext))) varlista <- varlista[names(varlista) != "Kon"]
  if (all(is.na(alder_koder))) varlista <- varlista[names(varlista) != "Alder"]

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

  # Om användaren vill spara data till en Excel-fil
  if (!is.na(output_mapp) & !is.na(excel_filnamn)){
    write.xlsx(px_df, paste0(output_mapp, excel_filnamn))
  }

  # Returnera data som en dataframe om användern valt det
  if (returnera_df) return(px_df)
} # slut hämta data-funktion
