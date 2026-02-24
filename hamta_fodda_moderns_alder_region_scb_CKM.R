hamta_fodda_moderns_alder_region_scb_CKM <- function(
			region_vekt = "20",			   # Val av region. Finns: t.ex. "00", "01", "0114", "0115", "03", "04", "05", "06", "07", "08", "09", "10", "12", "13", "14", "17", "18", "19", "20", "21", "22", "23", "24", "25", "2583", "2584"
			aldermoder_klartext = "*",			 #  Finns: t.ex. "totalt, samtliga åldrar", "-14 år", "15 år", "15-19 år", "15-24 år", "16 år", "17 år", "18 år", "19 år", "20 år", "48 år", "49+ år", "uppgift saknas"
			kon_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "totalt, samtliga män och kvinnor", "män", "kvinnor"
			cont_klartext = "*",			 #  Finns: "Antal"
			tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2025"
			output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
			excel_filnamn = "fodda_SCB_CKM.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
			returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
){

  # ====================================================================================================
  #
  # Funktion för att hämta data från SCB:s API med hjälp av pxweb-paketet
  # Automatgenererat av en funktion i R som skrivits av Peter Möller, Region Dalarna
  #
  # Skapad av: frkjon den 24 februari 2026
  # Senast uppdaterad: 24 februari 2026
  #
  # url till tabellens API: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__BE__BE0101__BE0101H/FoddaKCKM/
  #
  # ====================================================================================================

  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
    			tidyverse,
    			writexl)

  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

  # Url till databas
  url_uttag <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101H/FoddaKCKM"
  px_meta <- pxweb_get(url_uttag)

  varlist_koder <- pxvarlist(px_meta)$koder
  varlist_bada <- pxvarlist(px_meta)

  # Gör om från klartext till kod som databasen förstår
  kon_vekt <- if (!all(is.na(kon_klartext))) hamta_kod_med_klartext(px_meta, kon_klartext, skickad_fran_variabel = "kon") else NA
  aldermoder_vekt <- hamta_kod_med_klartext(px_meta, aldermoder_klartext, skickad_fran_variabel = "aldermoder")

  cont_vekt <-  hamta_kod_med_klartext(px_meta, cont_klartext, "contentscode")
  if (length(cont_vekt) > 1) wide_om_en_contvar <- FALSE

  # Hantera tid-koder
  giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "tid")
  tid_vekt <- if (all(tid_koder != "*")) tid_koder %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar] %>% unique() else giltiga_ar

  # query-lista till pxweb-uttag
  varlista <- list(
  	"Region" = region_vekt,
  	"AlderModer" = aldermoder_vekt,
  	"Kon" = kon_vekt,
  	"ContentsCode" = cont_vekt,
  	"Tid" = tid_vekt)

  if (all(is.na(kon_klartext))) varlista <- varlista[names(varlista) != "Kon"]

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

  # Av oklar anledning blir det dubbelräkning. Har lagt till en distinct här
  px_df <- px_df %>% distinct()
  # Returnera data som en dataframe om användern valt det
  if (returnera_df) return(px_df)
} # slut hämta data-funktion
