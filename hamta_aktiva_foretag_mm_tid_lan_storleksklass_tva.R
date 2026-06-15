hamta_aktiva_foretag_mm_tid_lan_storleksklass_variabel_tva <- function(
			tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2023"
			region_vekt = "20",			   # Val av region. Finns: "01", "03", "04", "05", "06", "07", "08", "09", "10", "12", "13", "14", "17", "18", "19", "20", "21", "22", "23", "24", "25"
			storleksklass_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "00 anställda", "01-09 anställda", "10+ anställda"
			variabel_klartext = "*",			 #  Finns: "Antal aktiva företag", "Antal nyetablerade företag", "Antal nedlagda företag", "Antal anställda", "Antal anställda i nyetablerade företag", "Antal anställda i nedlagda företag"
			output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
			excel_filnamn = ".xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
			returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
){

  # ====================================================================================================
  #
  # Funktion för att hämta data från Tillväxtanalys API med hjälp av pxweb-paketet
  # Automatgenererat av en funktion i R som skrivits av Peter Möller, Region Dalarna
  #
  # Skapad av: frkjon den 15 juni 2026
  # Senast uppdaterad: 15 juni 2026
  #
  # url till tabellens API: https://statistik.tillvaxtanalys.se:443/PxWeb/api/v1/sv/Tillväxtanalys statistikdatabas/Företagens demografi/Aktiva, nyetablerade och nedlagda företag/foretagens_demografi_aktiva_nyetablerade_nedlagda_foretag_lan_storleksklass.px
  #
  # ====================================================================================================

  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
    			tidyverse,
    			writexl)

  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

  # Url till databas
  url_uttag <- "https://statistik.tillvaxtanalys.se:443:443/PxWeb/api/v1/sv/Tillväxtanalys statistikdatabas/Företagens demografi/Aktiva, nyetablerade och nedlagda företag/foretagens_demografi_aktiva_nyetablerade_nedlagda_foretag_lan_storleksklass.px"
  px_meta <- pxweb_get(url_uttag)

  varlist_koder <- pxvarlist(px_meta)$koder
  varlist_bada <- pxvarlist(px_meta)

  # Gör om från klartext till kod som databasen förstår
  storleksklass_vekt <- if (!all(is.na(storleksklass_klartext))) hamta_kod_med_klartext(px_meta, storleksklass_klartext, skickad_fran_variabel = "storleksklass") else NA
  variabel_vekt <- hamta_kod_med_klartext(px_meta, variabel_klartext, skickad_fran_variabel = "variabel")

  # Hantera region-koder när regionkoderna ligger tillsammans med klartext i samma kolumn, och det är löpnummer istället för koder för län och kommuner
  region_vekt <- hamta_regionkod_med_knas_regionkod(px_meta, region_vekt, "län")           # konvertera korrekta läns- och kommunkoder till de löpnummer som används i denna databas

  # Hantera tid-koder
  giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "tid")
  tid_vekt <- if (all(tid_koder != "*")) tid_koder %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar] %>% unique() else giltiga_ar

  # query-lista till pxweb-uttag
  varlista <- list(
  	"Tid" = tid_vekt,
  	"Län" = region_vekt,
  	"Storleksklass" = storleksklass_vekt,
  	"variabel" = variabel_vekt)

  #if (all(is.na(lan_klartext))) varlista <- varlista[names(varlista) != "Län"]
  if (all(is.na(storleksklass_klartext))) varlista <- varlista[names(varlista) != "Storleksklass"]

  # Hämta data med varlista
  px_uttag <- pxweb_get(url = url_uttag, query = varlista)

  var_vektor <- NA
  var_vektor_klartext <- NA

  # gör om pxweb-uttaget till en dataframe
  px_df <- as.data.frame(px_uttag)
  px_df <- px_df %>% region_kolumn_splitta_kod_klartext("län")
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
