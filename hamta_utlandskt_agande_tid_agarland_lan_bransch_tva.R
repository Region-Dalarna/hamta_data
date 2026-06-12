hamta_utlandskt_agande_tid_agarland_lan_bransch_tva <- function(
			tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2022", "2023"
			agarland_vekt = "*",			   # Val av region. Finns: t.ex. "ae", "at", "vn", "za". Tyvärr är det inte möjligt att skriva klartext
			region_vekt = "20",			   # Val av region. Finns: "01", "03", "04", "05", "06", "07", "08", "09", "10", "12", "13", "14", "17", "18", "19", "20", "21", "22", "23", "24", "25", "99"
			bransch_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "A Jordbruk, skogsbruk och fiske", "B Utvinning av mineral", "C Tillverkning", "D Försörjning av el, gas, värme och kyla", "E Vattenförsörjning; avloppsrening, avfallshantering och sanering", "F Byggverksamhet", "G Handel; reparation av motorfordon och motorcyklar", "H Transport och magasinering", "I Hotell- och restaurangverksamhet", "J Informations- och kommunikationsverksamhet", "K Finans- och försäkringsverksamhet", "L Fastighetsverksamhet", "M Verksamhet inom juridik, ekonomi, vetenskap och teknik", "N Uthyrning, fastighetsservice, resetjänster och andra stödtjänster", "O Offentlig förvaltning och försvar; obligatorisk socialförsäkring", "P Utbildning", "Q Vård och omsorg; sociala tjänster", "R Kultur, nöje och fritid", "S Annan serviceverksamhet", "Bransch ospecificerad"
			variabel_klartext = "*",			 #  Finns: "Antal arbetsställen", "Antal anställda"
			output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
			excel_filnamn = "utlandskt_agande.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
			returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
){

  # ====================================================================================================
  #
  # Funktion för att hämta data från Tillväxtanalys API med hjälp av pxweb-paketet
  # Automatgenererat av en funktion i R som skrivits av Peter Möller, Region Dalarna
  #
  # Skapad av: frkjon den 11 juni 2026
  # Senast uppdaterad: 11 juni 2026
  #
  # url till tabellens API: https://statistik.tillvaxtanalys.se:443/PxWeb/api/v1/sv/Tillväxtanalys statistikdatabas/Utlandsägda företag/internationella_foretag_utlandsagda_arbetsstallen_land_region_bransch_2022_2023.px
  #
  # ====================================================================================================

  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
    			tidyverse,
    			writexl)

  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

  # Url till databas
  url_uttag <- "https://statistik.tillvaxtanalys.se:443:443/PxWeb/api/v1/sv/Tillväxtanalys statistikdatabas/Utlandsägda företag/internationella_foretag_utlandsagda_arbetsstallen_land_region_bransch_2022_2023.px"
  px_meta <- pxweb_get(url_uttag)

  varlist_koder <- pxvarlist(px_meta)$koder
  varlist_bada <- pxvarlist(px_meta)

  # Gör om från klartext till kod som databasen förstår
  bransch_vekt <- if (!all(is.na(bransch_klartext))) hamta_kod_med_klartext(px_meta, bransch_klartext, skickad_fran_variabel = "bransch") else NA
  variabel_vekt <- hamta_kod_med_klartext(px_meta, variabel_klartext, skickad_fran_variabel = "variabel")

  # Hantera region-koder när regionkoderna ligger tillsammans med klartext i samma kolumn, och det är löpnummer istället för koder för län och kommuner
  region_vekt <- hamta_regionkod_med_knas_regionkod(px_meta, region_vekt, "Län")           # konvertera korrekta läns- och kommunkoder till de löpnummer som används i denna databas

  # Hantera tid-koder
  giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "tid")
  tid_vekt <- if (all(tid_koder != "*")) tid_koder %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar] %>% unique() else giltiga_ar

  # query-lista till pxweb-uttag
  varlista <- list(
  	"Tid" = tid_vekt,
  	"Ägarland" = agarland_vekt,
  	"Län" = region_vekt,
  	"Bransch" = bransch_vekt,
  	"variabel" = variabel_vekt)

  if (all(is.na(bransch_klartext))) varlista <- varlista[names(varlista) != "Bransch"]

  # Hämta data med varlista
  px_uttag <- pxweb_get(url = url_uttag, query = varlista)


  # gör om pxweb-uttaget till en dataframe
  px_df <- as.data.frame(px_uttag)
  px_df <- px_df %>% region_kolumn_splitta_kod_klartext("län")

  # Om användaren vill spara data till en Excel-fil
  if (!is.na(output_mapp) & !is.na(excel_filnamn)){
    write.xlsx(px_df, paste0(output_mapp, excel_filnamn))
  }

  # Returnera data som en dataframe om användern valt det
  if (returnera_df) return(px_df)
} # slut hämta data-funktion
