hamta_nystartade_kvartal_lan_variabel_tva <- function(
    kvartal_klartext = "*",			 #  Finns: "2024Q2", "2024Q1", "2023Q4", "2023Q3", "2023Q2", "2023Q1", "2022Q4", "2022Q3", "2022Q2", "2022Q1", "2021Q4", "2021Q3", "2021Q2", "2021Q1", "2020Q4", "2020Q3", "2020Q2", "2020Q1", "2019Q4", "2019Q3", "2019Q2", "2019Q1", "2018Q4", "2018Q3", "2018Q2", "2018Q1", "2017Q4", "2017Q3", "2017Q2", "2017Q1", "2016Q4", "2016Q3", "2016Q2", "2016Q1", "2015Q4", "2015Q3", "2015Q2", "2015Q1", "2014Q4", "2014Q3", "2014Q2", "2014Q1", "2013Q4", "2013Q3", "2013Q2", "2013Q1", "2012Q4", "2012Q3", "2012Q2", "2012Q1", "2011Q4", "2011Q3", "2011Q2", "2011Q1"
    region_vekt = "20",			   # Val av region. Finns: "01", "03", "04", "05", "06", "07", "08", "09", "10", "12", "13", "14", "17", "18", "19", "20", "21", "22", "23", "24", "25"
    variabel_klartext = "*",			 #  Finns: "antal nystartade företag"
    output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
    excel_filnamn = "nystartade.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
    returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
){
  
  # ====================================================================================================
  #
  # Funktion för att hämta data från Tillväxtanalys API med hjälp av pxweb-paketet
  # Automatgenererat av en funktion i R som skrivits av Peter Möller, Region Dalarna
  #
  # Skapad av: frkjon den 17 oktober 2024
  # Senast uppdaterad: 17 oktober 2024
  #
  # url till tabellens API: https://statistik.tillvaxtanalys.se:443/PxWeb/api/v1/sv/Tillväxtanalys statistikdatabas/Nystartade företag/nyaf_kvartal_lan_2011.px
  #
  # Tillväxtanalys hade bland annat ändrat url, rättat
  # ====================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
         tidyverse,
         writexl)
  
  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  # Url till SCB:s databas
  url_uttag <- "https://statistik.tillvaxtanalys.se:443/PxWeb/api/v1/sv/Tillväxtanalys statistikdatabas/Nystartade företag/nystartade_foretag_kvartal_lan.px"
  px_meta <- pxweb_get(url_uttag)
  
  varlist_koder <- pxvarlist(px_meta)$koder
  varlist_bada <- pxvarlist(px_meta)
  
  # Gör om från klartext till kod som databasen förstår
  kvartal_vekt <- hamta_kod_med_klartext(px_meta, kvartal_klartext, skickad_fran_variabel = "kvartal")
  variabel_vekt <- hamta_kod_med_klartext(px_meta, variabel_klartext, skickad_fran_variabel = "variabel")
  
  # Hantera region-koder när regionkoderna ligger tillsammans med klartext i samma kolumn, och det är löpnummer istället för koder för län och kommuner
  region_vekt <- hamta_regionkod_med_knas_regionkod(px_meta, region_vekt, "län")           # konvertera korrekta läns- och kommunkoder till de löpnummer som används i denna databas
  
  # query-lista till pxweb-uttag
  varlista <- list(
    "Tid" = kvartal_vekt,
    "Län" = region_vekt,
    "variabel" = variabel_vekt)
  
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
