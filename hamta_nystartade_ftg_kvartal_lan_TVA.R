hamta_nystartadeforetag_kvartal_lan_TVA <- function(
    kvartal_klartext = "*",			 #  Finns: "2024Q2", "2024Q1", "2023Q4", "2023Q3", "2023Q2", "2023Q1", "2022Q4", "2022Q3", "2022Q2", "2022Q1", "2021Q4", "2021Q3", "2021Q2", "2021Q1", "2020Q4", "2020Q3", "2020Q2", "2020Q1", "2019Q4", "2019Q3", "2019Q2", "2019Q1", "2018Q4", "2018Q3", "2018Q2", "2018Q1", "2017Q4", "2017Q3", "2017Q2", "2017Q1", "2016Q4", "2016Q3", "2016Q2", "2016Q1", "2015Q4", "2015Q3", "2015Q2", "2015Q1", "2014Q4", "2014Q3", "2014Q2", "2014Q1", "2013Q4", "2013Q3", "2013Q2", "2013Q1", "2012Q4", "2012Q3", "2012Q2", "2012Q1", "2011Q4", "2011Q3", "2011Q2", "2011Q1"
    län_klartext = "*",			 #  Finns: "01 Stockholms län", "03 Uppsala län", "04 Södermanlands län", "05 Östergötlands län", "06 Jönköpings län", "07 Kronobergs län", "08 Kalmars län", "09 Gotlands län", "10 Blekinges län", "12 Skåne län", "13 Hallands län", "14 Västra Götalands län", "17 Värmlands län", "18 Örebro län", "19 Västmanlands län", "20 Dalarnas län", "21 Gävleborgs län", "22 Västernorrlands län", "23 Jämtlands län", "24 Västerbottens län", "25 Norrbottens län"
    variabel_klartext = "*",			 #  Finns: "antal nystartade företag"
    output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
    excel_filnamn = "nystartade_ftg.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
    returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
){
  
  # ====================================================================================================
  #
  # Funktion för att hämta data från Tillväxtanalys API med hjälp av pxweb-paketet
  # Automatgenererat av en funktion i R som skrivits av Peter Möller, Region Dalarna
  #
  # Skapad av: frkjon den 27 september 2024
  # Senast uppdaterad: 27 september 2024
  #
  # url till tabellens API: https://statistik.tillvaxtanalys.se:443/PxWeb/api/v1/sv/Tillväxtanalys statistikdatabas/Nystartade företag/nyaf_kvartal_lan_2011.px
  # url till källa: https://statistik.tillvaxtanalys.se/PxWeb/pxweb/sv/Tillv%C3%A4xtanalys%20statistikdatabas/
  # ====================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
         tidyverse,
         writexl)
  
  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  # Url till SCB:s databas
  url_uttag <- "https://statistik.tillvaxtanalys.se:443/PxWeb/api/v1/sv/Tillväxtanalys statistikdatabas/Nystartade företag/nyaf_kvartal_lan_2011.px"
  px_meta <- pxweb_get(url_uttag)
  
  # Gör om från klartext till kod som databasen förstår
  län_vekt <- hamta_kod_med_klartext(px_meta, län_klartext, skickad_fran_variabel = "län")
  variabel_vekt <- hamta_kod_med_klartext(px_meta, variabel_klartext, skickad_fran_variabel = "variabel")
  
  # Hanterar tiden
  if(all(kvartal_klartext == "9999")){
    kvartal_vekt <- min(hamta_kod_med_klartext(px_meta, "*", skickad_fran_variabel = "kvartal"))
  } else{
    kvartal_vekt <- hamta_kod_med_klartext(px_meta, kvartal_klartext, skickad_fran_variabel = "kvartal")
  }
  
  # query-lista till pxweb-uttag
  varlista <- list(
    "kvartal" = kvartal_vekt,
    "län" = län_vekt,
    "variabel" = variabel_vekt)
  
  px_uttag <- pxweb_get(url = url_uttag, query = varlista)
  
  var_vektor <- NA
  var_vektor_klartext <- NA
  
  px_df <- as.data.frame(px_uttag)
  
  # Om användaren vill spara data till en Excel-fil
  if (!is.na(output_mapp) & !is.na(excel_filnamn)){
    write.xlsx(px_df, paste0(output_mapp, excel_filnamn))
  }
  
  # Returnera data som en dataframe om användern valt det
  if (returnera_df) return(px_df)
  
}
