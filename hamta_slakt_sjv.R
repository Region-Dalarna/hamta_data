hamta_slakt_sjv <- function(
			län_klartext = "20 Dalarnas län",			 #  Finns: "00 Riket", "01 Stockholms län", "03 Uppsala län", "04 Södermanlands län", "05 Östergötlands län", "06 Jönköpings län", "07 Kronobergs län", "08 Kalmar län", "09 Gotlands län", "10 Blekinge län", "12 Skåne län", "13 Hallands län", "14 Västra Götalands län", "17 Värmlands län", "18 Örebro län", "19 Västmanlands län", "20 Dalarnas län", "21 Gävleborgs län", "22 Västernorrlands län", "23 Jämtlands län", "24 Västerbottens län", "25 Norrbottens län", "Okänd"
			djurslag_klartext = "*",			 #  Finns: "Mellankalv", "Summa kalv", "Stut", "Äldre tjur", "Ungtjur", "Yngre tjur", "Summa tjur", "Kviga", "Ko", "Ungko", "Summa ko", "Summa storboskap", "Summa nötkreatur", "Slaktgris", "Suggor", "Galtar", "Unggris", "Unggalt", "Summa gris", "Häst", "Tackor och baggar", "Lamm", "Summa får"
			variabel_klartext = "Kvantitet, 1 000 ton",			 #  Finns: "Slaktade hela kroppar, 1 000-tal", "Kvantitet, 1 000 ton", "Antal företag"
			år_klartext = "*",			 #  Finns: "2016", "2017", "2018", "2019", "2020", "2021", "2022"
			output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
			excel_filnamn = "jbv_slakt.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
			returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
){

  # ====================================================================================================
  #
  # Funktion för att hämta data från SCB:s API med hjälp av pxweb-paketet
  # Automatgenererat av en funktion i R som skrivits av Peter Möller, Region Dalarna
  #
  # Skapad av: moepet den 06 mars 2024
  # Senast uppdaterad: 06 mars 2024
  #
  # url till tabellens API: https://statistik.sjv.se/PXWeb/api/v1/sv/Jordbruksverkets statistikdatabas/Animalieproduktion/Slakt/JO0604B1.px
  #
  # ====================================================================================================

if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
    			tidyverse,
    			writexl)

  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

  # Url till SCB:s databas
  url_uttag <- "https://statistik.sjv.se/PXWeb/api/v1/sv/Jordbruksverkets statistikdatabas/Animalieproduktion/Slakt/JO0604B1.px"
  px_meta <- pxweb_get(url_uttag)

  varlist_koder <- pxvarlist(px_meta)$koder
  varlist_bada <- pxvarlist(px_meta)

  # Gör om från klartext till kod som databasen förstår
  län_vekt <- hamta_kod_med_klartext(px_meta, län_klartext, skickad_fran_variabel = "län")
  djurslag_vekt <- hamta_kod_med_klartext(px_meta, djurslag_klartext, skickad_fran_variabel = "djurslag")
  variabel_vekt <- hamta_kod_med_klartext(px_meta, variabel_klartext, skickad_fran_variabel = "variabel")
  år_vekt <- hamta_kod_med_klartext(px_meta, år_klartext, skickad_fran_variabel = "år")


# query-lista till pxweb-uttagdata:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAWElEQVR42mNgGPTAxsZmJsVqQApgmGw1yApwKcQiT7phRBuCzzCSDSHGMKINIeDNmWQlA2IigKJwIssQkHdINgxfmBBtGDEBS3KCxBc7pMQgMYE5c/AXPwAwSX4lV3pTWwAAAABJRU5ErkJggg==
  varlista <- list(
  "Län" = län_vekt,
  "Djurslag" = djurslag_vekt,
  "Variabel" = variabel_vekt,
  "År" = år_vekt)

  px_uttag <- pxweb_get(url = url_uttag, query = varlista)

  var_vektor <- NA
  var_vektor_klartext <- NA
  
  px_df <- as.data.frame(px_uttag)
  if (!is.na(var_vektor)) {      px_df <- px_df %>%
            cbind(as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>%
            select(any_of(var_vektor)))

      px_df <- map2(names(var_vektor), var_vektor_klartext, ~ px_df %>% relocate(all_of(.x), .before = all_of(.y))) %>% list_rbind()
  }

  # Om användaren vill spara data till en Excel-fil
  if (!is.na(output_mapp) & !is.na(excel_filnamn)){
    write.xlsx(px_df, paste0(output_mapp, excel_filnamn))
  }

  # Returnera data som en dataframe om användern valt det
  if (returnera_df) return(px_df)

}
