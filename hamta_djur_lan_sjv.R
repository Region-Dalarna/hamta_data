hamta_djur_lan_jbv <- function(
			län_klartext = c("00 Riket", "20 Dalarnas län"),			 #  Finns: "00 Riket", "01 Stockholms län", "03 Uppsala län", "04 Södermanlands län", "05 Östergötlands län", "06 Jönköpings län", "07 Kronobergs län", "08 Kalmar län", "09 Gotlands län", "10 Blekinge län", "12 Skåne län", "11 f.d. Kristianstads län", "122 f.d. Malmöhus län", "13 Hallands län", "14 Västra Götalands län", "141 f.d. Göteborgs- och Bohus län", "15 f.d. Älvsborgs län", "16 f.d. Skaraborgs län", "17 Värmlands län", "18 Örebro län", "19 Västmanlands län", "20 Dalarnas län", "21 Gävleborgs län", "22 Västernorrlands län", "23 Jämtlands län", "24 Västerbottens län", "25 Norrbottens län"
			kategori_klartext = "Kor för mjölkproduktion",			 #  Finns: "Kor för mjölkproduktion", "Kor för uppfödning av kalvar", "Summa kor", "Kvigor, tjurar och stutar", "Kalvar, under 1 år", "Summa nötkreatur", "Baggar och tackor", "Lamm", "Summa får", "Galtar", "Suggor, inklusive gyltor", "Summa galtar och suggor", "Slaktgrisar", "Smågrisar", "Summa grisar", "Höns", "Värpkycklingar", "Slaktkycklingar", "Slaktkycklingar, normal uppfödningsomgång", "Kalkoner", "Hästar", "Mjölkkor på företag med 1-24 mjölkkor", "Mjölkkor på företag med 25-49 mjölkkor", "Mjölkkor på företag med 50-74 mjölkkor", "Mjölkkor på företag med 75-99 mjölkkor", "Mjölkkor på företag med 100-199 mjölkkor", "Mjölkkor på företag med över 199 mjölkkor", "Baggar och tackor på företag med 1-9 baggar och tackor", "Baggar och tackor på företag med 10-24 baggar och tackor", "Baggar och tackor på företag med 25-49 baggar och tackor", "Baggar och tackor på företag med över 49 baggar och tackor", "Avelsgrisar på företag med 1-49 avelsgrisar", "Avelsgrisar på företag med 50-99 avelsgrisar", "Avelsgrisar på företag med 100-199 avelsgrisar", "Avelsgrisar på företag med 200-499 avelsgrisar", "Avelsgrisar på företag med över 499 avelsgrisar", "Slaktgrisar på företag med 1-99 slaktgrisar", "Slaktgrisar på företag med 100-249 slaktgrisar", "Slaktgrisar på företag med 250-499 slaktgrisar", "Slaktgrisar på företag med 500-749 slaktgrisar", "Slaktgrisar på företag med 750-1999 slaktgrisar", "Slaktgrisar på företag med över 1999 slaktgrisar", "Höns på företag med 1-49 höns", "Höns på företag med 50-199 höns", "Höns på företag med 200-4999 höns", "Höns på företag med över 4999 höns"
			variabel_klartext = "Antal djur",			 #  Finns: "Antal djur", "Antal företag"
			tabelluppgift_klartext = "Värde",			 #  Finns: "Värde", "Medelfel, procent"
			år_klartext = "*",			 #  Finns: "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023"
			output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
			excel_filnamn = "jbv_mjolkkor.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
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
  # url till tabellens API: https://statistik.sjv.se/PXWeb/api/v1/sv/Jordbruksverkets statistikdatabas/Lantbrukets djur/Lantbruksdjur i juni/JO0103F01.px
  #
  # ====================================================================================================

if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
    			tidyverse,
    			writexl)

  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

  # Url till SCB:s databas
  url_uttag <- "https://statistik.sjv.se/PXWeb/api/v1/sv/Jordbruksverkets statistikdatabas/Lantbrukets djur/Lantbruksdjur i juni/JO0103F01.px"
  px_meta <- pxweb_get(url_uttag)

  varlist_koder <- pxvarlist(px_meta)$koder
  varlist_bada <- pxvarlist(px_meta)

  # Gör om från klartext till kod som databasen förstår
  län_vekt <- hamta_kod_med_klartext(px_meta, län_klartext, skickad_fran_variabel = "län")
  kategori_vekt <- hamta_kod_med_klartext(px_meta, kategori_klartext, skickad_fran_variabel = "kategori")
  variabel_vekt <- hamta_kod_med_klartext(px_meta, variabel_klartext, skickad_fran_variabel = "variabel")
  tabelluppgift_vekt <- hamta_kod_med_klartext(px_meta, tabelluppgift_klartext, skickad_fran_variabel = "tabelluppgift")
  år_vekt <- hamta_kod_med_klartext(px_meta, år_klartext, skickad_fran_variabel = "år")


# query-lista till pxweb-uttag
  varlista <- list(
  "Län" = län_vekt,
  "Kategori" = kategori_vekt,
  "Variabel" = variabel_vekt,
  "Tabelluppgift" = tabelluppgift_vekt,
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
