hamta_skordar_lan_sjv <- function(
			län_klartext = "20 Dalarnas län",			 #  Finns: "00 Riket", "01 Stockholms län", "03 Uppsala län", "04 Södermanlands län", "05 Östergötlands län", "06 Jönköpings län", "07 Kronobergs län", "08 Kalmar län", "09 Gotlands län", "10 Blekinge län", "11 f.d. Kristianstads län", "12 Skåne län", "122 f.d. Malmöhus län", "13 Hallands län", "14 Västra Götalands län", "141 f.d. Göteborgs o Bohus län", "15 f.d. Älvsborgs län", "16 f.d. Skaraborgs län", "17 Värmlands län", "18 Örebro län", "19 Västmanlands län", "20 Dalarnas län", "21 Gävleborgs län", "22 Västernorrlands län", "23 Jämtlands län", "24 Västerbottens län", "25 Norrbottens län"
			gröda_klartext = c("Höstvete", "Vårvete", "Vårkorn", "Havre", "Matpotatis"),			 #  Finns: "Höstvete", "Vårvete", "Råg", "Höstkorn", "Vårkorn", "Havre", "Rågvete", "Höstrågvete", "Vårrågvete", "Blandsäd", "Majs", "Spannmål totalt", "Ärter", "Åkerbönor", "Höstraps", "Vårraps", "Höstrybs", "Vårrybs", "Stråsädesgrödor (exkl. majs) till grönfoder", "Majs till grönfoder", "Andra ettåriga grödor till grönfoder", "Oljelin", "Matpotatis", "Potatis för stärkelse", "Sockerbetor", "Slåttervall, första skörd", "Slåttervall, återväxt", "Slåttervall, total vallskörd"
			variabel_klartext = "Totalskörd, ton",			 #  Finns: "Hektarskörd, kg/hektar", "Hektarskörd, red. kg/hektar", "Areal, hektar", "Totalskörd, ton"
			tabelluppgift_klartext = "Värde",			 #  Finns: "Värde", "Medelfel, procent"
			år_klartext = c("2010":"2022"),			 #  Finns: "1965", "1966", "1967", "1968", "1969", "1970", "1971", "1972", "1973", "1974", "1975", "1976", "1977", "1978", "1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022"
			output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
			excel_filnamn = "jbv_skordar.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
			returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
){

  # ====================================================================================================
  #
  # Funktion för att hämta data från Jordbruksverkets API med hjälp av pxweb-paketet
  # Automatgenererat av en funktion i R som skrivits av Peter Möller, Region Dalarna
  #
  # Skapad av: moepet den 06 mars 2024
  # Senast uppdaterad: 06 mars 2024
  #
  # url till tabellens API: https://statistik.sjv.se/PXWeb/api/v1/sv/Jordbruksverkets statistikdatabas/Skordar/JO0601J01.px
  #
  # ====================================================================================================

if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
    			tidyverse,
    			writexl)

  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

  # Url till SCB:s databas
  url_uttag <- "https://statistik.sjv.se/PXWeb/api/v1/sv/Jordbruksverkets statistikdatabas/Skordar/JO0601J01.px"
  px_meta <- pxweb_get(url_uttag)

  varlist_koder <- pxvarlist(px_meta)$koder
  varlist_bada <- pxvarlist(px_meta)

  # Gör om från klartext till kod som databasen förstår
  län_vekt <- hamta_kod_med_klartext(px_meta, län_klartext, skickad_fran_variabel = "län")
  gröda_vekt <- hamta_kod_med_klartext(px_meta, gröda_klartext, skickad_fran_variabel = "gröda")
  variabel_vekt <- hamta_kod_med_klartext(px_meta, variabel_klartext, skickad_fran_variabel = "variabel")
  tabelluppgift_vekt <- hamta_kod_med_klartext(px_meta, tabelluppgift_klartext, skickad_fran_variabel = "tabelluppgift")
  år_vekt <- hamta_kod_med_klartext(px_meta, år_klartext, skickad_fran_variabel = "år")



# query-lista till pxweb-uttag
  varlista <- list(
  "Län" = län_vekt,
  "Gröda" = gröda_vekt,
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
