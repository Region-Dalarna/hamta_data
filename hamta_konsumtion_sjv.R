hamta_konsumtion_sjv <- function(
			vara_klartext = "*",			 #  Finns: "Vetemjöl", "Rågmjöl", "Gryn och mjöl av ris", "Gryn och mjöl av havre och korn m.m.", "SUMMA MJÖL OCH GRYN", "Nötkött inklusive kalvkött, vara med ben", "Fårkött, vara med ben", "Hästkött, vara med ben", "Griskött, vara med ben", "Renkött, vara med ben", "Fjäderfäkött, urtagen vara", "Kött av vilt", "Inälvor", "SUMMA KÖTT", "Fisk, färsk och fryst", "Konserver och beredd fisk", "SUMMA FISK", "Konsumtionsmjölk, liter", "Syrade produkter, liter", "Tjock grädde", "Tunn grädde", "Ost (inkl. margarinost)", "Smör", "Hushållsmargarin och Bregott", "Lättmargarin", "Bagerimargarin", "Bageri- och matolja", "Kokosfett, friteringsfett m.m.", "Ägg", "Färskpotatis, inkl. skalad potatis", "Potatisinnehåll i förädlade produkter", "Matpotatis", "Socker och sirap (vitsockervärde)", "Kaffe, rostat", "Te", "Köksväxter, färska och frysta", "Köksväxter  beredda", "Frukter och bär, färska och frysta", "Frukter och bär, beredda"
			variabel_klartext = "*",			 #  Finns: "Totalt, 1 000 ton eller miljoner liter", "Kilo eller liter per person och år"
			år_klartext = "*",			 #  Finns: "1960", "1961", "1962", "1963", "1964", "1965", "1966", "1967", "1968", "1969", "1970", "1971", "1972", "1973", "1974", "1975", "1976", "1977", "1978", "1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022 prel."
			output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
			excel_filnamn = "jbv_konsumtion.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
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
  # url till tabellens API: https://statistik.sjv.se/PXWeb/api/v1/sv/Jordbruksverkets statistikdatabas/Konsumtion av livsmedel/JO1301K2.px
  #
  # ====================================================================================================

if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
    			tidyverse,
    			writexl)

  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

  # Url till SCB:s databas
  url_uttag <- "https://statistik.sjv.se/PXWeb/api/v1/sv/Jordbruksverkets statistikdatabas/Konsumtion av livsmedel/JO1301K2.px"
  px_meta <- pxweb_get(url_uttag)

  varlist_koder <- pxvarlist(px_meta)$koder
  varlist_bada <- pxvarlist(px_meta)

  # Gör om från klartext till kod som databasen förstår
  vara_vekt <- hamta_kod_med_klartext(px_meta, vara_klartext, skickad_fran_variabel = "vara")
  variabel_vekt <- hamta_kod_med_klartext(px_meta, variabel_klartext, skickad_fran_variabel = "variabel")
  år_vekt <- hamta_kod_med_klartext(px_meta, år_klartext, skickad_fran_variabel = "år")


# query-lista till pxweb-uttag
  varlista <- list(
  "Vara" = vara_vekt,
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
