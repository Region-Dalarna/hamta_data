#test = hamta_konjunkturbarometern()
hamta_konjunkturbarometern <- function(
			indikator_klartext = "*",			 #  Finns: "Barometerindikatorn", "Totala näringslivet", "Tillverkningsindustri (SNI 10-33)", "Insatsvaruindustri", "Investeringsvaruindustri", "Konsumtionsvaruindustri", "Varaktiga konsumtionsvaror", "Icke varaktiga konsumtionsvaror", "Livsmedelsindustri (SNI 10-12)", "Textilindustri (SNI 13-15)", "Trävaruindustri (SNI 16)", "Sågverk (SNI 16.1)", "Övrig trävaruindustri (SNI 16.2)", "Massa- & pappersindustri (SNI 17)", "Massaindustri (SNI 17.11)", "Pappersindustri (SNI 17.12)", "Grafisk industri (SNI 18)", "Petroleumindustri (SNI 19)", "Kemisk industri (SNI 20)", "Läkemedelsindustri (SNI 21)", "Gummi- & plastindustri (SNI 22)", "Jord- & stenvaruindustri (SNI 23)", "Stål- & metallindustri (SNI 24)", "Järn- & stålverk (SNI 24.1-3)", "Metallvaruindustri (SNI 25)", "Elektronikindustri (SNI 26)", "Industri för elapparatur (SNI 27)", "Maskinindustri (SNI 28)", "Motorfordonsindustri (SNI 29)", "Annan transportmedelsindustri (SNI 30)", "Möbelindustri (SNI 31)", "Övrig tillverkningsindustri (SNI 32)", "Reparationsverkstäder (SNI 33)", "Byggindustri (SNI 41-43)", "Bygg & anläggning (SNI 41-42)", "Husbyggande (SNI 41)", "Anläggningsverksamhet (SNI 42)", "Specialiserad byggverksamhet (SNI 43)", "Handel (SNI 45-47)", "Detaljhandel (SNI 45 + 47)", "Handel med motorfordon (SNI 45)", "Partihandel (SNI 46)", "Partihandel med jordbruksvaror (SNI 46.2)", "Partihandel med livsmedel (SNI 46.3)", "Partihandel med hushållsvaror (SNI 46.4)", "Partihandel med  IT-utrustning (SNI 46.5)", "Partihandel med övriga maskiner (SNI 46.6)", "Annan partihandel (SNI 46.7)", "Specialiserad butikshandel (SNI 47.19 + 47.3-9)", "Sällanköpshandel (SNI 47.19+47.4-47.9)", "Dagligvaruhandel (SNI 47.11 + 47.2)", "Detaljhandel med brett sortiment  (SNI 47.19)", "Detaljhandel med drivmedel (SNI 47.3)", "Elektronikhandel (SNI 47.4)", "Heminredningsbutiker (SNI 47.5)", "Butiker för kultur- & fritidsartiklar (SNI 47.6)", "Butiker för övriga hushållsvaror (SNI 47.7)", "Internethandel (SNI 47.9)", "Tjänstesektorn (SNI 49-82 + 95-96)", "Uppdragsverksamhet", "Landtransport (SNI 49)", "Åkerier (SNI 49.4)", "Hotell & restauranger (SNI 55.1 + 56.1-2)", "Hotell (SNI 55.1)", "Restauranger (SNI 56.1-2)", "Förlag (SNI 58)", "Datakonsulter (SNI 62)", "Finans- & försäkringsverksamhet (SNI 64-66)", "Förvaltare av egna fastigheter (68.2)", "Fastighetsmäklare & -förvaltare (SNI 68.3)", "Juridiska & ekonomiska konsulter (SNI 69)", "Konsulter företagsledning (SNI 70)", "Arkitekter & tekniska konsulter (SNI 71.1)", "Arkitekter (SNI 71.11)", "Tekniska konsulter (SNI 71.12)", "Reklam & marknadsundersökning (SNI 73)", "Uthyrnings- & leasingföretag (SNI 77)", "Arbetsförmedling, bemanning mm (SNI 78)", "Personaluthyrning (SNI 78.2-3)", "Researrangörer & -byråer (SNI 79)", "Andra företagstjänster (SNI 82)", "Konfidensindikator hushåll", "Makroindex hushåll", "Mikroindex hushåll"
			period_klartext = "*",			 #  Finns: "1996M01", "1996M02", "1996M03", "1996M04", "1996M05", "1996M06", "1996M07", "1996M08", "1996M09", "1996M10", "1996M11", "1996M12", "1997M01", "1997M02", "1997M03", "1997M04", "1997M05", "1997M06", "1997M07", "1997M08", "1997M09", "1997M10", "1997M11", "1997M12", "1998M01", "1998M02", "1998M03", "1998M04", "1998M05", "1998M06", "1998M07", "1998M08", "1998M09", "1998M10", "1998M11", "1998M12", "1999M01", "1999M02", "1999M03", "1999M04", "1999M05", "1999M06", "1999M07", "1999M08", "1999M09", "1999M10", "1999M11", "1999M12", "2000M01", "2000M02", "2000M03", "2000M04", "2000M05", "2000M06", "2000M07", "2000M08", "2000M09", "2000M10", "2000M11", "2000M12", "2001M01", "2001M02", "2001M03", "2001M04", "2001M05", "2001M06", "2001M07", "2001M08", "2001M09", "2001M10", "2001M11", "2001M12", "2002M01", "2002M02", "2002M03", "2002M04", "2002M05", "2002M06", "2002M07", "2002M08", "2002M09", "2002M10", "2002M11", "2002M12", "2003M01", "2003M02", "2003M03", "2003M04", "2003M05", "2003M06", "2003M07", "2003M08", "2003M09", "2003M10", "2003M11", "2003M12", "2004M01", "2004M02", "2004M03", "2004M04", "2004M05", "2004M06", "2004M07", "2004M08", "2004M09", "2004M10", "2004M11", "2004M12", "2005M01", "2005M02", "2005M03", "2005M04", "2005M05", "2005M06", "2005M07", "2005M08", "2005M09", "2005M10", "2005M11", "2005M12", "2006M01", "2006M02", "2006M03", "2006M04", "2006M05", "2006M06", "2006M07", "2006M08", "2006M09", "2006M10", "2006M11", "2006M12", "2007M01", "2007M02", "2007M03", "2007M04", "2007M05", "2007M06", "2007M07", "2007M08", "2007M09", "2007M10", "2007M11", "2007M12", "2008M01", "2008M02", "2008M03", "2008M04", "2008M05", "2008M06", "2008M07", "2008M08", "2008M09", "2008M10", "2008M11", "2008M12", "2009M01", "2009M02", "2009M03", "2009M04", "2009M05", "2009M06", "2009M07", "2009M08", "2009M09", "2009M10", "2009M11", "2009M12", "2010M01", "2010M02", "2010M03", "2010M04", "2010M05", "2010M06", "2010M07", "2010M08", "2010M09", "2010M10", "2010M11", "2010M12", "2011M01", "2011M02", "2011M03", "2011M04", "2011M05", "2011M06", "2011M07", "2011M08", "2011M09", "2011M10", "2011M11", "2011M12", "2012M01", "2012M02", "2012M03", "2012M04", "2012M05", "2012M06", "2012M07", "2012M08", "2012M09", "2012M10", "2012M11", "2012M12", "2013M01", "2013M02", "2013M03", "2013M04", "2013M05", "2013M06", "2013M07", "2013M08", "2013M09", "2013M10", "2013M11", "2013M12", "2014M01", "2014M02", "2014M03", "2014M04", "2014M05", "2014M06", "2014M07", "2014M08", "2014M09", "2014M10", "2014M11", "2014M12", "2015M01", "2015M02", "2015M03", "2015M04", "2015M05", "2015M06", "2015M07", "2015M08", "2015M09", "2015M10", "2015M11", "2015M12", "2016M01", "2016M02", "2016M03", "2016M04", "2016M05", "2016M06", "2016M07", "2016M08", "2016M09", "2016M10", "2016M11", "2016M12", "2017M01", "2017M02", "2017M03", "2017M04", "2017M05", "2017M06", "2017M07", "2017M08", "2017M09", "2017M10", "2017M11", "2017M12", "2018M01", "2018M02", "2018M03", "2018M04", "2018M05", "2018M06", "2018M07", "2018M08", "2018M09", "2018M10", "2018M11", "2018M12", "2019M01", "2019M02", "2019M03", "2019M04", "2019M05", "2019M06", "2019M07", "2019M08", "2019M09", "2019M10", "2019M11", "2019M12", "2020M01", "2020M02", "2020M03", "2020M04", "2020M05", "2020M06", "2020M07", "2020M08", "2020M09", "2020M10", "2020M11", "2020M12", "2021M01", "2021M02", "2021M03", "2021M04", "2021M05", "2021M06", "2021M07", "2021M08", "2021M09", "2021M10", "2021M11", "2021M12", "2022M01", "2022M02", "2022M03", "2022M04", "2022M05", "2022M06", "2022M07", "2022M08", "2022M09", "2022M10", "2022M11", "2022M12", "2023M01", "2023M02", "2023M03", "2023M04", "2023M05", "2023M06", "2023M07", "2023M08", "2023M09", "2023M10", "2023M11", "2023M12", "2024M01", "2024M02", "2024M03"
			output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
			excel_filnamn = "konjunkturbarometern.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
			returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
){

  # ====================================================================================================
  #
  # Funktion för att hämta data från Konjunkturinstitutets API med hjälp av pxweb-paketet
  # Automatgenererat av en funktion i R som skrivits av Peter Möller, Region Dalarna
  #
  # Skapad av: frkjon den 12 april 2024
  # Senast uppdaterad: 12 april 2024
  #
  # url till tabellens API: https://statistik.konj.se/PXWeb/api/v1/sv/KonjBar/indikatorer/Indikatorm.px
  #
  # ====================================================================================================

  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
    			tidyverse,
    			writexl)

  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

  # Url till SCB:s databas
  url_uttag <- "https://statistik.konj.se/PXWeb/api/v1/sv/KonjBar/indikatorer/Indikatorm.px"
  px_meta <- pxweb_get(url_uttag)

  varlist_koder <- pxvarlist(px_meta)$koder
  varlist_bada <- pxvarlist(px_meta)

  # Gör om från klartext till kod som databasen förstår
  indikator_vekt <- hamta_kod_med_klartext(px_meta, indikator_klartext, skickad_fran_variabel = "indikator")
  period_vekt <- hamta_kod_med_klartext(px_meta, period_klartext, skickad_fran_variabel = "period")

  cont_vekt <-  hamta_kod_med_klartext(px_meta, cont_klartext, "contentscode")
  if (length(cont_vekt) > 1) wide_om_en_contvar <- FALSE

  # Hantera tid-koder
  giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "period")
  if (all(period_vekt != "*")) period_vekt <- period_vekt %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar] %>% unique()

  # query-lista till pxweb-uttag
  varlista <- list(
  "Indikator" = indikator_vekt,
  "Period" = period_vekt)



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
