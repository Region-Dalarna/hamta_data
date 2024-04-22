#test <- hamta__region_sni2007_foretagsform_tid_scb()
hamta__region_sni2007_foretagsform_tid_scb <- function(
			region_vekt = "20",			# Val av region.
			sni2007_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "jordbruksföretag och serviceföretag till jordbruk", "skogsbruksföretag", "fiskare och vattenbrukare ", "gruvor och mineralutvinningsindustri", "livsmedels-, dryckesvaru- och tobaksindustri", "textil-, beklädnads-, läder - och lädervaruindustri", "industri för trä och varor av trä, kork och rotting o.d. utom möbler", "massa-, pappers- och pappersvaruindustri", "grafisk och annan reproduktionsindustri", "kemisk industri, petroleumprodukter och läkemedelsindustri", "gummi- och plastvaruindustri", "industri för andra icke-metalliska mineraliska produkter", "stål- och metallverk", "industri för metallvaror utom maskiner och apparater", "industri för datorer, elektronikvaror, optik och elapparatur", "övrig maskinindustri", "transportmedelsindustri", "övrig tillverkningsindustri, reparationsverkstäder och installationsföretag", "el-, gas- och värmeverk; vatten- och reningsverk; anläggningar för avfallshantering, återvinning och sanering", "byggindustri", "handel med och serviceverkstäder för motorfordon och motorcyklar", "parti - och provisionshandel utom med motorfordon", "detaljhandel utom med motorfordon och motorcyklar", "transport- och magasineringsföretag", "hotell och restauranger", "informations- och kommunikationsföretag", "kreditinstitut och försäkringsbolag m.m.", "fastighetsbolag och fastighetsförvaltare", "företag inom juridik, ekonomi, vetenskap och teknik", "uthyrningsfirmor, arbetsförmedlingar, rekryteringsföretag, personaluthyrningsföretag o.d.", "resebyråer och researrangörer; turistbyråer", "bevaknings-, fastighetsservice och kontorstjänstföretag m.m.", "utbildningsväsendet", "enheter för vård och omsorg, socialtjänst", "enheter för kultur nöje och fritid", "andra serviceföretag och företag för personliga tjänster, civila myndigheter och försvaret", "uppgift saknas"
			foretagsform_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "aktiebolag", "enskild firma", "handelsbolag m.m."
			cont_klartext = "*",			 #  Finns: "Anställda drabbade av konkurser"
			tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2009M01", "2009M02", "2009M03", "2009M04", "2009M05", "2009M06", "2009M07", "2009M08", "2009M09", "2009M10", "2009M11", "2009M12", "2010M01", "2010M02", "2010M03", "2010M04", "2010M05", "2010M06", "2010M07", "2010M08", "2010M09", "2010M10", "2010M11", "2010M12", "2011M01", "2011M02", "2011M03", "2011M04", "2011M05", "2011M06", "2011M07", "2011M08", "2011M09", "2011M10", "2011M11", "2011M12", "2012M01", "2012M02", "2012M03", "2012M04", "2012M05", "2012M06", "2012M07", "2012M08", "2012M09", "2012M10", "2012M11", "2012M12", "2013M01", "2013M02", "2013M03", "2013M04", "2013M05", "2013M06", "2013M07", "2013M08", "2013M09", "2013M10", "2013M11", "2013M12", "2014M01", "2014M02", "2014M03", "2014M04", "2014M05", "2014M06", "2014M07", "2014M08", "2014M09", "2014M10", "2014M11", "2014M12", "2015M01", "2015M02", "2015M03", "2015M04", "2015M05", "2015M06", "2015M07", "2015M08", "2015M09", "2015M10", "2015M11", "2015M12", "2016M01", "2016M02", "2016M03", "2016M04", "2016M05", "2016M06", "2016M07", "2016M08", "2016M09", "2016M10", "2016M11", "2016M12", "2017M01", "2017M02", "2017M03", "2017M04", "2017M05", "2017M06", "2017M07", "2017M08", "2017M09", "2017M10", "2017M11", "2017M12", "2018M01", "2018M02", "2018M03", "2018M04", "2018M05", "2018M06", "2018M07", "2018M08", "2018M09", "2018M10", "2018M11", "2018M12", "2019M01", "2019M02", "2019M03", "2019M04", "2019M05", "2019M06", "2019M07", "2019M08", "2019M09", "2019M10", "2019M11", "2019M12", "2020M01", "2020M02", "2020M03", "2020M04", "2020M05", "2020M06", "2020M07", "2020M08", "2020M09", "2020M10", "2020M11", "2020M12", "2021M01", "2021M02", "2021M03", "2021M04", "2021M05", "2021M06", "2021M07", "2021M08", "2021M09", "2021M10", "2021M11", "2021M12", "2022M01", "2022M02", "2022M03", "2022M04", "2022M05", "2022M06", "2022M07", "2022M08", "2022M09", "2022M10", "2022M11", "2022M12", "2023M01", "2023M02", "2023M03", "2023M04", "2023M05", "2023M06", "2023M07", "2023M08", "2023M09", "2023M10", "2023M11", "2023M12", "2024M01", "2024M02", "2024M03"
			output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
			excel_filnamn = ".xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
			returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
){

  # ====================================================================================================
  #
  # Funktion för att hämta data från SCB:s API med hjälp av pxweb-paketet
  # Automatgenererat av en funktion i R som skrivits av Peter Möller, Region Dalarna
  #
  # Skapad av: frkjon den 15 april 2024
  # Senast uppdaterad: 15 april 2024
  #
  # url till tabellens API: https://api.scb.se/OV0104/v1/doris/sv/ssd/START/NV/NV1401/NV1401A/KonkurserAnst07
  #
  # ====================================================================================================

  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
    			tidyverse,
    			writexl)

  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

  # Url till SCB:s databas
  url_uttag <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/NV/NV1401/NV1401A/KonkurserAnst07"
  px_meta <- pxweb_get(url_uttag)

  varlist_koder <- pxvarlist(px_meta)$koder
  varlist_bada <- pxvarlist(px_meta)

  # Gör om från klartext till kod som databasen förstår
  sni2007_vekt <- if (!all(is.na(sni2007_klartext))) hamta_kod_med_klartext(px_meta, sni2007_klartext, skickad_fran_variabel = "sni2007") else NA
  foretagsform_vekt <- if (!all(is.na(foretagsform_klartext))) hamta_kod_med_klartext(px_meta, foretagsform_klartext, skickad_fran_variabel = "foretagsform") else NA

  cont_vekt <-  hamta_kod_med_klartext(px_meta, cont_klartext, "contentscode")
  if (length(cont_vekt) > 1) wide_om_en_contvar <- FALSE

  # Hantera tid-koder
  giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "tid")
  if (all(tid_koder != "*")) tid_koder <- tid_koder %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar] %>% unique()

  # query-lista till pxweb-uttag
  varlista <- list(
  "Region" = region_vekt,
  "SNI2007" = sni2007_vekt,
  "Foretagsform" = foretagsform_vekt,
  "ContentsCode" = cont_vekt,
  "Tid" = tid_koder)

  if (all(is.na(sni2007_klartext))) varlista <- varlista[names(varlista) != "SNI2007"]
  if (all(is.na(foretagsform_klartext))) varlista <- varlista[names(varlista) != "Foretagsform"]

  px_uttag <- pxweb_get(url = url_uttag, query = varlista)

  var_vektor <- c(regionkod = "Region")
  var_vektor_klartext <- "region"

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
