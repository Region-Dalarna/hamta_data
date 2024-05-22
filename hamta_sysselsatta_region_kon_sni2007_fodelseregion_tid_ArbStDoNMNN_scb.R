hamta_sysselsatta_region_kon_sni2007_fodelseregion_tid_scb <- function(
			region_vekt = "20",			# Val av region.
			kon_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "män", "kvinnor", "totalt"
			sni2007_klartext = "*",			 #  Finns: "Total", "företag inom jordbruk, skogsbruk och fiske", "tillverkningsindustri; gruvor och mineralutvinningsindustri", "företag inom energi och miljö", "byggindustri", "handel; serviceverkstäder för motorfordon och motorcyklar", "transport- och magasineringsföretag", "hotell och restauranger", "informations- och kommunikationsföretag", "kreditinstitut och försäkringsbolag m.m.", "fastighetsbolag och fastighetsförvaltare", "företag inom juridik, ekonomi, vetenskap och teknik; företag inom uthyrning, fastighetsservice, resetjänster och andra stödtjänster", "civila myndigheter och försvaret", "utbildningsväsendet", "enheter för vård och omsorg, socialtjänst", "enheter för kultur, nöje och fritid; andra serviceföretag m.m.", "uppgift saknas"
			fodelseregion_klartext = "*",			#  NA = tas inte med i uttaget,  Finns: "inrikes född", "utrikes född", "totalt"
			cont_klartext = "*",			 #  Finns: "sysselsatta efter arbetsställets belägenhet", "sysselsatta efter bostadens belägenhet"
			tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2020M01", "2020M02", "2020M03", "2020M04", "2020M05", "2020M06", "2020M07", "2020M08", "2020M09", "2020M10", "2020M11", "2020M12", "2021M01", "2021M02", "2021M03", "2021M04", "2021M05", "2021M06", "2021M07", "2021M08", "2021M09", "2021M10", "2021M11", "2021M12", "2022M01", "2022M02", "2022M03", "2022M04", "2022M05", "2022M06", "2022M07", "2022M08", "2022M09", "2022M10", "2022M11", "2022M12", "2023M01", "2023M02", "2023M03", "2023M04", "2023M05", "2023M06", "2023M07", "2023M08", "2023M09", "2023M10", "2023M11", "2023M12", "2024M01", "2024M02"
			long_format = TRUE,			# TRUE = konvertera innehållsvariablerna i datasetet till long-format 
			wide_om_en_contvar = TRUE,			# TRUE = om man vill behålla wide-format om det bara finns en innehållsvariabel, FALSE om man vill konvertera till long-format även om det bara finns en innehållsvariabel
			output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
			excel_filnamn = "sysselsatta.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
			returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
){

  # ====================================================================================================
  #
  # Funktion för att hämta data från SCB:s API med hjälp av pxweb-paketet
  # Automatgenererat av en funktion i R som skrivits av Peter Möller, Region Dalarna
  #
  # Skapad av: frkjon den 02 maj 2024
  # Senast uppdaterad: 02 maj 2024
  #
  # url till tabellens API: https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0210/AM0210B/ArbStDoNMNN
  #
  # ====================================================================================================

  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
    			tidyverse,
    			writexl)

  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  # Hämtar nycklar för branscher
  branschtabell <- read.csv("G:/skript/nycklar/Bransch_Gxx_farger.csv", sep = ";", encoding = "latin1")      # hämta nyckel för branscher        
  

  # Url till SCB:s databas
  url_uttag <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0210/AM0210B/ArbStDoNMNN"
  px_meta <- pxweb_get(url_uttag)

  varlist_koder <- pxvarlist(px_meta)$koder
  varlist_bada <- pxvarlist(px_meta)

  # Gör om från klartext till kod som databasen förstår
  kon_vekt <- if (!all(is.na(kon_klartext))) hamta_kod_med_klartext(px_meta, kon_klartext, skickad_fran_variabel = "kon") else NA
  sni2007_vekt <- hamta_kod_med_klartext(px_meta, sni2007_klartext, skickad_fran_variabel = "sni2007")
  fodelseregion_vekt <- if (!all(is.na(fodelseregion_klartext))) hamta_kod_med_klartext(px_meta, fodelseregion_klartext, skickad_fran_variabel = "fodelseregion") else NA

  cont_vekt <-  hamta_kod_med_klartext(px_meta, cont_klartext, "contentscode")
  if (length(cont_vekt) > 1) wide_om_en_contvar <- FALSE

  # Hantera tid-koder
  giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "tid")
  if (all(tid_koder != "*")) tid_koder <- tid_koder %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar] %>% unique()

  # query-lista till pxweb-uttag
  varlista <- list(
  "Region" = region_vekt,
  "Kon" = kon_vekt,
  "SNI2007" = sni2007_vekt,
  "Fodelseregion" = fodelseregion_vekt,
  "ContentsCode" = cont_vekt,
  "Tid" = tid_koder)

  if (all(is.na(kon_klartext))) varlista <- varlista[names(varlista) != "Kon"]
  if (all(is.na(fodelseregion_klartext))) varlista <- varlista[names(varlista) != "Fodelseregion"]

  px_uttag <- pxweb_get(url = url_uttag, query = varlista)

  var_vektor <- c(regionkod = "Region",SNI2007 ="SNI2007")
  #var_vektor_klartext <- c("region", "näringsgren SNI 2007")

  px_df <- as.data.frame(px_uttag)
  if (all(!is.na(var_vektor))) {      px_df <- px_df %>%
            cbind(as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>%
            select(all_of(var_vektor)))

     # px_df <- map2(names(var_vektor), var_vektor_klartext, ~ px_df %>% relocate(all_of(.x), .before = all_of(.y))) %>% list_rbind()
  }

  # man kan välja bort long-format, då låter vi kolumnerna vara wide om det finns fler innehållsvariabler, annars
  # pivoterar vi om till long-format, dock ej om det bara finns en innehållsvariabel
  if (long_format & !wide_om_en_contvar) px_df <- px_df %>% konvertera_till_long_for_contentscode_variabler(url_uttag)
  
  # Binder ihop med våra branscher
  px_df <- px_df %>% 
    rename("branschkod" = "SNI2007") %>%
      relocate(branschkod, .before = `näringsgren SNI 2007`) %>%
        relocate(regionkod,.before = region) %>% 
        mutate(branschkod = ifelse(branschkod == "US", "00", branschkod)) %>%
          filter(branschkod != "A-U+US") %>% 
            left_join(branschtabell %>% select(Br15kod, BrKod, HexCode, bransch = Bransch), by = c("branschkod" = "Br15kod")) %>% 
              select(-`näringsgren SNI 2007`) %>% 
                relocate(branschkod, .after = region) %>% 
                  relocate(bransch, .after = branschkod)

  # Om användaren vill spara data till en Excel-fil
  if (!is.na(output_mapp) & !is.na(excel_filnamn)){
    write.xlsx(px_df, paste0(output_mapp, excel_filnamn))
  }

  # Returnera data som en dataframe om användern valt det
  if (returnera_df) return(px_df)

}
