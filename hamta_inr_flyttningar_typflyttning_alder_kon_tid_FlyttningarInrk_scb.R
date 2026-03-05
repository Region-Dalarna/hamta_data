hamta_inr_flyttningar_typflyttning_alder_kon_tid_scb <- function(
			typflyttning_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "Inom fastighet", "Mellan fastigheter inom distrikt", "Mellan distrikt inom kommun", "Mellan kommuner inom län", "Över länsgräns"
			alder_koder = "*",			 #  NA = tas inte med i uttaget,  Finns: "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "43", "44", "45", "46", "47", "48", "49", "50", "51", "52", "53", "54", "55", "56", "57", "58", "59", "60", "61", "62", "63", "64", "65", "66", "67", "68", "69", "70", "71", "72", "73", "74", "75", "76", "77", "78", "79", "80", "81", "82", "83", "84", "85", "86", "87", "88", "89", "90", "91", "92", "93", "94", "95", "96", "97", "98", "99", "100+", "tot"
			kon_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "män", "kvinnor"
			cont_klartext = "*",			 #  Finns: "Antal"
			tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024"
			output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
			excel_filnamn = "inr_flyttningar.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
			returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
){

  # ====================================================================================================
  #
  # Funktion för att hämta data från SCB:s API med hjälp av pxweb-paketet
  # Automatgenererat av en funktion i R som skrivits av Peter Möller, Region Dalarna
  #
  # Skapad av: moepet den 28 april 2025
  # Senast uppdaterad: 28 april 2025
  #
  # url till tabellens API: https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101J/FlyttningarInrk
  #
  # ====================================================================================================

  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
    			tidyverse,
    			writexl)

  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

  # Url till databas
  url_list <- c("https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101J/FlyttningarInrk",
                "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101J/FlyttningarInrkCKM")
  
  giltig_tid <- map(url_list, ~ hamta_giltiga_varden_fran_tabell(.x, "tid")) %>% unlist() %>% unique()      # hämta alla giltiga år som finns i alla medskickade tabeller
  senaste_ar <- giltig_tid %>% max()      # hämta senaste år som finns i alla medskickade tabeller
  hamta_tid <- if (all(tid_koder == "*")) hamta_tid <- giltig_tid else {
    tid_koder <- tid_koder %>% str_replace("9999", senaste_ar)
    hamta_tid <- tid_koder[tid_koder %in% giltig_tid]
  }
  
  hamta_data <- function(url_uttag) {
      
      px_meta <- pxweb_get(url_uttag)
      ar_ckm <- str_detect(tolower(url_uttag), "ckm")
      
      varlist_koder <- pxvarlist(px_meta)$koder
      varlist_bada <- pxvarlist(px_meta)
      
      # hantering av ålder
      alder_koder <- alder_koder %>% as.character()
      if (!all(is.na(alder_koder))) {
        alder_giltiga_varden <- hamta_giltiga_varden_fran_tabell(px_meta, "alder") %>% 
          .[!str_detect(., "-")] %>% 
          .[!(str_detect(tolower(.), "tot") & . != "TOT1") | tolower(.) == "tot"] %>% 
          .[!(str_detect(tolower(.), "100") & . != "100+1") | . == "100+"] %>% 
          as.character()      # hämta giltiga värden för ålder och gör om till character
        # om det finns fler total ålder-grupper, behåller vi bara en
        if (sum(str_count(tolower(alder_giltiga_varden), "tot")) > 1) {
          tot_traffar <- alder_giltiga_varden[str_detect(tolower(alder_giltiga_varden), "tot")]
          tot_tabort <- tot_traffar[2:length(tot_traffar)]
          alder_giltiga_varden <- alder_giltiga_varden %>%
            .[!. %in% tot_tabort]
        }
        hundra_giltig <- alder_giltiga_varden[str_detect(alder_giltiga_varden, "100")][1]
        alder_hamta <- if (all(alder_koder == "*")) alder_giltiga_varden else alder_koder %>% str_replace("100", hundra_giltig) %>% str_replace("totalt", "TOT1") %>% .[. %in% alder_giltiga_varden]
        if (ar_ckm) {
          alder_hamta <- alder_hamta %>%                             # byt ut till gitliga koder om man skickat med 100 eller totalt 
            as.character() %>%                                       # säkerställ att alder_koder är character och inte numeric
            .[. %in% alder_giltiga_varden]
        } else alder_hamta <- alder_hamta[alder_hamta %in% alder_giltiga_varden]
      } else {
        alder_hamta <- if (ar_ckm) "TOT1" else NA
      }
    
    # Gör om från klartext till kod som databasen förstår
    typflyttning_vekt <- if (!all(is.na(typflyttning_klartext))) hamta_kod_med_klartext(px_meta, typflyttning_klartext, skickad_fran_variabel = "typflyttning") else NA
    
    kon_giltiga <- hamta_giltiga_varden_fran_tabell(px_meta, "kon") %>% .[. %in% c("1", "2")]
    kon_koder <- if (all(!is.na(kon_klartext))) {
      # om * ta alla giltiga 
      if (all(kon_klartext == "*")) kon_giltiga else hamta_kod_med_klartext(px_meta, kon_klartext, skickad_fran_variabel = "kon") %>% .[. %in% kon_giltiga]
    } else NA
    
    cont_vekt <-  hamta_kod_med_klartext(px_meta, cont_klartext, "contentscode")
  
    # hantering av tid (i detta fall år) och att kunna skicka med "9999" som senaste år
    giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "tid")
    tabell_tid <- hamta_tid[hamta_tid %in% giltiga_ar]
    
    if (length(tabell_tid) > 0) {
      # query-lista till pxweb-uttag
      varlista <- list(
      	"Typflyttning" = typflyttning_vekt,
      	"Alder" = alder_hamta,
      	"Kon" = kon_koder,
      	"ContentsCode" = cont_vekt,
      	"Tid" = tabell_tid)
    
      if (all(is.na(typflyttning_klartext)) & !ar_ckm) varlista <- varlista[names(varlista) != "Typflyttning"]
      if (all(is.na(alder_koder)) & !ar_ckm) varlista <- varlista[names(varlista) != "Alder"]
      if (all(is.na(kon_klartext))) varlista <- varlista[names(varlista) != "Kon"]
    
      # Hämta data med varlista
      px_uttag <- pxweb_get(url = url_uttag, query = varlista)
    
      var_vektor <- NA
      var_vektor_klartext <- NA
    
      # gör om pxweb-uttaget till en dataframe
      px_df <- as.data.frame(px_uttag)
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
      if ("ålder" %in% names(px_df)) {
        px_df <- px_df %>% 
          mutate(ålder = ifelse(ålder == "totalt, samtliga åldrar", "totalt ålder", ålder))
      }
      
      # ta bort kolumner om vi valt bort civilstånd
      if (all(is.na(typflyttning_klartext)) & ar_ckm) px_df <- px_df %>% select(-`typ av flyttning`)
      if (all(is.na(alder_koder)) & ar_ckm) px_df <- px_df %>% select(-ålder)
      
      return(px_df)
    } # slut test om det finns giltiga år i just denna tabell
  } # slut hamta data-funktion

  retur_df <- map(url_list, ~hamta_data(url_uttag = .x)) %>% 
    list_rbind()
  
  # Om användaren vill spara data till en Excel-fil
  if (!is.na(output_mapp) & !is.na(excel_filnamn)){
    write.xlsx(retur_df, paste0(output_mapp, excel_filnamn))
  }

  # Returnera data som en dataframe om användern valt det
  if (returnera_df) return(retur_df)
} # slut hämta data-funktion
