hamta_regso_bilinnehav_region_bestand_tid_scb <- function(
			region_vekt = "20",			   # Val av region. Finns: t.ex. "0114A0010_DeSO2025", "0114C1010_DeSO2025", "2584R014_RegSO2025", "2584R015_RegSO2025", "00", "01", "0114", "0115", "03", "04", "05", "06", "07", "08", "09", "10", "12", "13", "14", "17", "18", "19", "20", "21", "22", "23", "24", "25", "2583", "2584", "0114A0010", "0114C1010", "0114R001", "0114R002", "2584C1110", "2584C1130", "2584R014", "2584R015"
			bestand_klartext = "*",			 #  Finns: "i trafik", "avställda", "totalt"
			cont_klartext = "*",			 #  Finns: "Antal"
			tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024"
			region_indelning = "regso",        # "regso", "deso" eller "alla
			long_format = TRUE,			# TRUE = konvertera innehållsvariablerna i datasetet till long-format 
			wide_om_en_contvar = TRUE,			# TRUE = om man vill behålla wide-format om det bara finns en innehållsvariabel, FALSE om man vill konvertera till long-format även om det bara finns en innehållsvariabel
			output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
			excel_filnamn = "regso_bilinnehav.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
			returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
){

  # ====================================================================================================
  #
  # Funktion för att hämta data från SCB:s API med hjälp av pxweb-paketet
  # Automatgenererat av en funktion i R som skrivits av Peter Möller, Region Dalarna
  #
  # Skapad av: moepet den 13 november 2025
  # Senast uppdaterad: 13 november 2025
  #
  # url till tabellens API: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__TK__TK1001__TK1001Z/PersBilarDesoN/
  #												https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__TK__TK1001__TK1001Z/PersBilarDeso/
  #
  # ====================================================================================================

  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
    			tidyverse,
    			writexl)

  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

  # Url till databas
  url_list <- c("https://api.scb.se/OV0104/v1/doris/sv/ssd/START/TK/TK1001/TK1001Z/PersBilarDesoN",
						"https://api.scb.se/OV0104/v1/doris/sv/ssd/START/TK/TK1001/TK1001Z/PersBilarDeso")

  # ta ut senaste år från "9999" i båda tabellerna
  if (any(str_detect(tid_koder, "9999"))) {
    max_ar <-  map(url_list, ~ hamta_giltiga_varden_fran_tabell(.x, "tid")) %>%
    unlist() %>%
    max()
    tid_koder <- tid_koder %>% str_replace_all("9999", max_ar)
  }
  
 hamta_data <- function(url_uttag) {

  px_meta <- pxweb_get(url_uttag)

  varlist_koder <- pxvarlist(px_meta)$koder
  varlist_bada <- pxvarlist(px_meta)

  alla_regionkoder <- hamta_giltiga_varden_fran_tabell(px_meta, "region") 
  
  # vi tar ut endast RegSO eller DeSO beroende på vad användaren valt
  alla_regionkoder <- if (all("alla" %in% tolower(region_indelning))) {
    alla_regionkoder
  } else if (all(c("deso", "regso") %in% tolower(region_indelning))) {
    alla_regionkoder[str_detect(alla_regionkoder, "A|B|C|R")]
  } else if (all("regso" %in% tolower(region_indelning))) {
    alla_regionkoder[str_detect(alla_regionkoder, "R")]
  } else if (all("deso" %in% tolower(region_indelning))) {
    alla_regionkoder[str_detect(alla_regionkoder, "A|B|C")]
  } else {
    alla_regionkoder
  }
  
  if (!("*" %in% region_vekt)) {
    # här delar vi upp de medskickade regionkoderna i RegSO/DeSO, länskoder och kommunkoder 
    fardiga_regionkoder <- region_vekt[str_length(region_vekt) > 7]           # RegSO eller DeSO
    lanskoder <- region_vekt[str_length(region_vekt) == 2]                     # länskoder
    kommunkoder <- region_vekt[str_length(region_vekt) == 4]                   # kommunkoder
    
    # här hämtar vi alla RegSO/DeSO för de länskoder eller kommunkoder som skickats med
    region_lan <- alla_regionkoder[str_sub(alla_regionkoder, 1,2) %in% lanskoder & str_length(alla_regionkoder) > 7]
    region_kommun <- alla_regionkoder[str_sub(alla_regionkoder, 1,4) %in% kommunkoder & str_length(alla_regionkoder) > 7]
    
    # vi lägger ihop vektorerna för färdiga RegSO/DeSO, samt RegSO/DeSO för de län och kommuner som skickats med samt tar bort dubletter
    alla_region <- c(fardiga_regionkoder, region_lan, region_kommun) %>% .[!duplicated(.)]
    
  } else alla_region <- alla_regionkoder                     # om användaren vill ha samtliga koder för RegSO eller DeSO
  
  
  # Gör om från klartext till kod som databasen förstår
  bestand_vekt <- hamta_kod_med_klartext(px_meta, bestand_klartext, skickad_fran_variabel = "bestand")

  cont_vekt <-  hamta_kod_med_klartext(px_meta, cont_klartext, "contentscode")
  if (length(cont_vekt) > 1) wide_om_en_contvar <- FALSE

  # Hantera tid-koder
  giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "tid")
  tid_vekt <- if (all(tid_koder != "*")) tid_koder %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar] %>% unique() else giltiga_ar

  if (length(tid_vekt) > 0) {
  # query-lista till pxweb-uttag
  varlista <- list(
  	"Region" = alla_region,
  	"Bestand" = bestand_vekt,
  	"ContentsCode" = cont_vekt,
  	"Tid" = tid_vekt)

  # Hämta data med varlista
  px_uttag <- pxweb_get(url = url_uttag, query = varlista)

  var_vektor <- c(regionkod = "Region")
  var_vektor_klartext <- "region"

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
  
  # special för att det finns fler versioner av regionkoder
  px_df <- px_df %>%
    mutate(regionkod = regionkod %>% str_remove("_.*")) %>%
    group_by(across(-Antal)) %>%
    summarise(Antal = sum(Antal, na.rm = TRUE), .groups = "drop")
  

  # man kan välja bort long-format, då låter vi kolumnerna vara wide om det finns fler innehållsvariabler, annars
  # pivoterar vi om till long-format, dock ej om det bara finns en innehållsvariabel
  if (long_format & !wide_om_en_contvar) px_df <- px_df %>% konvertera_till_long_for_contentscode_variabler(url_uttag)

  return(px_df)
  }
  } # slut hämta data-funktion 

  px_alla <- map(url_list, ~ hamta_data(.x)) %>% list_rbind()

  # Om användaren vill spara data till en Excel-fil
  if (!is.na(output_mapp) & !is.na(excel_filnamn)){
    write.xlsx(px_alla, paste0(output_mapp, excel_filnamn))
  }

  # Returnera data som en dataframe om användern valt det
  if (returnera_df) return(px_alla)
} # slut hämta data-funktion
