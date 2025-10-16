hamta_regso_valdeltagande_region_bakgrund_kon_tid_scb <- function(
			region_vekt = "20",			   # Val av region. Finns: t.ex. "0114R001", "0114R002", "2584R014", "2584R015"
			bakgrund_klartext = "*",			 #  Finns: "samtliga 18 år och äldre", "ålder: 18-29 år", "ålder: 30-49 år", "ålder: 50-64 år", "ålder: 65+ år", "samtliga utbildningsnivåer", "utbildningsnivå: förgymnasial utbildning", "utbildningsnivå: gymnasial utbildning", "utbildningsnivå: eftergymnasial utbildning", "utbildningsnivå: uppgift saknas", "samtliga", "födelseregion: Sverige", "födelseregion: Norden exkl. Sverige", "födelseregion: EU/EFTA exkl. Norden", "födelseregion: övriga världen", "samtliga utrikes födda invandrare", "skäl till invandring: skyddsbehövande och deras anhöriga", "skäl till invandring: övriga utrikes födda invandrare", "samtliga utrikes födda", "vistelsetid 10- år", "vistelsetid < 10 år"
			kon_klartext = "*",			 #  Finns: "män och kvinnor", "män", "kvinnor"
			cont_klartext = "*",			 #  Finns: "Valdeltagande i val till riksdag, procent", "Valdeltagande i val till region, procent", "Valdeltagande i val till kommun, procent"
			tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2018", "2022"
			region_indelning = "regso",
			long_format = TRUE,			# TRUE = konvertera innehållsvariablerna i datasetet till long-format 
			wide_om_en_contvar = TRUE,			# TRUE = om man vill behålla wide-format om det bara finns en innehållsvariabel, FALSE om man vill konvertera till long-format även om det bara finns en innehållsvariabel
			output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
			excel_filnamn = "regso_valdeltagande.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
			returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
){

  # ====================================================================================================
  #
  # Funktion för att hämta data från SCB:s API med hjälp av pxweb-paketet
  # Automatgenererat av en funktion i R som skrivits av Peter Möller, Region Dalarna
  #
  # Skapad av: moepet den 16 oktober 2025
  # Senast uppdaterad: 16 oktober 2025
  #
  # url till tabellens API: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AA__AA0003__AA0003J/IntGr11RegSO1/
  #
  # ====================================================================================================

  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
    			tidyverse,
    			writexl)

  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

  # Url till databas
  url_uttag <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AA/AA0003/AA0003J/IntGr11RegSO1"
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
  bakgrund_vekt <- hamta_kod_med_klartext(px_meta, bakgrund_klartext, skickad_fran_variabel = "bakgrund")
  kon_vekt <- hamta_kod_med_klartext(px_meta, kon_klartext, skickad_fran_variabel = "kon")

  cont_vekt <- hamta_kod_med_klartext(px_meta, cont_klartext, "contentscode", returnera_alltid_snoflinga = FALSE)
  if (length(cont_vekt) > 1) wide_om_en_contvar <- FALSE

  # Hantera tid-koder
  giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "tid")
  tid_vekt <- if (all(tid_koder != "*")) tid_koder %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar] %>% unique() else giltiga_ar

  # query-lista till pxweb-uttag
  varlista <- list(
  	"Region" = alla_region,
  	"Bakgrund" = bakgrund_vekt,
  	"Kon" = kon_vekt,
  	"ContentsCode" = cont_vekt,
  	"Tid" = tid_vekt)

  # Hämta data med varlista
  px_uttag <- pxweb_get(url = url_uttag, query = varlista)

  var_vektor <- c(regionkod = "Region")
  var_vektor_klartext <- "region"

  # gör om pxweb-uttaget till en dataframe
  px_df <- suppress_specific_warning(as.data.frame(px_uttag)) %>% 
    rename(bakgrund = variabel)
  
  if (!all(is.na(var_vektor))) {
      # om man vill ha med koder också för variabler utöver klartext så läggs de på här (om det finns värden i var_vektor)
      px_df <- suppress_specific_warning(
        px_df %>%
            cbind(as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>%
            select(any_of(var_vektor)))
      )

      # kolumnerna med koder läggs framför motsvarande kolumner med klartext
      for (varflytt_index in 1:length(var_vektor)) {
        px_df <- px_df %>%
            relocate(any_of(names(var_vektor)[varflytt_index]), .before = any_of(var_vektor_klartext[varflytt_index]))
      }
  }

  # man kan välja bort long-format, då låter vi kolumnerna vara wide om det finns fler innehållsvariabler, annars
  # pivoterar vi om till long-format, dock ej om det bara finns en innehållsvariabel
  if (long_format & !wide_om_en_contvar) px_df <- px_df %>% konvertera_till_long_for_contentscode_variabler(px_meta)

  regionnyckel <- hamtaregtab() %>% rename(kommunkod = regionkod, kommun = region)
  
  if ("varde" %in% names(px_df)){
    px_df <- px_df %>% 
      filter(!is.na(varde))
  } else {
    alla_cont <- hamta_giltiga_varden_fran_tabell(px_meta, "contentscode", klartext = TRUE) 
    tabell_cont <- alla_cont[alla_cont %in% names(px_df)]
    px_df <- px_df %>%
      filter(if_all(all_of(tabell_cont), ~ !is.na(.x)))
  }
  
  # ta bara med värden som inte är NA, korrigera region om det är regso
  px_df <- px_df %>% 
    mutate(region = if_else(str_detect(region, "\\("), str_extract(region, "(?<=\\().*(?=\\))"), region), # allt inuti parentesen, utan ()
           kommunkod = str_sub(regionkod, 1, 4)) %>% 
    left_join(regionnyckel, by = "kommunkod") %>% 
    relocate(kommunkod, kommun, .after = regionkod)
  
  # Om användaren vill spara data till en Excel-fil
  if (!is.na(output_mapp) & !is.na(excel_filnamn)){
    write.xlsx(px_df, paste0(output_mapp, excel_filnamn))
  }

  # Returnera data som en dataframe om användern valt det
  if (returnera_df) return(px_df)
} # slut hämta data-funktion
