hamta_ek_standard_regso_region_alder_tid_scb <- function(
    region_vekt = "20", # Finns för riket, län, kommun och regso/deso. "20" i kombination med ej regso_deso = FALSE ger alla regso eller deso (variabeln region_indelning) i det området
    alder_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "totalt ålder", "0-19 år", "20-64 år", "20-65 år", "65+ år", "66+ år"
    cont_klartext = "*",			 #  Finns: "Låg ekonomisk standard, procent", "Hög ekonomisk standard, procent", "Antal personer totalt"
    region_indelning = "RegSO",          # Finns: "RegSO", "DeSO"
    ej_regso_deso = FALSE, # TRUE om man istället för att få ut samtliga regso/deso för ett område vill få ut enbart området (exempelvis Dalarna)
    tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023"
    long_format = TRUE,			# TRUE = konvertera innehållsvariablerna i datasetet till long-format 
    wide_om_en_contvar = TRUE,			# TRUE = om man vill behålla wide-format om det bara finns en innehållsvariabel, FALSE om man vill konvertera till long-format även om det bara finns en innehållsvariabel
    output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
    excel_filnamn = "ek_standard_regso.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
    returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
){
  
  # ====================================================================================================
  #
  # Funktion för att hämta data från SCB:s API med hjälp av pxweb-paketet
  # Automatgenererat av en funktion i R som skrivits av Peter Möller, Region Dalarna
  #
  # Skapad av: frkjon den 18 september 2025
  # Senast uppdaterad: 18 september 2025
  #
  # url till tabellens API: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__HE__HE0110__HE0110I/TabVX4InkDesoN2/
  # 
  # SCB har ändrat adress till tabell av oklar anledning. Ny adress: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__HE__HE0110__HE0110I/Tab4InkDesoRegso/
  # Har även ändrat api-adress nedan /Jon, 2026-02-13
  # ====================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
         tidyverse,
         writexl)
  
  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  # Url till databas
  url_uttag <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/HE/HE0110/HE0110I/Tab4InkDesoRegso"
  px_meta <- pxweb_get(url_uttag)
  
  varlist_koder <- pxvarlist(px_meta)$koder
  varlist_bada <- pxvarlist(px_meta)
  
  ########### Hanterar Regso/Deso #######################################################################################
  # vi hämtar alla regionkoder i databasen
  alla_regionkoder <- hamta_giltiga_varden_fran_tabell(px_meta, "region") 
  # vi tar ut endast RegSO eller DeSO beroende på vad användaren valt
  alla_regionkoder <- if(region_indelning == "RegSO") alla_regionkoder[str_detect(alla_regionkoder, "R")] else alla_regionkoder[str_detect(alla_regionkoder, "A|B|C")]
  
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
  
  #######################################################################################################################
  
  # Om man bara vill ta ut på riket/län/kommun (och inte få samtliga deso/regso för det området)
  
  if(ej_regso_deso == TRUE) alla_region = region_vekt
  
  # Gör om från klartext till kod som databasen förstår
  alder_vekt <- if (!all(is.na(alder_klartext))) hamta_kod_med_klartext(px_meta, alder_klartext, skickad_fran_variabel = "alder") else NA
  cont_vekt <-  hamta_kod_med_klartext(px_meta, cont_klartext, "contentscode")
  if (length(cont_vekt) > 1) wide_om_en_contvar <- FALSE
  
  # Hantera tid-koder
  giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "tid")
  tid_vekt <- if (all(tid_koder != "*")) tid_koder %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar] %>% unique() else giltiga_ar
  
  # query-lista till pxweb-uttag
  varlista <- list(
    "Region" = alla_region,
    "Alder" = alder_vekt,
    "ContentsCode" = cont_vekt,
    "Tid" = tid_vekt)
  
  if (all(is.na(alder_klartext))) varlista <- varlista[names(varlista) != "Alder"]
  
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
  
  # man kan välja bort long-format, då låter vi kolumnerna vara wide om det finns fler innehållsvariabler, annars
  # pivoterar vi om till long-format, dock ej om det bara finns en innehållsvariabel
  if (long_format & !wide_om_en_contvar) px_df <- px_df %>% konvertera_till_long_for_contentscode_variabler(url_uttag)
  
  # Om användaren vill spara data till en Excel-fil
  if (!is.na(output_mapp) & !is.na(excel_filnamn)){
    write.xlsx(px_df, paste0(output_mapp, excel_filnamn))
  }
  
  # Returnera data som en dataframe om användern valt det
  if (returnera_df) return(px_df)
} # slut hämta data-funktion
