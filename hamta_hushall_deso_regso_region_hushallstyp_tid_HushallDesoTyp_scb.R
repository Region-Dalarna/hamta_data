hamta_hushall_deso_regso_region_hushallstyp_tid_scb <- function(
			region_vekt = "20",			   # Val av region. Finns: t.ex. "00", "01", "0114", "0114A0010_DeSO2025", "0114A0010", "0114C1010_DeSO2025", "0114C1010", "0114R001", "0114R002", "0115", "03", "04", "05", "06", "07", "08", "09", "10", "12", "13", "14", "17", "18", "19", "20", "21", "22", "23", "24", "25", "2583", "2584", "2584C1110", "2584C1130", "2584R014_RegSO2025", "2584R014", "2584R015_RegSO2025", "2584R015"
			hushallstyp_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "sammanboende med barn", "sammanboende utan barn", "ensamstående med barn", "ensamstående utan barn", "övriga hushåll", "totalt antal hushåll"
			cont_klartext = "*",			 #  Finns: "Antal hushåll"
			tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024", "2025"
			output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
			excel_filnamn = "hushall_deso_regso.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
			returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
){

  # ====================================================================================================
  #
  # Funktion för att hämta data från SCB:s API med hjälp av pxweb-paketet
  # Automatgenererat av en funktion i R som skrivits av Peter Möller, Region Dalarna
  #
  # Skapad av: moepet den 05 juni 2026
  # Senast uppdaterad: 05 juni 2026
  #
  # url till tabellens API: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__BE__BE0101__BE0101Y/HushallDesoTyp/
  #
  # ====================================================================================================

  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
    			tidyverse,
    			writexl)

  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

  # Url till databas
  url_uttag <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101Y/HushallDesoTyp"
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
  hushallstyp_vekt <- if (!all(is.na(hushallstyp_klartext))) hamta_kod_med_klartext(px_meta, hushallstyp_klartext, skickad_fran_variabel = "hushallstyp") else NA

  cont_vekt <-  hamta_kod_med_klartext(px_meta, cont_klartext, "contentscode")
  if (length(cont_vekt) > 1) wide_om_en_contvar <- FALSE

  # Hantera tid-koder
  giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "tid")
  tid_vekt <- if (all(tid_koder != "*")) tid_koder %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar] %>% unique() else giltiga_ar

  # query-lista till pxweb-uttag
  varlista <- list(
  	"Region" = alla_region,
  	"Hushallstyp" = hushallstyp_vekt,
  	"ContentsCode" = cont_vekt,
  	"Tid" = tid_vekt)

  if (all(is.na(hushallstyp_klartext))) varlista <- varlista[names(varlista) != "Hushallstyp"]

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
  # ta fram kommunnyckel en gång
  kommunkoder <- px_df$regionkod %>% 
    str_remove("_.*") %>% 
    str_sub(1, 4) %>% 
    unique()
  
  kommunnyckel <- hamtaregion_kod_namn(kommunkoder)
  
  regso_nyckel <- px_df %>%
    mutate(
      regionkod_clean = str_remove(regionkod, "_.*"),
      region_regso = str_extract(region, "\\([^()]+\\)") %>%
        str_remove_all("^\\(|\\)$") %>% 
        str_trim()
    ) %>%
    filter(!is.na(region_regso)) %>%
    distinct(regionkod_clean, region_regso)
  
  px_df <- px_df %>%
    
    # 1. Städa regionkod och skapa hjälpkolumner
    mutate(
      # tar bort suffix som "_RegSO2025", så att t.ex.
      # "2084R005_RegSO2025" och "2084R005" blir samma kod
      regionkod = str_remove(regionkod, "_.*"),
      
      # kommunkod är de fyra första tecknen i regionkoden
      kommunkod = str_sub(regionkod, 1, 4),
      
      # intern hjälpflagga: TRUE om aktuell körning gäller RegSO
      # str_detect() gör den lite mer robust än == "RegSO"
      is_regso = str_detect(region_indelning, "RegSO")
    ) %>% 
    
    
    # 2. Hämta kommunnamn ur regiontexten för RegSO
    mutate(
      kommun = if_else(
        is_regso,
        
        # för t.ex. "2021R001 Vansbro (Vansbro västra landsbygd)"
        # tas först parentesdelen bort, sedan regionkoden i början
        # kvar blir "Vansbro"
        region %>% 
          str_remove("\\s*\\(.*") %>% 
          str_remove("^\\d{4}R\\d{3}\\s+") %>% 
          str_trim(),
        
        # för DeSO fylls kommun på senare via kommunnyckel
        NA_character_
      )
    ) %>% 
    
    
    # 3. Koppla på RegSO-namn från nyckeln
    # Detta hanterar att vissa rader har parentesnamn och andra inte,
    # men samma regionkod efter att suffixet har tagits bort.
    left_join(
      regso_nyckel %>% 
        distinct(regionkod_clean, region_regso),
      by = c("regionkod" = "regionkod_clean")
    ) %>% 
    
    
    # 4. Skriv om region till själva områdesnamnet för RegSO
    mutate(
      region = coalesce(
        str_extract(region, "\\([^()]+\\)") %>% 
          str_remove_all("^\\(|\\)$") %>% 
          str_trim(),
        region_regso,
        region
      )
    ) %>%
    
    # 5. Ta bort tillfällig RegSO-kolumn
    select(-region_regso) %>% 
    
    # 6. Koppla på kommunnamn via kommunkod
    # Detta behövs framför allt för DeSO, men används också som fallback
    # om kommun saknas för någon RegSO-rad.
    left_join(
      kommunnyckel %>% 
        rename(kommun_join = region) %>% 
        select(regionkod, kommun_join),
      by = c("kommunkod" = "regionkod")
    ) %>% 
    
    
    # 7. Fyll kommun
    # Om kommun redan finns från RegSO-texten används den.
    # Annars används kommunnamnet från kommunnyckeln.
    mutate(
      kommun = coalesce(kommun, kommun_join)
    ) %>% 
    
    
    # 8. Ta bort tillfälliga hjälpkolumner
    select(
      -kommun_join,
      -is_regso
    ) %>% 
    
    
    # 9. Slå ihop rader där regionkoderna tidigare hade olika suffix
    # Exempel: "2084R005_RegSO2025" och "2084R005"
    # grupperas nu tillsammans eftersom regionkod har rensats.
    group_by(across(-`Antal hushåll`)) %>% 
    
    summarise(
      `Antal hushåll` = sum(`Antal hushåll`, na.rm = TRUE),
      .groups = "drop"
    ) %>% 
    
    
    # 10. Lägg kolumnerna i önskad ordning
    relocate(kommunkod, .after = region) %>% 
    relocate(kommun, .after = kommunkod)
  
  # Om användaren vill spara data till en Excel-fil
  if (!is.na(output_mapp) & !is.na(excel_filnamn)){
    write.xlsx(px_df, paste0(output_mapp, excel_filnamn))
  }

  # Returnera data som en dataframe om användern valt det
  if (returnera_df) return(px_df)
} # slut hämta data-funktion
