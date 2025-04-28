hamta_sjalvskattad_halsa_riket_alder_fodelseland_kon_ar <- function(
    halsotillstand_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "Bra eller mycket bra hälsa"
    andel_och_konfidensintervall_klartext = "*",			 #  Finns: "Andel", "Konfidensintervall nedre gräns", "Konfidensintervall övre gräns", "Antal svar"
    alder_klartext = "*",			 #  Finns: "16-84 år", "16- år"
    region_vekt = "20",			   # Val av region. Finns: "00", "01", "02", "03", "04"
    kon_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "Totalt", "Kvinnor", "Män"
    fodelseland_klartext ="*", # Finns: "Sverige", "Övriga Norden", "Övriga Europa", "Övriga världen"
    tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2018", "2020", "2021", "2022", "2024"
    output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
    excel_filnamn = ".xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
    returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
){
  
  # ====================================================================================================
  #
  # Funktion för att hämta data från Folkhälsomyndighetens API med hjälp av pxweb-paketet
  # Automatgenererat av en funktion i R som skrivits av Peter Möller, Region Dalarna
  #
  # Skapad av: frkjon den 28 april 2025
  # Senast uppdaterad: 28 april 2025
  #
  # url till tabellens API: https://fohm-app.folkhalsomyndigheten.se/Folkhalsodata/pxweb/sv/A_Folkhalsodata/A_Folkhalsodata__A_Mo8__Halsoutfall__01Overgrip__01.01halsgod/halsgodcfod.px/
  #
  # ====================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
         tidyverse,
         writexl)
  
  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  # Url till databas
  url_uttag <- "https://fohm-app.folkhalsomyndigheten.se/Folkhalsodata/api/v1/sv/A_Folkhalsodata/A_Mo8/Halsoutfall/01Overgrip/01.01halsgod/halsgodcfod.px"
  px_meta <- pxweb_get(url_uttag)
  
  varlist_koder <- pxvarlist(px_meta)$koder
  varlist_bada <- pxvarlist(px_meta)
  
  # Gör om från klartext till kod som databasen förstår
  halsotillstand_vekt <- if (!all(is.na(halsotillstand_klartext))) hamta_kod_med_klartext(px_meta, halsotillstand_klartext, skickad_fran_variabel = "hälsotillstånd") else NA
  andel_och_konfidensintervall_vekt <- hamta_kod_med_klartext(px_meta, andel_och_konfidensintervall_klartext, skickad_fran_variabel = "andel och konfidensintervall")
  alder_vekt <- hamta_kod_med_klartext(px_meta, alder_klartext, skickad_fran_variabel = "ålder")
  fodelseland_vekt <- hamta_kod_med_klartext(px_meta, fodelseland_klartext, skickad_fran_variabel = "födelseland")
  kon_vekt <- if (!all(is.na(kon_klartext))) hamta_kod_med_klartext(px_meta, kon_klartext, skickad_fran_variabel = "kön") else NA
  
  # Hantera tid-koder
  px_meta$variables <- sortera_px_variabler(px_meta$variables, sorterings_vars = "År", sortera_pa_kod = FALSE)        # sortera om månader så att de kommer i kronologisk ordning
  tid_koder <- tid_koder %>%           # ersätt "9999" med senaste år
    str_replace_all("9999", hamta_giltiga_varden_fran_tabell(px_meta, "År", klartext = TRUE) %>% max())
  giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "År")
  
  if (all(tid_koder == "*")) {
    tid_vekt <- giltiga_ar
  } else {
    tid_vekt <- map(tid_koder, function(period) {
      if (str_detect(period, ":")){     # kontrollera om det finns ett kolon = intervall
        intervall <- map_chr(str_split(period, ":") %>% unlist(), ~ hamta_kod_med_klartext(px_meta, .x, "År"))
        retur_txt <- giltiga_ar[which(giltiga_ar == intervall[1]):which(giltiga_ar == intervall[2])]
      } else retur_txt <- hamta_kod_med_klartext(px_meta, period, "År")
    }) %>% unlist()
    index_period <- map_lgl(px_meta$variables, ~ .x$text == "År")          # hitta platsen i px_meta$variables där variabeln "År" finns
    period_varden <- px_meta$variables[[which(index_period)]]$values         # läs in alla värden för variabeln "År"
    tid_vekt <- tid_vekt[match(period_varden[period_varden %in% tid_vekt], tid_vekt)]        # sortera om tid_vekt utifrån ordningen i px_meta (som vi sorterade ovan) 
  }
  
  # query-lista till pxweb-uttag
  varlista <- list(
    "Hälsotillstånd" = halsotillstand_vekt,
    "Andel och konfidensintervall" = andel_och_konfidensintervall_vekt,
    "Ålder" = alder_vekt,
    "Födelseland" = fodelseland_vekt,
    "Kön" = kon_vekt,
    "År" = tid_vekt)
  
  if (all(is.na(halsotillstand_klartext))) varlista <- varlista[names(varlista) != "Hälsotillstånd"]
  if (all(is.na(fodelseland_klartext))) varlista <- varlista[names(varlista) != "Födelseland"]
  if (all(is.na(kon_klartext))) varlista <- varlista[names(varlista) != "Kön"]
  
  # Hämta data med varlista
  px_uttag <- pxweb_get(url = url_uttag, query = varlista)
  
  var_vektor <- c(födelselandkod = "Födelseland")
  var_vektor_klartext <- "Födelseland"
  
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
  
  # Om användaren vill spara data till en Excel-fil
  if (!is.na(output_mapp) & !is.na(excel_filnamn)){
    write.xlsx(px_df, paste0(output_mapp, excel_filnamn))
  }
  
  # Returnera data som en dataframe om användern valt det
  if (returnera_df) return(px_df)
} # slut hämta data-funktion
