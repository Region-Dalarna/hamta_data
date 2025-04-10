hamta_sjalvskattad_halsa_lan_kommun_halsotillstand_kon_ar_fohm <- function(
    region_vekt = "20",			   # Val av region. Finns: "00", "01", "03", "04", "05", "06", "07", "08", "09", "10", "12", "13", "14", "17", "18", "19", "20", "21", "22", "23", "24", "25", "0114", "0115", "0117", "0120", "0123", "0125", "0126", "0127", "0128", "0136", "0138", "0139", "0140", "0160", "0162", "0163", "0180", "0181", "0182", "0183", "0184", "0186", "0187", "0188", "0191", "0192", "0305", "0319", "0330", "0331", "0360", "0380", "0381", "0382", "0428", "0461", "0480", "0481", "0482", "0483", "0484", "0486", "0488", "0509", "0512", "0513", "0560", "0561", "0562", "0563", "0580", "0581", "0582", "0583", "0584", "0586", "0604", "0617", "0642", "0643", "0662", "0665", "0680", "0682", "0683", "0684", "0685", "0686", "0687", "0760", "0761", "0763", "0764", "0765", "0767", "0780", "0781", "0821", "0834", "0840", "0860", "0861", "0862", "0880", "0881", "0882", "0883", "0884", "0885", "0980", "1060", "1080", "1081", "1082", "1083", "1214", "1230", "1231", "1233", "1256", "1257", "1260", "1261", "1262", "1263", "1264", "1265", "1266", "1267", "1270", "1272", "1273", "1275", "1276", "1277", "1278", "1280", "1281", "1282", "1283", "1284", "1285", "1286", "1287", "1290", "1291", "1292", "1293", "1315", "1380", "1381", "1382", "1383", "1384", "1401", "1402", "1407", "1415", "1419", "1421", "1427", "1430", "1435", "1438", "1439", "1440", "1441", "1442", "1443", "1444", "1445", "1446", "1447", "1452", "1460", "1461", "1462", "1463", "1465", "1466", "1470", "1471", "1472", "1473", "1480", "1481", "1482", "1484", "1485", "1486", "1487", "1488", "1489", "1490", "1491", "1492", "1493", "1494", "1495", "1496", "1497", "1498", "1499", "1715", "1730", "1737", "1760", "1761", "1762", "1763", "1764", "1765", "1766", "1780", "1781", "1782", "1783", "1784", "1785", "1814", "1860", "1861", "1862", "1863", "1864", "1880", "1881", "1882", "1883", "1884", "1885", "1904", "1907", "1960", "1961", "1962", "1980", "1981", "1982", "1983", "1984", "2021", "2023", "2026", "2029", "2031", "2034", "2039", "2061", "2062", "2080", "2081", "2082", "2083", "2084", "2085", "2101", "2104", "2121", "2132", "2161", "2180", "2181", "2182", "2183", "2184", "2260", "2262", "2280", "2281", "2282", "2283", "2284", "2303", "2305", "2309", "2313", "2321", "2326", "2361", "2380", "2401", "2403", "2404", "2409", "2417", "2418", "2421", "2422", "2425", "2460", "2462", "2463", "2480", "2481", "2482", "2505", "2506", "2510", "2513", "2514", "2518", "2521", "2523", "2560", "2580", "2581", "2582", "2583", "2584"
    halsotillstand_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "Bra eller mycket bra hälsa"
    andel_och_konfidensintervall_klartext = "*",			 #  Finns: "Andel", "Konfidensintervall nedre gräns", "Konfidensintervall övre gräns", "Antal svar"
    kon_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "Totalt", "Kvinnor", "Män"
    tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2004-2007", "2005-2008", "2006-2009", "2007-2010", "2008-2011", "2009-2012", "2010-2013", "2011-2014", "2012-2015", "2013-2016", "2015-2018", "2017-2020", "2018-2021", "2019-2022", "2021-2024"
    output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
    excel_filnamn = "sjalvskattad_halsa.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
    returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
){
  
  # ====================================================================================================
  #
  # Funktion för att hämta data från Folkhälsomyndighetens API med hjälp av pxweb-paketet
  # Automatgenererat av en funktion i R som skrivits av Peter Möller, Region Dalarna
  #
  # Skapad av: frkjon den 17 februari 2025
  # Senast uppdaterad: 17 februari 2025
  #
  # url till tabellens API: https://fohm-app.folkhalsomyndigheten.se/Folkhalsodata/pxweb/sv/A_Folkhalsodata/A_Folkhalsodata__A_Mo8__Halsoutfall__01Overgrip__01.01halsgod/halsgodyreg.px/
  #
  # ====================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
         tidyverse,
         writexl)
  
  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  # Url till databas
  url_uttag <- "https://fohm-app.folkhalsomyndigheten.se/Folkhalsodata/api/v1/sv/A_Folkhalsodata/A_Mo8/Halsoutfall/01Overgrip/01.01halsgod/halsgodyreg.px"
  px_meta <- pxweb_get(url_uttag)
  
  varlist_koder <- pxvarlist(px_meta)$koder
  varlist_bada <- pxvarlist(px_meta)
  
  # Gör om från klartext till kod som databasen förstår
  halsotillstand_vekt <- if (!all(is.na(halsotillstand_klartext))) hamta_kod_med_klartext(px_meta, halsotillstand_klartext, skickad_fran_variabel = "hälsotillstånd") else NA
  andel_och_konfidensintervall_vekt <- hamta_kod_med_klartext(px_meta, andel_och_konfidensintervall_klartext, skickad_fran_variabel = "andel och konfidensintervall")
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
    "Region" = region_vekt,
    "Hälsotillstånd" = halsotillstand_vekt,
    "Andel och konfidensintervall" = andel_och_konfidensintervall_vekt,
    "Kön" = kon_vekt,
    "År" = tid_vekt)
  
  if (all(is.na(halsotillstand_klartext))) varlista <- varlista[names(varlista) != "Hälsotillstånd"]
  if (all(is.na(kon_klartext))) varlista <- varlista[names(varlista) != "Kön"]
  
  # Hämta data med varlista
  px_uttag <- pxweb_get(url = url_uttag, query = varlista)
  
  var_vektor <- c(regionkod = "Region")
  var_vektor_klartext <- "Region"
  
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
  
  # speciallösning för Folkhälsomyndigheten där vi tar bort regionkoder som ligger i klartextkolumnen
  px_df <- region_kolumn_splitta_kod_klartext(px_df, "Region")
  
  # Om användaren vill spara data till en Excel-fil
  if (!is.na(output_mapp) & !is.na(excel_filnamn)){
    write.xlsx(px_df, paste0(output_mapp, excel_filnamn))
  }
  
  # Returnera data som en dataframe om användern valt det
  if (returnera_df) return(px_df)
} # slut hämta data-funktion
