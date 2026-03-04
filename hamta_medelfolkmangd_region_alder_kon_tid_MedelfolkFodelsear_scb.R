hamta_medelfolkmangd_region_alder_kon_tid_scb <- function(
			region_vekt = "20",			   # Val av region. Finns: "00", "01", "0114", "0115", "0117", "0120", "0123", "0125", "0126", "0127", "0128", "0136", "0138", "0139", "0140", "0160", "0162", "0163", "0180", "0181", "0182", "0183", "0184", "0186", "0187", "0188", "0191", "0192", "03", "0305", "0319", "0330", "0331", "0360", "0380", "0381", "0382", "04", "0428", "0461", "0480", "0481", "0482", "0483", "0484", "0486", "0488", "05", "0509", "0512", "0513", "0560", "0561", "0562", "0563", "0580", "0581", "0582", "0583", "0584", "0586", "06", "0604", "0617", "0642", "0643", "0662", "0665", "0680", "0682", "0683", "0684", "0685", "0686", "0687", "07", "0760", "0761", "0763", "0764", "0765", "0767", "0780", "0781", "08", "0821", "0834", "0840", "0860", "0861", "0862", "0880", "0881", "0882", "0883", "0884", "0885", "09", "0980", "10", "1060", "1080", "1081", "1082", "1083", "12", "1214", "1230", "1231", "1233", "1256", "1257", "1260", "1261", "1262", "1263", "1264", "1265", "1266", "1267", "1270", "1272", "1273", "1275", "1276", "1277", "1278", "1280", "1281", "1282", "1283", "1284", "1285", "1286", "1287", "1290", "1291", "1292", "1293", "13", "1315", "1380", "1381", "1382", "1383", "1384", "14", "1401", "1402", "1407", "1415", "1419", "1421", "1427", "1430", "1435", "1438", "1439", "1440", "1441", "1442", "1443", "1444", "1445", "1446", "1447", "1452", "1460", "1461", "1462", "1463", "1465", "1466", "1470", "1471", "1472", "1473", "1480", "1481", "1482", "1484", "1485", "1486", "1487", "1488", "1489", "1490", "1491", "1492", "1493", "1494", "1495", "1496", "1497", "1498", "1499", "17", "1715", "1730", "1737", "1760", "1761", "1762", "1763", "1764", "1765", "1766", "1780", "1781", "1782", "1783", "1784", "1785", "18", "1814", "1860", "1861", "1862", "1863", "1864", "1880", "1881", "1882", "1883", "1884", "1885", "19", "1904", "1907", "1960", "1961", "1962", "1980", "1981", "1982", "1983", "1984", "20", "2021", "2023", "2026", "2029", "2031", "2034", "2039", "2061", "2062", "2080", "2081", "2082", "2083", "2084", "2085", "21", "2101", "2104", "2121", "2132", "2161", "2180", "2181", "2182", "2183", "2184", "22", "2260", "2262", "2280", "2281", "2282", "2283", "2284", "23", "2303", "2305", "2309", "2313", "2321", "2326", "2361", "2380", "24", "2401", "2403", "2404", "2409", "2417", "2418", "2421", "2422", "2425", "2460", "2462", "2463", "2480", "2481", "2482", "25", "2505", "2506", "2510", "2513", "2514", "2518", "2521", "2523", "2560", "2580", "2581", "2582", "2583", "2584"
			alder_koder = "*",			 #  NA = tas inte med i uttaget,  Finns: "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "43", "44", "45", "46", "47", "48", "49", "50", "51", "52", "53", "54", "55", "56", "57", "58", "59", "60", "61", "62", "63", "64", "65", "66", "67", "68", "69", "70", "71", "72", "73", "74", "75", "76", "77", "78", "79", "80", "81", "82", "83", "84", "85", "86", "87", "88", "89", "90", "91", "92", "93", "94", "95", "96", "97", "98", "99", "100+", "tot"
			kon_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "män", "kvinnor"
			cont_klartext = "*",			 #  Finns: "Antal"
			tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024"
			output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
			excel_filnamn = "medelfolkmangd.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
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
  # url till tabellens API: https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101D/MedelfolkFodelsear
  #
  # ====================================================================================================

  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
    			tidyverse,
    			writexl)

  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

  # Url till databas
  url_list <- c("https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101D/MedelfolkFodelsear",
                "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101D/MedelfolkFodarCKM")
  
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
      alder_hamta <- if (all(alder_koder == "*")) alder_giltiga_varden else alder_koder %>% str_replace("100", "100+") %>% str_replace("totalt", "TOT1") %>% .[. %in% alder_giltiga_varden]
      if (ar_ckm) {
        alder_hamta <- alder_hamta %>%                             # byt ut till gitliga koder om man skickat med 100 eller totalt 
          as.character() %>%                                       # säkerställ att alder_koder är character och inte numeric
          .[. %in% alder_giltiga_varden]
      } else alder_hamta <- alder_hamta[alder_hamta %in% alder_giltiga_varden]
    } else {
      alder_hamta <- if (ar_ckm) "TOT1" else NA
    }
    
    kon_giltiga <- hamta_giltiga_varden_fran_tabell(px_meta, "kon") %>% .[. %in% c("1", "2")]
    kon_koder <- if (all(!is.na(kon_klartext))) {
      # om * ta alla giltiga 
      if (all(kon_klartext == "*")) kon_giltiga else hamta_kod_med_klartext(px_meta, kon_klartext, skickad_fran_variabel = "kon") %>% .[. %in% kon_giltiga]
    } else NA
    
    cont_vekt <-  hamta_kod_med_klartext(px_meta, cont_klartext, "contentscode")
    
    # Hantera tid-koder
    # hantering av tid (i detta fall år) och att kunna skicka med "9999" som senaste år
    giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "tid")
    tabell_tid <- hamta_tid[hamta_tid %in% giltiga_ar]
    
    if (length(tabell_tid) > 0) {
    # query-lista till pxweb-uttag
    varlista <- list(
    	"Region" = region_vekt,
    	"Alder" = alder_hamta,
    	"Kon" = kon_koder,
    	"ContentsCode" = cont_vekt,
    	"Tid" = tabell_tid)
  
    if (all(is.na(alder_koder)) & !ar_ckm) varlista <- varlista[names(varlista) != "Alder"]
    if (all(is.na(kon_klartext))) varlista <- varlista[names(varlista) != "Kon"]
  
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
    
    if ("ålder" %in% names(px_df)) {
      px_df <- px_df %>%
        mutate(ålder = ifelse(ålder == "totalt, samtliga åldrar", "totalt ålder", ålder))
    }
    
    if (all(is.na(alder_koder)) & ar_ckm) px_df <- px_df %>% select(-ålder)
    return(px_df)
    
    } # slut test om det finns giltita år
  } # slut hämta data-funktion
    
    retur_df <- map(url_list, ~hamta_data(url_uttag = .x)) %>% 
      list_rbind()
    
    
    # Om användaren vill spara data till en Excel-fil
    if (!is.na(output_mapp) & !is.na(excel_filnamn)){
      write.xlsx(retur_df, paste0(output_mapp, excel_filnamn))
    }
    

  # Returnera data som en dataframe om användern valt det
  if (returnera_df) return(retur_df)
} # slut hämta data-funktion
