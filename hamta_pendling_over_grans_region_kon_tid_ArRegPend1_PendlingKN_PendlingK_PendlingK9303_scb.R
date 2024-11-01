hamta_pendling_over_grans_region_kon_tid_scb <- function(
			region_vekt = "*",			   # Val av region. Finns: "0114", "0115", "0117", "0120", "0123", "0125", "0126", "0127", "0128", "0136", "0138", "0139", "0140", "0160", "0162", "0163", "0180", "0181", "0182", "0183", "0184", "0186", "0187", "0188", "0191", "0192", "0305", "0319", "0330", "0331", "0360", "0380", "0381", "0382", "0428", "0461", "0480", "0481", "0482", "0483", "0484", "0486", "0488", "0509", "0512", "0513", "0560", "0561", "0562", "0563", "0580", "0581", "0582", "0583", "0584", "0586", "0604", "0617", "0642", "0643", "0662", "0665", "0680", "0682", "0683", "0684", "0685", "0686", "0687", "0760", "0761", "0763", "0764", "0765", "0767", "0780", "0781", "0821", "0834", "0840", "0860", "0861", "0862", "0880", "0881", "0882", "0883", "0884", "0885", "0980", "1060", "1080", "1081", "1082", "1083", "1214", "1230", "1231", "1233", "1256", "1257", "1260", "1261", "1262", "1263", "1264", "1265", "1266", "1267", "1270", "1272", "1273", "1275", "1276", "1277", "1278", "1280", "1281", "1282", "1283", "1284", "1285", "1286", "1287", "1290", "1291", "1292", "1293", "1315", "1380", "1381", "1382", "1383", "1384", "1401", "1402", "1407", "1415", "1419", "1421", "1427", "1430", "1435", "1438", "1439", "1440", "1441", "1442", "1443", "1444", "1445", "1446", "1447", "1452", "1460", "1461", "1462", "1463", "1465", "1466", "1470", "1471", "1472", "1473", "1480", "1481", "1482", "1484", "1485", "1486", "1487", "1488", "1489", "1490", "1491", "1492", "1493", "1494", "1495", "1496", "1497", "1498", "1499", "1715", "1730", "1737", "1760", "1761", "1762", "1763", "1764", "1765", "1766", "1780", "1781", "1782", "1783", "1784", "1785", "1814", "1860", "1861", "1862", "1863", "1864", "1880", "1881", "1882", "1883", "1884", "1885", "1904", "1907", "1960", "1961", "1962", "1980", "1981", "1982", "1983", "1984", "2021", "2023", "2026", "2029", "2031", "2034", "2039", "2061", "2062", "2080", "2081", "2082", "2083", "2084", "2085", "2101", "2104", "2121", "2132", "2161", "2180", "2181", "2182", "2183", "2184", "2260", "2262", "2280", "2281", "2282", "2283", "2284", "2303", "2305", "2309", "2313", "2321", "2326", "2361", "2380", "2401", "2403", "2404", "2409", "2417", "2418", "2421", "2422", "2425", "2460", "2462", "2463", "2480", "2481", "2482", "2505", "2506", "2510", "2513", "2514", "2518", "2521", "2523", "2560", "2580", "2581", "2582", "2583", "2584", "1917"
			kon_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "män", "kvinnor", "män och kvinnor"
			cont_klartext = "*",			 #  Finns: "Inpendlare över kommungräns", "Utpendlare över kommungräns", "Bor och arbetar i kommunen"
			tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021"
			long_format = TRUE,			# TRUE = konvertera innehållsvariablerna i datasetet till long-format 
			wide_om_en_contvar = TRUE,			# TRUE = om man vill behålla wide-format om det bara finns en innehållsvariabel, FALSE om man vill konvertera till long-format även om det bara finns en innehållsvariabel
			output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
			excel_filnamn = "pendling_over_grans.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
			returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
){

  # ====================================================================================================
  #
  # Funktion för att hämta data från SCB:s API med hjälp av pxweb-paketet
  # Automatgenererat av en funktion i R som skrivits av Peter Möller, Region Dalarna
  #
  # Skapad av: moepet den 29 oktober 2024
  # Senast uppdaterad: 29 oktober 2024
  #
  # url till tabellens API: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0207__AM0207Z/PendlingKN/
  #												https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0207__AM0207L/PendlingK/
  #												https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0207__AM0207L/PendlingK9303/
  #
  # ====================================================================================================

  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
    			tidyverse,
    			writexl)

  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

  # Url till SCB:s databas
  url_list <- c("https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0210/AM0210F/ArRegPend1",
                "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0207/AM0207Z/PendlingKN",
                "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0207/AM0207L/PendlingK",
                "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0207/AM0207L/PendlingK9303")

  # hantera att innehållsvariablerna heter olika de olika åren
  px_meta_list <- map(url_list, ~ pxweb_get(.x))
  px_meta_enkel_list <- extrahera_unika_varden_flera_scb_tabeller(px_meta_list)  

  cont_var_vekt <- px_meta_enkel_list %>%
    keep(~ .$code == "ContentsCode") %>%
    pluck(1, "valueTexts")
  
  bor_samma_vekt <- cont_var_vekt %>% .[str_detect(., "och")] %>% unique()
  inpendlare_vekt <- cont_var_vekt %>% 
    .[!. %in% bor_samma_vekt] %>%
    .[str_detect(., "Inpendlare") | str_detect(., "arbetsställe i")] %>% unique() 
  utpendlare_vekt <- cont_var_vekt %>% 
    .[!. %in% bor_samma_vekt] %>%
    .[str_detect(., "Utpendlare") | str_detect(., "arbetsställe utanför")] %>% unique()
  
  if (any(cont_klartext %in% bor_samma_vekt)) cont_klartext <- c(cont_klartext, bor_samma_vekt) %>% unique()
  if (any(cont_klartext %in% inpendlare_vekt)) cont_klartext <- c(cont_klartext, inpendlare_vekt) %>% unique()
  if (any(cont_klartext %in% utpendlare_vekt)) cont_klartext <- c(cont_klartext, utpendlare_vekt) %>% unique()
  
  # speciallösning för att det finns olika benämningar på båda könen-värdet på variabeln kön
  kon_bada_var <- c("totalt", "män och kvinnor")
  if (any(kon_klartext %in% kon_bada_var)) kon_klartext <- c(kon_klartext, kon_bada_var) %>% unique()
  
 hamta_data <- function(url_uttag) {

  px_meta <- pxweb_get(url_uttag)

  varlist_koder <- pxvarlist(px_meta)$koder
  varlist_bada <- pxvarlist(px_meta)

  # Gör om från klartext till kod som databasen förstår
  kon_vekt <- if (!all(is.na(kon_klartext))) hamta_kod_med_klartext(px_meta, kon_klartext, skickad_fran_variabel = "kon") else NA

  cont_vekt <-  hamta_kod_med_klartext(px_meta, cont_klartext, "contentscode")
  if (length(cont_vekt) > 1) wide_om_en_contvar <- FALSE

  # Hantera tid-koder
  giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "tid")
  tid_vekt <- if (all(tid_koder != "*")) tid_koder %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar] %>% unique() else giltiga_ar

  # special, ta bort år 2020 och 2021 om det är RAMS-tabellen (annars blir de dubbelt)
  if (url_uttag == "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0207/AM0207Z/PendlingKN") {
    if (all(tid_vekt != "*")) tid_vekt <- tid_vekt %>% .[. != "2020" & . != "2021"]
  }
  
  if (length(tid_vekt) > 0) {
    # query-lista till pxweb-uttag
    varlista <- list(
    	"Region" = region_vekt,
    	"Kon" = kon_vekt,
    	"ContentsCode" = cont_vekt,
    	"Tid" = tid_vekt)
  
    if (all(is.na(kon_klartext))) varlista <- varlista[names(varlista) != "Kon"]
  
    # special för bas-tabellen där man bytt ut region mot kommun
    if (str_detect(url_uttag, "AM0210F")) names(varlista)[names(varlista) == "Region"] <- "Kommun"
    
    # Hämta data med varlista
    px_uttag <- pxweb_get(url = url_uttag, query = varlista)
  
    var_vektor <- c(regionkod = "Region", regionkod = "Kommun")
    var_vektor_klartext <- c("region", "region")
  
    # gör om pxweb-uttaget till en dataframe
    px_df <- as.data.frame(px_uttag)
    
    # special för bas för att döpa om variabler till samma som i rams
    if (str_detect(url_uttag, "AM0210F")) {
      px_df <- px_df %>%
        mutate(kön = ifelse(kön == "totalt", "män och kvinnor", kön))
      
      if ("bostad utanför kommunen men arbetsställe i kommunen" %in% names(px_df)) px_df <- px_df %>% rename(`Inpendlare över kommungräns` = `bostad utanför kommunen men arbetsställe i kommunen`)
      if ("bostad i kommunen men arbetsställe utanför kommunen" %in% names(px_df)) px_df <- px_df %>% rename(`Utpendlare över kommungräns` = `bostad i kommunen men arbetsställe utanför kommunen`)
      if ("bostad och arbetsställe i kommunen" %in% names(px_df)) px_df <- px_df %>% rename(`Bor och arbetar i kommunen` = `bostad och arbetsställe i kommunen`)
      if ("kommun" %in% names(px_df)) px_df <- px_df %>% rename(region = kommun)
    }
    
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
    if (long_format & !wide_om_en_contvar){ 
      if (str_detect(url_uttag, "AM0210F")) {
        cont_kol <- names(px_df) %>% .[!. %in% c("regionkod", "region", "kön", "år")]
        px_df <- px_df %>% konvertera_till_long_for_contentscode_variabler(url_uttag, content_kolumner = cont_kol)
        
      } else {
        px_df <- px_df %>% konvertera_till_long_for_contentscode_variabler(url_uttag)
      }
    } # slut om man vill ha long_format
  
    return(px_df)
   } # slut if-test om det finns mer än noll tid_koder
  } # slut hämta data-funktion 

  px_alla <- map(url_list, ~ hamta_data(.x)) %>% list_rbind()

  kol_sort <- c("år", "regionkod", "region", "kön", "variabel", "varde")
  kol_sort <- c(kol_sort, names(px_alla)[!names(px_alla) %in% kol_sort])
  px_alla <- px_alla %>% 
    select(any_of(kol_sort)) %>% 
    arrange(år, regionkod)
  
  # Om användaren vill spara data till en Excel-fil
  if (!is.na(output_mapp) & !is.na(excel_filnamn)){
    write.xlsx(px_alla, paste0(output_mapp, excel_filnamn))
  }

  # Returnera data som en dataframe om användern valt det
  if (returnera_df) return(px_alla)
} # slut hämta data-funktion
