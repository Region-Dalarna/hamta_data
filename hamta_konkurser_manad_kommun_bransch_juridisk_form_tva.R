hamta_konkurser_region_bransch_juridisk_form_tva <- function(
			tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2009M01", "2009M02", "2009M03", "2009M04", "2009M05", "2009M06", "2009M07", "2009M08", "2009M09", "2009M10", "2009M11", "2009M12", "2010M01", "2010M02", "2010M03", "2010M04", "2010M05", "2010M06", "2010M07", "2010M08", "2010M09", "2010M10", "2010M11", "2010M12", "2011M01", "2011M02", "2011M03", "2011M04", "2011M05", "2011M06", "2011M07", "2011M08", "2011M09", "2011M10", "2011M11", "2011M12", "2012M01", "2012M02", "2012M03", "2012M04", "2012M05", "2012M06", "2012M07", "2012M08", "2012M09", "2012M10", "2012M11", "2012M12", "2013M01", "2013M02", "2013M03", "2013M04", "2013M05", "2013M06", "2013M07", "2013M08", "2013M09", "2013M10", "2013M11", "2013M12", "2014M01", "2014M02", "2014M03", "2014M04", "2014M05", "2014M06", "2014M07", "2014M08", "2014M09", "2014M10", "2014M11", "2014M12", "2015M01", "2015M02", "2015M03", "2015M04", "2015M05", "2015M06", "2015M07", "2015M08", "2015M09", "2015M10", "2015M11", "2015M12", "2016M01", "2016M02", "2016M03", "2016M04", "2016M05", "2016M06", "2016M07", "2016M08", "2016M09", "2016M10", "2016M11", "2016M12", "2017M01", "2017M02", "2017M03", "2017M04", "2017M05", "2017M06", "2017M07", "2017M08", "2017M09", "2017M10", "2017M11", "2017M12", "2018M01", "2018M02", "2018M03", "2018M04", "2018M05", "2018M06", "2018M07", "2018M08", "2018M09", "2018M10", "2018M11", "2018M12", "2019M01", "2019M02", "2019M03", "2019M04", "2019M05", "2019M06", "2019M07", "2019M08", "2019M09", "2019M10", "2019M11", "2019M12", "2020M01", "2020M02", "2020M03", "2020M04", "2020M05", "2020M06", "2020M07", "2020M08", "2020M09", "2020M10", "2020M11", "2020M12", "2021M01", "2021M02", "2021M03", "2021M04", "2021M05", "2021M06", "2021M07", "2021M08", "2021M09", "2021M10", "2021M11", "2021M12", "2022M01", "2022M02", "2022M03", "2022M04", "2022M05", "2022M06", "2022M07", "2022M08", "2022M09", "2022M10", "2022M11", "2022M12", "2023M01", "2023M02", "2023M03", "2023M04", "2023M05", "2023M06", "2023M07", "2023M08", "2023M09", "2023M10", "2023M11", "2023M12", "2024M01", "2024M02", "2024M03", "2024M04", "2024M05", "2024M06", "2024M07", "2024M08", "2024M09", "2024M10", "2024M11", "2024M12", "2025M01"
			region_vekt = "*",			   # Val av region. Finns: "0114", "0115", "0117", "0120", "0123", "0125", "0126", "0127", "0128", "0136", "0138", "0139", "0140", "0160", "0162", "0163", "0180", "0181", "0182", "0183", "0184", "0186", "0187", "0188", "0191", "0192", "0305", "0319", "0330", "0331", "0360", "0380", "0381", "0382", "0428", "0461", "0480", "0481", "0482", "0483", "0484", "0486", "0488", "0509", "0512", "0513", "0560", "0561", "0562", "0563", "0580", "0581", "0582", "0583", "0584", "0586", "0604", "0617", "0642", "0643", "0662", "0665", "0680", "0682", "0683", "0684", "0685", "0686", "0687", "0760", "0761", "0763", "0764", "0765", "0767", "0780", "0781", "0821", "0834", "0840", "0860", "0861", "0862", "0880", "0881", "0882", "0883", "0884", "0885", "0980", "1060", "1080", "1081", "1082", "1083", "1214", "1230", "1231", "1233", "1256", "1257", "1260", "1261", "1262", "1263", "1264", "1265", "1266", "1267", "1270", "1272", "1273", "1275", "1276", "1277", "1278", "1280", "1281", "1282", "1283", "1284", "1285", "1286", "1287", "1290", "1291", "1292", "1293", "1315", "1380", "1381", "1382", "1383", "1384", "1401", "1402", "1407", "1415", "1419", "1421", "1427", "1430", "1435", "1438", "1439", "1440", "1441", "1442", "1443", "1444", "1445", "1446", "1447", "1452", "1460", "1461", "1462", "1463", "1465", "1466", "1470", "1471", "1472", "1473", "1480", "1481", "1482", "1484", "1485", "1486", "1487", "1488", "1489", "1490", "1491", "1492", "1493", "1494", "1495", "1496", "1497", "1498", "1499", "1715", "1730", "1737", "1760", "1761", "1762", "1763", "1764", "1765", "1766", "1780", "1781", "1782", "1783", "1784", "1785", "1814", "1860", "1861", "1862", "1863", "1864", "1880", "1881", "1882", "1883", "1884", "1885", "1904", "1907", "1960", "1961", "1962", "1980", "1981", "1982", "1983", "1984", "2021", "2023", "2026", "2029", "2031", "2034", "2039", "2061", "2062", "2080", "2081", "2082", "2083", "2084", "2085", "2101", "2104", "2121", "2132", "2161", "2180", "2181", "2182", "2183", "2184", "2260", "2262", "2280", "2281", "2282", "2283", "2284", "2303", "2305", "2309", "2313", "2321", "2326", "2361", "2380", "2401", "2403", "2404", "2409", "2417", "2418", "2421", "2422", "2425", "2460", "2462", "2463", "2480", "2481", "2482", "2505", "2506", "2510", "2513", "2514", "2518", "2521", "2523", "2560", "2580", "2581", "2582", "2583", "2584"
			bransch_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "01-03 Jord-, skogsbruk och fiske", "05-39 Tillverkningsindustri m.fl.", "41-43 Byggindustri", "45 Handel med samt reparation av motorfordon", "46 Parti- och provisionshandel utom med motorfordon", "47 Detaljhandel utom med motordorfon", "49-53 Transport och magasinsering", "55 Hotell- och logiverksamhet", "56 Restaurang-, catering och barverksamhet", "58-63 Information och kommunikation", "64-82 Finans-, fastighets- och företagstjänster", "85-99 Utbildning, hälso- och sjukvård, övriga personliga tjänster", "Bransch ospecificerad"
			juridisk_form_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "Aktiebolag", "Handels- eller kommanditbolag mm."
			variabel_klartext = "*",			 #  Finns: "Antal anställda berörda av konkurser"
			output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
			excel_filnamn = "konkurser_lan_bransch.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
			returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
){

  # ====================================================================================================
  #
  # Funktion för att hämta data från Tillväxtanalys API med hjälp av pxweb-paketet
  # Automatgenererat av en funktion i R som skrivits av Peter Möller, Region Dalarna
  #
  # Skapad av: frkjon den 19 februari 2025
  # Senast uppdaterad: 19 februari 2025
  #
  # url till tabellens API: https://statistik.tillvaxtanalys.se:443/PxWeb/api/v1/sv/Tillväxtanalys statistikdatabas/Konkurser och offentliga ackord/z_anstallda_manad_kom_bransch_juridisk_form_popAH_20250212_140613.px
  # Enligt Tillväxtanalys är det preliminär statistik som på sikt kan utökas med flera typer av juridisk form.
  # Av oklar anledning hade url-adressen ändrats till 
  # https://statistik.tillvaxtanalys.se:443/PxWeb/api/v1/sv/Tillväxtanalys statistikdatabas/Konkurser och offentliga ackord/z_anstallda_manad_kom_bransch_juridisk_form_popAH_20250307_081423.px
  # Ytterligare en ändring av url-adress
  # https://statistik.tillvaxtanalys.se:443/PxWeb/api/v1/sv/Tillväxtanalys statistikdatabas/Konkurser och offentliga ackord/14_anstallda_manad_kom_bransch_juridisk_form_popAH_20250411_090221.px
  # Ytterligare ett byte av url-adress
  # https://statistik.tillvaxtanalys.se:443/PxWeb/api/v1/sv/Tillväxtanalys statistikdatabas/Konkurser och offentliga ackord/14_anstallda_manad_kom_bransch_juridisk_form_popAH.px
  # ====================================================================================================

  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
    			tidyverse,
    			writexl)

  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

  # Url till databas
  url_uttag <- "https://statistik.tillvaxtanalys.se:443/PxWeb/api/v1/sv/Tillväxtanalys statistikdatabas/Konkurser och offentliga ackord/14_anstallda_manad_kom_bransch_juridisk_form_popAH.px"
  px_meta <- pxweb_get(url_uttag)

  varlist_koder <- pxvarlist(px_meta)$koder
  varlist_bada <- pxvarlist(px_meta)

  # Gör om från klartext till kod som databasen förstår
  bransch_vekt <- if (!all(is.na(bransch_klartext))) hamta_kod_med_klartext(px_meta, bransch_klartext, skickad_fran_variabel = "bransch") else NA
  juridisk_form_vekt <- if (!all(is.na(juridisk_form_klartext))) hamta_kod_med_klartext(px_meta, juridisk_form_klartext, skickad_fran_variabel = "juridisk_form") else NA
  variabel_vekt <- hamta_kod_med_klartext(px_meta, variabel_klartext, skickad_fran_variabel = "variabel")

  # Hantera region-koder när regionkoderna ligger tillsammans med klartext i samma kolumn, och det är löpnummer istället för koder för län och kommuner
  region_vekt <- hamta_regionkod_med_knas_regionkod(px_meta, region_vekt, "Region")           # konvertera korrekta läns- och kommunkoder till de löpnummer som används i denna databas

  # Hantera tid-koder
  giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "tid")
  tid_vekt <- if (all(tid_koder != "*")) tid_koder %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar] %>% unique() else giltiga_ar

  # query-lista till pxweb-uttag
  varlista <- list(
  	"Tid" = tid_vekt,
  	"Region" = region_vekt,
  	"Bransch" = bransch_vekt,
  	"Juridisk_form" = juridisk_form_vekt,
  	"variabel" = variabel_vekt)

  if (all(is.na(bransch_klartext))) varlista <- varlista[names(varlista) != "Bransch"]
  if (all(is.na(juridisk_form_klartext))) varlista <- varlista[names(varlista) != "Juridisk_form"]

  # Hämta data med varlista
  px_uttag <- pxweb_get(url = url_uttag, query = varlista)

  # Kod om inte verkar uppfylla någon funktion. Kommenteras bort
  # var_vektor <- c(regionkod = "Region")
  # var_vektor_klartext <- "region"

  # gör om pxweb-uttaget till en dataframe
  px_df <- as.data.frame(px_uttag)
  px_df <- px_df %>% region_kolumn_splitta_kod_klartext("region")
  
  # Kod om inte verkar uppfylla någon funktion. Kommenteras bort
  # if (!all(is.na(var_vektor))) {
  #     # om man vill ha med koder också för variabler utöver klartext så läggs de på här (om det finns värden i var_vektor)
  #     px_df <- px_df %>%
  #           cbind(as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>%
  #           select(any_of(var_vektor)))
  # 
  #     # kolumnerna med koder läggs framför motsvarande kolumner med klartext
  #     for (varflytt_index in 1:length(var_vektor)) {
  #       px_df <- px_df %>%
  #           relocate(any_of(names(var_vektor)[varflytt_index]), .before = any_of(var_vektor_klartext[varflytt_index]))
  #     }
  # }

  # Om användaren vill spara data till en Excel-fil
  if (!is.na(output_mapp) & !is.na(excel_filnamn)){
    write.xlsx(px_df, paste0(output_mapp, excel_filnamn))
  }

  # Returnera data som en dataframe om användern valt det
  if (returnera_df) return(px_df)
} # slut hämta data-funktion
