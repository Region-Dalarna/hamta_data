hamta_studieforbund_region_arrangemangstyp_verksamhetsform_mm_tid_scb <- function(
    region_vekt = "20",			# Val av region. Finns: "00", "01", "0114", "0115", "0117", "0120", "0123", "0125", "0126", "0127", "0128", "0136", "0138", "0139", "0140", "0160", "0162", "0163", "0180", "0181", "0182", "0183", "0184", "0186", "0187", "0188", "0191", "0192", "03", "0305", "0319", "0330", "0331", "0360", "0380", "0381", "0382", "04", "0428", "0461", "0480", "0481", "0482", "0483", "0484", "0486", "0488", "05", "0509", "0512", "0513", "0560", "0561", "0562", "0563", "0580", "0581", "0582", "0583", "0584", "0586", "06", "0604", "0617", "0642", "0643", "0662", "0665", "0680", "0682", "0683", "0684", "0685", "0686", "0687", "07", "0760", "0761", "0763", "0764", "0765", "0767", "0780", "0781", "08", "0821", "0834", "0840", "0860", "0861", "0862", "0880", "0881", "0882", "0883", "0884", "0885", "09", "0980", "10", "1060", "1080", "1081", "1082", "1083", "12", "1214", "1230", "1231", "1233", "1256", "1257", "1260", "1261", "1262", "1263", "1264", "1265", "1266", "1267", "1270", "1272", "1273", "1275", "1276", "1277", "1278", "1280", "1281", "1282", "1283", "1284", "1285", "1286", "1287", "1290", "1291", "1292", "1293", "13", "1315", "1380", "1381", "1382", "1383", "1384", "14", "1401", "1402", "1407", "1415", "1419", "1421", "1427", "1430", "1435", "1438", "1439", "1440", "1441", "1442", "1443", "1444", "1445", "1446", "1447", "1452", "1460", "1461", "1462", "1463", "1465", "1466", "1470", "1471", "1472", "1473", "1480", "1481", "1482", "1484", "1485", "1486", "1487", "1488", "1489", "1490", "1491", "1492", "1493", "1494", "1495", "1496", "1497", "1498", "1499", "17", "1715", "1730", "1737", "1760", "1761", "1762", "1763", "1764", "1765", "1766", "1780", "1781", "1782", "1783", "1784", "1785", "18", "1814", "1860", "1861", "1862", "1863", "1864", "1880", "1881", "1882", "1883", "1884", "1885", "19", "1904", "1907", "1960", "1961", "1962", "1980", "1981", "1982", "1983", "1984", "20", "2021", "2023", "2026", "2029", "2031", "2034", "2039", "2061", "2062", "2080", "2081", "2082", "2083", "2084", "2085", "21", "2101", "2104", "2121", "2132", "2161", "2180", "2181", "2182", "2183", "2184", "22", "2260", "2262", "2280", "2281", "2282", "2283", "2284", "23", "2303", "2305", "2309", "2313", "2321", "2326", "2361", "2380", "24", "2401", "2403", "2404", "2409", "2417", "2418", "2421", "2422", "2425", "2460", "2462", "2463", "2480", "2481", "2482", "25", "2505", "2506", "2510", "2513", "2514", "2518", "2521", "2523", "2560", "2580", "2581", "2582", "2583", "2584"
    arrangemangstyp_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "Totalt", "Folkbildning", "Asylsökande/förläggningsboende", "Vardagssvenska", "Uppdragsverksamhet finansierat via kunskapslyftet", "Regionfinansierad utbildningsverksamhet", "Uppsökande och motiverande insatser               ", "Föräldrars delaktighet i lärandet", "Övrig uppdragsverksamhet", "Annan verksamhet", "Svenska för föräldralediga", "Arbetsmarknadsnära insatser på distans", "Studieförbundens insatser med personer långt från arbetsmarknaden"
    verksamhetsform_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "Totalt", "Studiecirkel", "Kulturprogram", "Annan folkbildningsverksamhet", "Uppsökande verksamhet", "Behörighetsgivande utbildning", "Fri", "Orienteringskurser"
    studieforbund_klartext = "*",			 #  Finns: "Samtliga förbund", "Samtliga distrikt", "Samtliga avdelningar", "Arbetarnas Bildningsförbund (ABF)", "ABF Stockholm", "ABF Botkyrka-Salem", "ABF Huddinge", "ABF Norrtälje", "ABF Norra Stockholms Län", "ABF Södertälje-Nykvarn", "ABF Södertörn", "ABF i Uppsala län", "ABF Sörmland", "ABF Flen", "ABF Östergötland", "ABF Jönköpings län", "ABF Kronoberg", "ABF Södra Småland", "ABF Kalmar län", "ABF Gotland", "ABF Blekinge län", "ABF Helsingborg", "ABF Hörby", "ABF MittSkåne", "ABF Skåne Nordost", "ABF Nordvästskåne", "ABF Sydvästra Skåne", "ABF Österlen", "ABF Malmö", "ABF Halland", "ABF Göteborg", "ABF FyrBoDal", "ABF Sydvästra Götaland", "ABF Stockholms län", "ABF Sjuhärad", "ABF Skaraborg", "ABF Värmland", "ABF Norra Värmland", "ABF Örebro län", "ABF Västra Västmanland", "ABF Västerås", "ABF Södra- Östra Dalarna", "ABF Borlänge-Nedansiljan", "ABF Dala Finnmark", "ABF Västra Hälsingland", "ABF Gästrikebygden", "ABF Hofors", "ABF Hälsingekusten", "ABF Bräcke", "ABF Västernorrland", "ABF JH", "ABF Ånge", "ABF Skellefteå", "ABF Umeåregionen", "ABF Mitt i Lappland", "ABF Norr", "ABF Uppsala", "ABF Södermanland", "ABF Distriktet i Stockholmsregionen", "ABF Skåne", "ABF Västra Götaland", "ABF Dalarna", "ABF Gävleborg", "ABF Västerbotten", "ABF Förbundsexpeditionen", "ABF Jönköping", "ABF Kronobergs län", "ABF Blekinge", "ABF Hallands län", "ABF Västmanland", "ABF Västernorrlands län", "ABF Jämtland", "ABF Norrbotten", "ABF Förbundet", "Bilda", "Bilda Sydöst", "Bilda Sydväst", "Bilda Svealand", "Bilda Öst", "Bilda Mitt", "Bilda Nord", "Folkuniversitetet (FU)", "FU Öst", "FU Stockholm Balettakademien", "FU Stockholm Estet", "FU Stockholm Teori", "FU Stockholm Uppdrag", "FU Stockholm SUS", "FU Stockholm Studieservice", "FU Stockholms Län", "FU Stockholm Studierådet", "FU Sörmland", "FU Linköping", "FU Norrköping", "FU Jönköping", "FU Gotland", "FU Mitt", "FU Uppsala", "FU Västmanland", "FU Örebro", "FU Gävleborg", "FU Dalarna", "FU Väst", "FU Region Öst", "FU Region Mitt", "FU Region Väst", "FU Region Syd", "FU Region Norr", "FU Göteborg GSU", "FU Göteborg", "FU Göteborg Balettakademien", "FU Varberg", "FU Kungsbacka", "FU Alingsås", "FU Borås", "FU Trollhättan", "FU Karlstad", "FU Syd", "FU Malmö", "FU Trelleborg", "FU Landskrona", "FU Helsingborg", "FU Växjö", "FU Kalmar", "FU Karlskrona", "FU Kristianstad", "FU Lund", "FU Halmstad", "FU Falkenberg", "FU Norr", "FU Umeå region", "FU Västerbotten", "FU Norrbotten", "FU Västernorrland", "FU Jämtland", "Ibn Rushd (IR)", "Ibn Rushd Norra", "Ibn Rushd Mitt", "Ibn Rushd Östra", "Ibn Rushd GSU", "Ibn Rushd Västra", "Ibn Rushd Södra", "Ibn Rushd Akademin", "Kulturens Bildningsverksamhet (KBV)", "KBV Region Norr", "Sfr Stockholms län", "KBV Region Mitt", "KBV Region Öst", "Sfr Uppsala Västmanland", "KBV Region Väst", "Sfr Sörmland", "KBV Region Syd", "Sfr Östergötland", "KBV Region Mitt Norr", "Sfr Småland & Gotland", "Sfr Skåne-Blekinge", "Sfr Västra Skåne", "Sfr Halland", "Sfr Göteborg Sjuhärad", "Sfr Fyrbodal", "Sfr Bohuslän Norra", "Sfr Skaraborg", "Sfr Norra Älvsborg", "Sfr Utbildningscenter Väst", "Sfr Lidköping-Skarabygden", "Sfr Södra Skaraborg-Ulricehamn", "Sfr Norra Skaraborg", "Sfr Örebro-Värmland", "Sfr Västmanland", "Sfr Mitt", "Sfr Västerbotten", "Sfr Norrbotten", "Medborgarskolan (MBSK)", "Medborgarskolan Stockholmsregionen", "Medborgarskolan Mälardalen", "Medborgarskolan Region Ost", "Medborgarskolan Syd", "Medborgarskolan Region Väst", "Medborgarskolan Mitt", "Medborgarskolan Gamla Mitt", "Medborgarskolan Nord", "Medborgarskolan Region Syd Utbildning AB", "Medborgarskolan - Tillskärarakademin Malmö AB (Förbund-Distrikt) ", "Nykterhetsrörelsens Bildningsverksamhet (NBV)", "Nykterhetsrörelsens bildningsverksamhet (inget distrikt)", "Nykterhetsrörelsens bildningsverksamhet (förbund-distrikt)", "SV Västmanland", "SV Gotland", "SV Örebro län", "SV Uppsala län", "SV Sörmland", "SV Blekinge", "SV Dalarna", "SV Gävleborg", "SV Göteborg", "SV Göteborgsregionen Sydost", "SV Halland", "SV Jämtlands län", "SV Jönköpings län", "SV Kalmar län", "SV Kronoberg", "SV Lundabygden", "SV Malmö", "SV Skåneland", "SV Norrbotten", "SV Sjuhärad", "SV Skaraborg", "SV Stockholm", "SV Stockholms län", "SV Värmland", "SV Väst", "SV Västerbotten", "SV Västernorrland", "SV Östergötland", "SV Regionförbund Västra Götaland", "Sensus", "NBV Syd", "NBV Sydost", "NBV Väst", "NBV Öst", "NBV Mitt", "NBV Norr", "Sensus Svealand", "Sensus Östra Götaland", "Sensus Skåne-Blekinge", "Sensus Västra Sverige", "Sensus Norrland", "Sensus Stockholm-Gotland", "Sensus Riksförbund (Förbund-Distrikt)", "Studiefrämjandet (SFR)", "Sfr Uppsala län", "Sfr Södermanland", "Sfr Östergötlands län", "Sfr Småland-Gotland", "Sfr Väst", "Sfr Västmanlands län", "Studieförbundet Vuxenskolan (SV)", "Studieförbundet vuxenskolan (inget distrikt)", "Mbsk Stockholm", "Mbsk Lidingö", "Mbsk Norrtäljekontoret", "Mbsk Kulturama", "Mbsk Gotland", "Mbsk Tillskärarakademin", "Mbsk Stockholmsregionen", "Mbsk Västmanland", "Mbsk Enköping", "Mbsk Uppsala", "Mbsk Eskilstuna", "Mbsk Strängnäs", "Mbsk Nyköping", "Mbsk Katrineholm", "Mbsk Mälardalen-regionen", "Mbsk Kalmar", "Mbsk Karlskrona", "Mbsk Ljungby", "Mbsk Nässjö", "Mbsk Växjö", "Mbsk Vetlanda-Eksjö", "Mbsk Värnamo", "Mbsk Gislaved", "Mbsk Jönköping", "Mbsk Linköping", "Mbsk Mjölby", "Mbsk Motala-Vadstena", "Mbsk Norrköping", "Mbsk Västervik-Vimmerby", "Mbsk Sävsjö", "Mbsk Region Ost", "Mbsk Syd", "Mbsk Ledarutbildningar Syd", "Mbsk Halmstad/Hylte", "Mbsk Falkenberg", "Mbsk Varberg", "Mbsk Kungsbacka", "Mbsk Göteborg", "Mbsk Stenungsund", "Mbsk Kungälv", "Mbsk Borås", "Mbsk Lidköping", "Mbsk Skara", "Mbsk Mariestad", "Mbsk Skövde", "Mbsk Fyrbodal", "Mbsk Alingsås", "Mbsk Region Väst", "Mbsk Örebro", "Mbsk Regional musik", "Mbsk Västra Värmland", "Mbsk Värmlands län", "Mbsk Värmland-Örebro län", "Mbsk Värmland-Örebro", "Mbsk Kultur i vården", "Mbsk Marie Kühlers showdansskola Entré", "Mbsk Mälardalen", "Mbsk Dalarna Syd", "Mbsk Falun-Borlänge", "Mbsk Gästrikland", "Mbsk Sundsvall, gamla", "Mbsk Örnsköldsvik, gamla", "Mbsk Ådalen, gamla", "Mbsk Gästrikland, gamla", "Mbsk Dalarna Syd, gamla", "Mbsk Hälsingland, gamla", "Mbsk Falun-Borlänge, gamla", "Mbsk Jämtland-Härjedalen, gamla", "Mbsk Region Mitt, gamla", "Mbsk Umeå", "Mbsk Skellefteå", "Mbsk Lycksele", "Mbsk Vilhelmina", "Mbsk Storuman", "Mbsk Luleå", "Mbsk Piteå", "Mbsk Boden", "Mbsk Kiruna", "Mbsk Gällivare", "Mbsk ÖvertorneåHaparanda", "Mbsk Hälsingland", "Mbsk Jämtland-Härjedalen", "Mbsk Sundsvall", "Mbsk Ådalen", "Mbsk Örnsköldsvik", "Mbsk Nord", "Mbsk Region Syd Utbildning AB", "Sensus Skåne - Blekinge", "Sensus Stockholm - Gotland", "Ibn Rushd SU", "Ibn Rushd Akademin - Riks", "Kulturens Region Norr", "Kulturens Region Mitt", "Kulturens Region Öst", "Kulturens Region Väst", "Kulturens Region Syd", "Kulturens Region Mitt Norr"
    distansellerej_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "distans", "ej distans", "totalt"
    cont_klartext = "*",			 #  Finns: "Arrangemang", "Studietimmar", "Studietimmar, funktionshindrade", "Studietimmar, utrikesfödda med brister i svenska", "Deltagartimmar", "Deltagare", "Deltagare, kvinnor", "Deltagare, män", "Deltagare, funktionshindrade", "Deltagare, utrikesfödda med brister i svenska"
    tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2019", "2020", "2021", "2022", "2023"
    long_format = TRUE,			# TRUE = konvertera innehållsvariablerna i datasetet till long-format 
    wide_om_en_contvar = TRUE,			# TRUE = om man vill behålla wide-format om det bara finns en innehållsvariabel, FALSE om man vill konvertera till long-format även om det bara finns en innehållsvariabel
    output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
    excel_filnamn = "studieforbund.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
    returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
){
  
  # ====================================================================================================
  #
  # Funktion för att hämta data från SCB:s API med hjälp av pxweb-paketet
  # Automatgenererat av en funktion i R som skrivits av Peter Möller, Region Dalarna
  #
  # Skapad av: frkjon den 03 juli 2024
  # Senast uppdaterad: 03 juli 2024
  #
  # url till tabellens API: https://api.scb.se/OV0104/v1/doris/sv/ssd/START/KU/KU0402/StudieforbHelarLanKo
  # Källa: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__KU__KU0402/StudieforbHelarLanKo/
  # ====================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
         tidyverse,
         writexl)
  
  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  # Url till SCB:s databas
  url_uttag <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/KU/KU0402/StudieforbHelarLanKo"
  px_meta <- pxweb_get(url_uttag)
  
  varlist_koder <- pxvarlist(px_meta)$koder
  varlist_bada <- pxvarlist(px_meta)
  
  # Gör om från klartext till kod som databasen förstår
  arrangemangstyp_vekt <- if (!all(is.na(arrangemangstyp_klartext))) hamta_kod_med_klartext(px_meta, arrangemangstyp_klartext, skickad_fran_variabel = "arrangemangstyp") else NA
  verksamhetsform_vekt <- if (!all(is.na(verksamhetsform_klartext))) hamta_kod_med_klartext(px_meta, verksamhetsform_klartext, skickad_fran_variabel = "verksamhetsform") else NA
  studieforbund_vekt <- hamta_kod_med_klartext(px_meta, studieforbund_klartext, skickad_fran_variabel = "studieforbund")
  distansellerej_vekt <- if (!all(is.na(distansellerej_klartext))) hamta_kod_med_klartext(px_meta, distansellerej_klartext, skickad_fran_variabel = "distansellerej") else NA
  
  cont_vekt <-  hamta_kod_med_klartext(px_meta, cont_klartext, "contentscode")
  if (length(cont_vekt) > 1) wide_om_en_contvar <- FALSE
  
  # Hantera tid-koder
  giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "tid")
  if (all(tid_koder != "*")) tid_koder <- tid_koder %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar] %>% unique()
  
  # query-lista till pxweb-uttag
  varlista <- list(
    "Region" = region_vekt,
    "Arrangemangstyp" = arrangemangstyp_vekt,
    "Verksamhetsform" = verksamhetsform_vekt,
    "Studieforbund" = studieforbund_vekt,
    "Distansellerej" = distansellerej_vekt,
    "ContentsCode" = cont_vekt,
    "Tid" = tid_koder)
  
  if (all(is.na(arrangemangstyp_klartext))) varlista <- varlista[names(varlista) != "Arrangemangstyp"]
  if (all(is.na(verksamhetsform_klartext))) varlista <- varlista[names(varlista) != "Verksamhetsform"]
  if (all(is.na(distansellerej_klartext))) varlista <- varlista[names(varlista) != "Distansellerej"]
  
  px_uttag <- pxweb_get(url = url_uttag, query = varlista)
  
  var_vektor <- c(regionkod = "Region")
  var_vektor_klartext <- "region"
  
  px_df <- as.data.frame(px_uttag)
  if (!all(is.na(var_vektor))) {
    # om man vill ha med koder också för variabler utöver klartext så läggs de på här (om det finns värden i var_vektor)
    px_df <- px_df %>%
      cbind(as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>%
              select(any_of(var_vektor)))
    
    # kolumnerna med koder läggs framför motsvarande kolumner med klartext
    for (varflytt_index in 1:length(var_vektor)) {
      px_df <- px_df %>%
        relocate(all_of(names(var_vektor)[varflytt_index]), .before = all_of(var_vektor_klartext[varflytt_index]))
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
  
}
