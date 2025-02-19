hamta_yrke_region_ssyk3_kon_tid_scb <- function(
			region_vekt = "20",			# Val av region. Finns: "00", "01", "0114", "0115", "0117", "0120", "0123", "0125", "0126", "0127", "0128", "0136", "0138", "0139", "0140", "0160", "0162", "0163", "0180", "0181", "0182", "0183", "0184", "0186", "0187", "0188", "0191", "0192", "03", "0305", "0319", "0330", "0331", "0360", "0380", "0381", "0382", "04", "0428", "0461", "0480", "0481", "0482", "0483", "0484", "0486", "0488", "05", "0509", "0512", "0513", "0560", "0561", "0562", "0563", "0580", "0581", "0582", "0583", "0584", "0586", "06", "0604", "0617", "0642", "0643", "0662", "0665", "0680", "0682", "0683", "0684", "0685", "0686", "0687", "07", "0760", "0761", "0763", "0764", "0765", "0767", "0780", "0781", "08", "0821", "0834", "0840", "0860", "0861", "0862", "0880", "0881", "0882", "0883", "0884", "0885", "09", "0980", "10", "1060", "1080", "1081", "1082", "1083", "12", "1214", "1230", "1231", "1233", "1256", "1257", "1260", "1261", "1262", "1263", "1264", "1265", "1266", "1267", "1270", "1272", "1273", "1275", "1276", "1277", "1278", "1280", "1281", "1282", "1283", "1284", "1285", "1286", "1287", "1290", "1291", "1292", "1293", "13", "1315", "1380", "1381", "1382", "1383", "1384", "14", "1401", "1402", "1407", "1415", "1419", "1421", "1427", "1430", "1435", "1438", "1439", "1440", "1441", "1442", "1443", "1444", "1445", "1446", "1447", "1452", "1460", "1461", "1462", "1463", "1465", "1466", "1470", "1471", "1472", "1473", "1480", "1481", "1482", "1484", "1485", "1486", "1487", "1488", "1489", "1490", "1491", "1492", "1493", "1494", "1495", "1496", "1497", "1498", "1499", "17", "1715", "1730", "1737", "1760", "1761", "1762", "1763", "1764", "1765", "1766", "1780", "1781", "1782", "1783", "1784", "1785", "18", "1814", "1860", "1861", "1862", "1863", "1864", "1880", "1881", "1882", "1883", "1884", "1885", "19", "1904", "1907", "1960", "1961", "1962", "1980", "1981", "1982", "1983", "1984", "20", "2021", "2023", "2026", "2029", "2031", "2034", "2039", "2061", "2062", "2080", "2081", "2082", "2083", "2084", "2085", "21", "2101", "2104", "2121", "2132", "2161", "2180", "2181", "2182", "2183", "2184", "22", "2260", "2262", "2280", "2281", "2282", "2283", "2284", "23", "2303", "2305", "2309", "2313", "2321", "2326", "2361", "2380", "24", "2401", "2403", "2404", "2409", "2417", "2418", "2421", "2422", "2425", "2460", "2462", "2463", "2480", "2481", "2482", "25", "2505", "2506", "2510", "2513", "2514", "2518", "2521", "2523", "2560", "2580", "2581", "2582", "2583", "2584", "99", "9999"
			yrke2012_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "Officerare", "Specialistofficerare", "Soldater m.fl.", "Politiker och högre ämbetsmän", "Verkställande direktörer m.fl.", "Ekonomi- och finanschefer", "Personal- och HR-chefer", "Förvaltnings- och planeringschefer", "Informations-, kommunikations- och PR-chefer", "Försäljnings- och marknadschefer", "Övriga administrations- och servicechefer", "IT-chefer", "Inköps-, logistik- och transportchefer", "Forsknings- och utvecklingschefer", "Chefer inom arkitekt- och ingenjörsverksamhet", "Fastighets- och förvaltningschefer", "Driftchefer inom bygg, anläggning och gruva", "Produktionschefer inom tillverkning", "Förvaltare inom skogsbruk och lantbruk m.fl.", "Chefer inom grund- och gymnasieskola samt vuxenutbildning", "Chefer inom förskoleverksamhet", "Övriga chefer inom utbildning", "Chefer inom hälso- och sjukvård", "Chefer inom socialt och kurativt arbete", "Chefer inom äldreomsorg", "Chefer och ledare inom trossamfund", "Övriga chefer inom samhällsservice", "Chefer inom bank, finans och försäkring", "Hotell- och konferenschefer", "Restaurang- och kökschefer", "Chefer inom handel", "Chefer inom friskvård, sport och fritid", "Chefer inom övrig servicenäring", "Fysiker och kemister m.fl.", "Matematiker, aktuarier och statistiker", "Biologer, farmakologer och specialister inom lant- och skogsbruk m.fl.", "Civilingenjörsyrken", "Arkitekter och lantmätare", "Designer och formgivare", "Specialister inom miljö- och hälsoskydd", "Läkare", "Sjuksköterskor", "Sjuksköterskor (fortsättning)", "Psykologer och psykoterapeuter", "Veterinärer", "Tandläkare", "Naprapater, sjukgymnaster och arbetsterapeuter m.fl.", "Andra specialister inom hälso- och sjukvård", "Universitets- och högskollärare", "Lärare i yrkesämnen", "Gymnasielärare", "Grundskollärare, fritidspedagoger och förskollärare", "Andra pedagoger med teoretisk specialistkompetens", "Revisorer, finansanalytiker och fondförvaltare m.fl.", "Organisationsutvecklare, utredare och HR-specialister m.fl.", "Marknadsförare och informatörer m.fl.", "IT-arkitekter, systemutvecklare och testledare m.fl.", "Jurister", "Museiintendenter och bibliotekarier m.fl.", "Författare, journalister och tolkar m.fl.", "Konstnärer, musiker och skådespelare m.fl.", "Socialsekreterare och kuratorer m.fl.", "Präster och diakoner", "Ingenjörer och tekniker", "Arbetsledare inom bygg och tillverkning m.m.", "Piloter, fartygs- och maskinbefäl m.fl.", "Biomedicinska analytiker, tandtekniker och laboratorieingenjörer m.fl.", "Terapeuter inom alternativmedicin", "Djursjukskötare m.fl.", "Tandhygienister", "Banktjänstemän och redovisningsekonomer m.fl.", "Försäkringsrådgivare, företagssäljare och inköpare m.fl.", "Förmedlare m.fl.", "Juristsekreterare, chefssekreterare och institutionssekreterare m.fl.", "Skatte- och socialförsäkringshandläggare m.fl.", "Poliser", "Behandlingsassistenter och pastorer", "Idrottsutövare och fritidsledare m.fl.", "Fotografer, dekoratörer och underhållningsartister m.fl.", "Trafiklärare och instruktörer", "Köksmästare och souschefer", "Drift-, support- och nätverkstekniker m.fl.", "Bild-, ljud- och ljustekniker m.fl.", "Kontorsassistenter och sekreterare", "Croupierer och inkasserare", "Resesäljare, kundtjänstpersonal och receptionister m.fl.", "Lagerpersonal och transportledare m.fl.", "Biblioteks- och arkivassistenter m.fl.", "Brevbärare och postterminalarbetare", "Förtroendevalda", "Kabinpersonal, tågmästare och guider m.fl.", "Kockar och kallskänkor", "Hovmästare, servitörer och bartendrar", "Skönhets- och kroppsterapeuter", "Städledare och fastighetsskötare m.fl.", "Övrig servicepersonal", "Butikspersonal", "Kassapersonal m.fl.", "Eventsäljare och telefonförsäljare m.fl.", "Barnskötare och elevassistenter m.fl.", "Undersköterskor", "Vårdbiträden", "Skötare, vårdare och personliga assistenter m.fl.", "Tandsköterskor", "Andra bevaknings- och säkerhetsyrken", "Växtodlare inom jordbruk och trädgård", "Djuruppfödare och djurskötare", "Växtodlare och djuruppfödare, blandad drift", "Skogsarbetare", "Fiskodlare och fiskare", "Snickare, murare och anläggningsarbetare", "Takmontörer, golvläggare och VVS-montörer m.fl.", "Målare, lackerare och skorstensfejare m.fl.", "Gjutare, svetsare och plåtslagare m.fl.", "Smeder och verktygsmakare m.fl.", "Fordonsmekaniker och reparatörer m.fl.", "Finmekaniker och konsthantverkare m.fl.", "Prepresstekniker, tryckare och bokbindare m.fl.", "Installations- och industrielektriker m.fl.", "Elektronikreparatörer och kommunikationselektriker m.fl.", "Ytbehandlare, trä och möbelsnickare m.fl.", "Skräddare, tapetserare och läderhantverkare m.fl.", "Slaktare, bagare och livsmedelsförädlare m.fl.", "Malmförädlingsyrken och brunnsborrare m.fl.", "Process- och maskinoperatörer, vid stål- och metallverk", "Maskinoperatörer, kemiska och farmaceutiska produkter m.m.", "Maskinoperatörer, gummi-, plast- och pappersvaruindustri", "Maskinoperatörer, textil-, tvätt- och läderindustri m.m.", "Maskinoperatörer, livsmedelsindustri", "Processoperatörer, trä- och pappersindustri", "Andra process- och maskinoperatörer", "Drifttekniker och processövervakare", "Montörer", "Lokförare och bangårdspersonal", "Bil-, motorcykel och cykelförare", "Lastbils- och bussförare", "Maskinförare", "Matroser och jungmän m.fl.", "Städare och hemservicepersonal m.fl.", "Tvättare, fönsterputsare och övriga rengöringsarbetare", "Bärplockare och plantörer m.fl.", "Grovarbetare inom bygg och anläggning", "Handpaketerare och andra fabriksarbetare", "Hamnarbetare och ramppersonal m.fl.", "Snabbmatspersonal, köks- och restaurangbiträden m.fl.", "Torg- och marknadsförsäljare", "Återvinningsarbetare", "Tidningsdistributörer, vaktmästare och övriga servicearbetare", "yrke okänt"
			kon_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "män", "kvinnor"
			cont_klartext = "*",			 #  Finns: "Anställda 16-64 år med arbetsplats i regionen (dagbef)"
			tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2020", "2021", "2022"
			output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
			excel_filnamn = "yrke.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
			returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
){

  # ====================================================================================================
  #
  # Funktion för att hämta data från SCB:s API med hjälp av pxweb-paketet
  # Automatgenererat av en funktion i R som skrivits av Peter Möller, Region Dalarna
  #
  # Skapad av: moepet den 02 september 2024
  # Senast uppdaterad: 02 september 2024
  #
  # url till tabellens API: https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0208/AM0208D/YREG58BAS
  #												https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0208/AM0208D/YREG58N
  #												https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0208/AM0208D/YREG58
  #
  # ====================================================================================================

  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
    			tidyverse,
    			writexl)

  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

  # Url till SCB:s databas
  url_list <- c("https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0208/AM0208D/YREG58BAS",
						"https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0208/AM0208D/YREG58N",
						"https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0208/AM0208D/YREG58")

  giltiga_ar <- map(url_list, ~hamta_giltiga_varden_fran_tabell(.x, "tid")) %>% unlist()
  if (all(tid_koder != "*")) tid_koder <- tid_koder %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar] %>% unique() else tid_vekt <- giltiga_ar
  
  if (length(tid_koder) < 1) stop("Inga giltiga år i tid_koder, kontrollera parametern och försök igen.")
  
 hamta_data <- function(url_uttag) {

  px_meta <- pxweb_get(url_uttag)

  varlist_koder <- pxvarlist(px_meta)$koder
  varlist_bada <- pxvarlist(px_meta)

  # Gör om från klartext till kod som databasen förstår
  yrke2012_vekt <- if (!all(is.na(yrke2012_klartext))) hamta_kod_med_klartext(px_meta, yrke2012_klartext, skickad_fran_variabel = "yrke2012") else NA
  kon_vekt <- if (!all(is.na(kon_klartext))) hamta_kod_med_klartext(px_meta, kon_klartext, skickad_fran_variabel = "kon") else NA

  cont_vekt <-  hamta_kod_med_klartext(px_meta, cont_klartext, "contentscode")
  if (length(cont_vekt) > 1) wide_om_en_contvar <- FALSE

  # Hantera tid-koder
  giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "tid")
  if (all(tid_koder != "*")) tid_vekt <- tid_koder %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar] %>% unique() else tid_vekt <- giltiga_ar
  
  # special, ta bort år 2020 och 2021 om det är RAMS-tabellen (annars blir de dubbelt)
  if (url_uttag == "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0208/AM0208D/YREG58N") {
    if (tid_koder == "*") tid_vekt <- giltiga_ar
    tid_vekt <- tid_vekt %>% .[. != "2020" & . != "2021"]
  }
  
  if (length(tid_vekt) > 0) {
    
    # query-lista till pxweb-uttag
    varlista <- list(
    "Region" = region_vekt,
    "Yrke2012" = yrke2012_vekt,
    "Kon" = kon_vekt,
    "ContentsCode" = cont_vekt,
    "Tid" = tid_vekt)
  
    if (all(is.na(yrke2012_klartext))) varlista <- varlista[names(varlista) != "Yrke2012"]
    if (all(is.na(kon_klartext))) varlista <- varlista[names(varlista) != "Kon"]
  
    px_uttag <- pxweb_get(url = url_uttag, query = varlista)
  
    var_vektor <- c(regionkod = "Region", yrkeskod = "Yrke2012")
    var_vektor_klartext <- c("region", "Yrke (SSYK 2012)")
  
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
  
    return(px_df)
   } # test om det finns år kvar i aktuellt uttag
  } # slut hamta data-funktion

  px_alla <- map(url_list, ~ hamta_data(.x)) %>% list_rbind()

  # Om användaren vill spara data till en Excel-fil
  if (!is.na(output_mapp) & !is.na(excel_filnamn)){
    write.xlsx(px_alla, paste0(output_mapp, excel_filnamn))
  }

  # Returnera data som en dataframe om användern valt det
  if (returnera_df) return(px_alla)

}
