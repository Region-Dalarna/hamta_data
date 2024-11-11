hamta_yrke_SSYK4_region_yrke2012_kon_tid_scb <- function(
			region_vekt = "20",			   # Val av region. Finns: "00", "01", "03", "04", "05", "06", "07", "08", "09", "10", "12", "13", "14", "17", "18", "19", "20", "21", "22", "23", "24", "25", "99"
			yrke2012_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "Officerare", "Specialistofficerare", "Soldater m.fl.", "Politiker", "General-, landstings- och kommundirektörer m.fl.", "Chefstjänstemän i intresseorganisationer", "Verkställande direktörer m.fl.", "Ekonomi- och finanschefer, nivå 1", "Ekonomi- och finanschefer, nivå 2", "Personal- och HR-chefer, nivå 1", "Personal- och HR-chefer, nivå 2", "Förvaltnings- och planeringschefer", "Informations-, kommunikations- och PR-chefer, nivå 1", "Informations-, kommunikations - och PR-chefer, nivå 2", "Försäljnings- och marknadschefer, nivå 1", "Försäljnings- och marknadschefer, nivå 2", "Övriga administrations- och servicechefer, nivå 1", "Övriga administrations- och servicechefer, nivå 2", "IT-chefer, nivå 1", "IT-chefer, nivå 2", "Inköps-, logistik- och transportchefer, nivå 1", "Inköps-, logistik- och transportchefer, nivå 2", "Forsknings- och utvecklingschefer, nivå 1", "Forsknings- och utvecklingschefer, nivå 2", "Chefer inom arkitekt- och ingenjörsverksamhet, nivå 1", "Chefer inom arkitekt- och ingenjörsverksamhet, nivå 2", "Fastighets- och förvaltningschefer, nivå 1", "Fastighets- och förvaltningschefer, nivå 2", "Driftchefer inom bygg, anläggning och gruva, nivå 1", "Driftchefer inom bygg, anläggning och gruva, nivå 2", "Produktionschefer inom tillverkning, nivå 1", "Produktionschefer inom tillverkning, nivå 2", "Förvaltare inom skogsbruk och lantbruk m.fl.", "Avdelningschefer inom grund- och gymnasieskola samt vuxenutbildning, nivå 1", "Rektorer, nivå 2", "Avdelningschefer inom förskola, nivå 1", "Förskolechefer, nivå 2", "Övriga avdelningschefer inom utbildning, nivå 1", "Övriga verksamhetschefer inom utbildning, nivå 2", "Klinik- och verksamhetschefer inom hälsa och sjukvård, nivå 1", "Avdelnings- och enhetschefer inom hälsa och sjukvård, nivå 2", "Avdelningschefer inom socialt och kurativt arbete, nivå 1", "Enhetschefer inom socialt och kurativt arbete, nivå 2", "Avdelningschefer inom äldreomsorg, nivå 1", "Enhetschefer inom äldreomsorg, nivå 2", "Chefer och ledare inom trossamfund", "Övriga chefer inom samhällsservice, nivå 1", "Övriga verksamhetschefer inom samhällsservice, nivå 2", "Chefer inom bank, finans och försäkring, nivå 1", "Chefer inom bank, finans och försäkring, nivå 2", "Hotell- och konferenschefer, nivå 1", "Hotell- och konferenschefer, nivå 2", "Restaurang- och kökschefer, nivå 1", "Restaurang- och kökschefer, nivå 2", "Chefer inom handel, nivå 1", "Chefer inom handel, nivå 2", "Chefer inom friskvård, sport och fritid, nivå 1", "Chefer inom friskvård, sport och fritid, nivå 2", "Chefer inom övrig servicenäring, nivå 1", "Chefer inom övrig servicenäring, nivå 2", "Fysiker och astronomer", "Meteorologer", "Kemister", "Geologer och geofysiker m.fl.", "Matematiker och aktuarier", "Statistiker", "Cell- och molekylärbiologer m.fl.", "Växt- och djurbiologer", "Farmakologer och biomedicinare", "Specialister och rådgivare inom lantbruk m.m.", "Specialister och rådgivare inom skogsbruk", "Civilingenjörsyrken inom logistik och produktionsplanering", "Civilingenjörsyrken inom bygg och anläggning", "Civilingenjörsyrken inom elektroteknik", "Civilingenjörsyrken inom maskinteknik", "Civilingenjörsyrken inom kemi och kemiteknik", "Civilingenjörsyrken inom gruvteknik och metallurgi", "Övriga civilingenjörsyrken", "Arkitekter m.fl.", "Landskapsarkitekter", "Planeringsarkitekter m.fl.", "Lantmätare", "Industridesigner", "Grafisk formgivare m.fl.", "Designer inom spel och digitala medier", "Övriga designer och formgivare", "Arbetsmiljöingenjörer, yrkes- och miljöhygieniker", "Miljö- och hälsoskyddsinspektörer", "Specialister inom miljöskydd och miljöteknik", "Specialistläkare", "ST-läkare", "AT-läkare", "Övriga läkare", "Grundutbildade sjuksköterskor", "Barnmorskor", "Anestesisjuksköterskor", "Distriktssköterskor", "Psykiatrisjuksköterskor", "Ambulanssjuksköterskor m.fl.", "Geriatriksjuksköterskor", "Intensivvårdssjuksköterskor", "Operationssjuksköterskor", "Barnsjuksköterskor", "Skolsköterskor", "Företagssköterskor", "Röntgensjuksköterskor", "Övriga specialistsjuksköterskor", "Psykologer", "Psykoterapeuter", "Veterinärer", "Tandläkare", "Kiropraktorer och naprapater m.fl.", "Sjukgymnaster", "Arbetsterapeuter", "Apotekare", "Dietister", "Audionomer och logopeder", "Optiker", "Övriga specialister inom hälso- och sjukvård", "Professorer", "Universitets- och högskolelektorer", "Forskarassistenter m.fl.", "Doktorander", "Övriga universitets- och högskollärare", "Lärare i yrkesämnen", "Gymnasielärare", "Grundskollärare", "Fritidspedagoger", "Förskollärare", "Speciallärare och specialpedagoger m.fl.", "Studie- och yrkesvägledare", "Övriga pedagoger med teoretisk specialistkompetens", "Revisorer m.fl.", "Controller", "Finansanalytiker och investeringsrådgivare m.fl.", "Traders och fondförvaltare", "Nationalekonomer och makroanalytiker m.fl.", "Övriga ekonomer", "Lednings- och organisationsutvecklare", "Planerare och utredare m.fl.", "Personal- och HR-specialister", "Marknadsanalytiker och marknadsförare m.fl.", "Informatörer, kommunikatörer och PR-specialister", "Systemanalytiker och IT-arkitekter m.fl.", "Mjukvaru- och systemutvecklare m.fl.", "Utvecklare inom spel och digitala media", "Systemtestare och testledare", "Systemförvaltare m.fl.", "IT-säkerhetsspecialister", "Övriga IT-specialister", "Advokater", "Domare", "Åklagare", "Affärs- och företagsjurister", "Förvaltnings- och organisationsjurister", "Övriga jurister", "Museiintendenter m.fl.", "Bibliotekarier och arkivarier", "Arkeologer och specialister inom humaniora m.m.", "Författare m.fl.", "Journalister m.fl.", "Översättare, tolkar och lingvister m.fl.", "Bildkonstnärer m.fl.", "Musiker, sångare och kompositörer", "Koreografer och dansare", "Regissörer och producenter av film, teater m.m.", "Skådespelare", "Socialsekreterare", "Kuratorer", "Biståndsbedömare m.fl.", "Övriga yrken inom socialt arbete", "Präster", "Diakoner", "Ingenjörer och tekniker inom industri, logistik och produktionsplanering", "Ingenjörer och tekniker inom bygg och anläggning", "Ingenjörer och tekniker inom elektroteknik", "Ingenjörer och tekniker inom maskinteknik", "Ingenjörer och tekniker inom kemi och kemiteknik", "Ingenjörer och tekniker inom gruvteknik och metallurgi", "GIS- och kartingenjörer", "Övriga ingenjörer och tekniker", "Arbetsledare inom bygg, anläggning och gruva", "Arbetsledare inom tillverkning", "Maskinbefäl", "Fartygsbefäl m.fl.", "Piloter m.fl.", "Flygledare", "Flygtekniker", "Tekniker, bilddiagnostik och medicinteknisk utrustning", "Biomedicinska analytiker m.fl.", "Receptarier", "Tandtekniker och ortopedingenjörer m.fl.", "Laboratorieingenjörer", "Terapeuter inom alternativmedicin", "Djursjukskötare m.fl.", "Tandhygienister", "Mäklare inom finans", "Banktjänstemän", "Redovisningsekonomer", "Skadereglerare och värderare", "Försäkringssäljare och försäkringsrådgivare", "Företagssäljare", "Inköpare och upphandlare", "Ordersamordnare m.fl.", "Speditörer och transportmäklare", "Evenemangs- och reseproducenter m.fl.", "Arbetsförmedlare", "Fastighetsmäklare", "Fastighetsförvaltare", "Övriga förmedlare", "Gruppledare för kontorspersonal", "Domstols- och juristsekreterare m.fl.", "Chefssekreterare och VD-assistenter m.fl.", "Tull- och kustbevakningstjänstemän", "Skattehandläggare", "Socialförsäkringshandläggare", "Säkerhetsinspektörer m.fl.", "Brandingenjörer och byggnadsinspektörer m.fl.", "Övriga handläggare", "Poliser", "Behandlingsassistenter och socialpedagoger m.fl.", "Pastorer m.fl.", "Professionella idrottsutövare", "Idrottstränare och instruktörer m.fl.", "Fritidsledare m.fl.", "Friskvårdskonsulenter och hälsopedagoger m.fl.", "Fotografer", "Inredare, dekoratörer och scenografer m.fl.", "Inspicienter och scriptor m.fl.", "Övriga yrken inom kultur och underhållning", "Trafiklärare", "Övriga utbildare och instruktörer", "Köksmästare och souschefer", "Storhushållsföreståndare", "Drifttekniker, IT", "Supporttekniker, IT", "Systemadministratörer", "Nätverks- och systemtekniker m.fl.", "Webbmaster och webbadministratörer", "Bild- och sändningstekniker", "Ljus-, ljud och scentekniker", "Ekonomiassistenter m.fl.", "Löne- och personaladministratörer", "Backofficepersonal m.fl.", "Marknads- och försäljningsassistenter", "Inköps- och orderassistenter", "Skolassistenter m.fl.", "Medicinska sekreterare, vårdadministratörer m.fl.", "Övriga kontorsassistenter och sekreterare", "Croupierer och oddssättare m.fl.", "Inkasserare och pantlånare m.fl.", "Resesäljare och trafikassistenter m.fl.", "Kundtjänstpersonal", "Telefonister", "Hotellreceptionister m.fl.", "Kontorsreceptionister", "Marknadsundersökare och intervjuare", "Arbetsledare inom lager och terminal", "Lager- och terminalpersonal", "Transportledare och transportsamordnare", "Biblioteks- och arkivassistenter m.fl.", "Brevbärare och postterminalarbetare", "Förtroendevalda", "Kabinpersonal m.fl.", "Tågvärdar och ombordansvariga m.fl.", "Guider och reseledare", "Kockar och kallskänkor", "Hovmästare och servitörer", "Bartendrar", "Frisörer", "Hudterapeuter", "Massörer och massageterapeuter", "Fotterapeuter", "Övriga skönhets- och kroppsterapeuter", "Städledare och husfruar", "Fastighetsskötare", "Begravnings- och krematoriepersonal", "Övrig servicepersonal", "Säljande butikschefer och avdelningschefer i butik", "Butikssäljare, dagligvaror", "Butikssäljare, fackhandel", "Optikerassistenter", "Bensinstationspersonal", "Uthyrare", "Apotekstekniker", "Kassapersonal m.fl.", "Eventsäljare och butiksdemonstratörer m.fl.", "Telefonförsäljare m.fl.", "Barnskötare", "Elevassistenter m.fl.", "Undersköterskor, hemtjänst, hemsjukvård och äldreboende", "Undersköterskor, habilitering", "Undersköterskor, vård- och specialavdelning", "Undersköterskor, mottagning", "Barnsköterskor", "Ambulanssjukvårdare", "Vårdbiträden", "Skötare", "Vårdare, boendestödjare", "Personliga assistenter", "Övrig vård- och omsorgspersonal", "Tandsköterskor", "Brandmän", "Kriminalvårdare", "Väktare och ordningsvakter", "SOS-operatörer m.fl.", "Övrig bevaknings- och säkerhetspersonal", "Odlare av jordbruksväxter, frukt- och bär", "Trädgårdsodlare", "Trädgårdsanläggare m.fl.", "Uppfödare och skötare av lantbrukets husdjur", "Uppfödare och skötare av sällskapsdjur", "Övriga djuruppfödare och djurskötare", "Växtodlare och djuruppfödare, blandad drift", "Skogsarbetare", "Fiskodlare", "Fiskare", "Träarbetare, snickare m.fl.", "Murare m.fl.", "Betongarbetare", "Anläggningsarbetare", "Anläggningsdykare", "Ställningsbyggare", "Övriga byggnads- och anläggningsarbetare", "Takmontörer", "Golvläggare", "Isoleringsmontörer", "Glastekniker", "VVS-montörer m.fl.", "Kyl- och värmepumpstekniker m.fl.", "Målare", "Lackerare och industrimålare", "Skorstensfejare", "Saneringsarbetare m.fl.", "Gjutare", "Svetsare och gasskärare", "Byggnads- och ventilationsplåtslagare", "Tunnplåtslagare", "Stålkonstruktionsmontörer och grovplåtsslagare", "Smeder", "Verktygsmakare", "Maskinställare och maskinoperatörer, metallarbete", "Slipare m.fl.", "Motorfordonsmekaniker och fordonsreparatörer", "Flygmekaniker m.fl.", "Underhållsmekaniker och maskinreparatörer", "Finmekaniker", "Guld- och silversmeder", "Musikinstrumentmakare och övriga konsthantverkare", "Prepresstekniker", "Tryckare", "Bokbindare m.fl.", "Installations- och serviceelektriker", "Industrielektriker", "Distributionselektriker", "Elektronikreparatörer och kommunikationselektriker m.fl.", "Manuella ytbehandlare, trä", "Fin-, inrednings- och möbelsnickare", "Maskinsnickare och maskinoperatörer, träindustri", "Skräddare och ateljésömmerskor m.fl.", "Sömmare", "Tapetserare", "Läderhantverkare och skomakare", "Slaktare och styckare m.fl.", "Bagare och konditorer", "Provsmakare och kvalitetsbedömare", "Övriga livsmedelsförädlare", "Gruv- och stenbrottsarbetare", "Processoperatörer, stenkross- och malmförädlingsanläggning", "Brunnsborrare m.fl.", "Maskinoperatörer, cement-, sten- och betongvaror", "Bergssprängare", "Stenhuggare m.fl.", "Maskinoperatörer, ytbehandling", "Valsverksoperatörer", "Övriga maskin- och processoperatörer vid stål- och metallverk", "Maskinoperatörer, farmaceutiska produkter", "Maskinoperatörer, kemisktekniska och fotografiska produkter", "Maskinoperatörer, gummiindustri", "Maskinoperatörer, plastindustri", "Maskinoperatörer, pappersvaruindustri", "Maskinoperatörer, blekning, färgning och tvättning", "Övriga maskinoperatörer, textil-, skinn- och läderindustri", "Maskinoperatörer, kött- och fiskberedningsindustri", "Maskinoperatörer, mejeri", "Maskinoperatörer, kvarn-, bageri- och konfektyrindustri", "Övriga maskinoperatörer inom livsmedelsindustri m.m.", "Processoperatörer, pappersmassa", "Processoperatörer, papper", "Operatörer inom sågverk, hyvleri och plywood m.m.", "Maskinoperatörer inom ytbehandling, trä", "Maskinoperatörer, påfyllning, packning och märkning", "Andra process- och maskinoperatörer", "Drifttekniker vid värme- och vattenverk", "Processövervakare, kemisk industri", "Processövervakare, metallproduktion", "Övriga drifttekniker och processövervakare", "Fordonsmontörer", "Montörer, elektrisk och elektronisk utrustning", "Montörer, metall-, gummi- och plastprodukter", "Montörer, träprodukter", "Övriga montörer", "Lokförare", "Bangårdspersonal", "Taxiförare m.fl.", "Övriga bil-, motorcykel- och cykelförare", "Buss- och spårvagnsförare", "Lastbilsförare m.fl.", "Förare av jordbruks- och skogsmaskiner", "Anläggningsmaskinförare m.fl.", "Kranförare m.fl.", "Truckförare", "Matroser och jungmän m.fl.", "Städare", "Övrig hemservicepersonal m.fl.", "Bilrekonditionerare, fönsterputsare och övriga rengöringsarbetare", "Bärplockare och plantörer m.fl.", "Grovarbetare inom bygg och anläggning", "Handpaketerare och andra fabriksarbetare", "Hamnarbetare", "Ramppersonal, flyttkarlar och varupåfyllare m.fl.", "Pizzabagare m.fl.", "Restaurang- och köksbiträden m.fl.", "Kafé- och konditoribiträden", "Torg- och marknadsförsäljare", "Renhållnings- och återvinningsarbetare", "Reklamutdelare och tidningsdistributörer", "Vaktmästare m.fl.", "Övriga servicearbetare", "yrke okänt"
			kon_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "män", "kvinnor"
			cont_klartext = "*",			 #  Finns: "Anställda (yrkesregistret) 16-64 år med arbetsplats i regionen (dagbef)"
			tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022"
			long_format = TRUE,			# TRUE = konvertera innehållsvariablerna i datasetet till long-format 
			wide_om_en_contvar = TRUE,			# TRUE = om man vill behålla wide-format om det bara finns en innehållsvariabel, FALSE om man vill konvertera till long-format även om det bara finns en innehållsvariabel
			output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
			excel_filnamn = "yrke_SSYK4.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
			returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
){

  # ====================================================================================================
  #
  # Funktion för att hämta data från SCB:s API med hjälp av pxweb-paketet
  # Automatgenererat av en funktion i R som skrivits av Peter Möller, Region Dalarna
  #
  # Skapad av: frkjon den 01 november 2024
  # Senast uppdaterad: 01 november 2024
  #
  # url till tabellens API: https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0208/AM0208M/YREG60BAS
  #												https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0208/AM0208M/YREG60N
  #												https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0208/AM0208M/YREG60
  #
  # Data tillbaka till 2014. Notera att det sker ett skifte mellan RAMS och BAS 2020. Det finns även 
  # data längre tillbaka i tiden men då är det en äldre version av SSYK som stökar till det
  # Källa: https://www.scb.se/hitta-statistik/statistik-efter-amne/arbetsmarknad/sysselsattning-forvarvsarbete-och-arbetstider/yrkesregistret-med-yrkesstatistik/#_TabelleriStatistikdatabasen
  # Se det 6:e plusset under tabeller i statistikdatabasen.
  #
  # Notera en märklighet där skriptet, om man väljer senaste år, även skickar med 2018. Oklart varför
  # ====================================================================================================

  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
    			tidyverse,
    			writexl)

  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

  # Url till SCB:s databas
  url_list <- c("https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0208/AM0208M/YREG60BAS",
						"https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0208/AM0208M/YREG60N",
						"https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0208/AM0208M/YREG60")

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
  tid_vekt <- if (all(tid_koder != "*")) tid_koder %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar] %>% unique() else giltiga_ar



	# special, ta bort överlappande värden mellan tabeller där de förekommer, värden från nyaste tabellerna behålls
	if (url_uttag == 'https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0208/AM0208M/YREG60N') {
		if (tid_koder == "*") tid_vekt <- giltiga_ar
		tid_vekt <- tid_vekt[!tid_vekt %in% c('2020', '2021')]
	}

	if (length(tid_vekt) > 0) {
	  # query-lista till pxweb-uttag
	  varlista <- 	list(
  	"Region" = region_vekt,
  	"Yrke2012" = yrke2012_vekt,
  	"Kon" = kon_vekt,
  	"ContentsCode" = cont_vekt,
  	"Tid" = tid_vekt)	

	  if (all(is.na(yrke2012_klartext))) varlista <- varlista[names(varlista) != "Yrke2012"]
  if (all(is.na(kon_klartext))) varlista <- varlista[names(varlista) != "Kon"]

	  # Hämta data med varlista
	  px_uttag <- pxweb_get(url = url_uttag, query = varlista)

	  var_vektor <- c(regionkod = "Region", yrke2012kod = "Yrke2012")
	  var_vektor_klartext <- c("region", "Yrke (SSYK 2012)")
	
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

	  return(px_df)
   } # test om det finns giltig(a) tid-kod(er) i aktuell tabell
  } # slut hämta data-funktion 

  px_alla <- map(url_list, ~ hamta_data(.x)) %>% list_rbind()

  # Om användaren vill spara data till en Excel-fil
  if (!is.na(output_mapp) & !is.na(excel_filnamn)){
    write.xlsx(px_alla, paste0(output_mapp, excel_filnamn))
  }

  # Returnera data som en dataframe om användern valt det
  if (returnera_df) return(px_alla)
} # slut hämta data-funktion
