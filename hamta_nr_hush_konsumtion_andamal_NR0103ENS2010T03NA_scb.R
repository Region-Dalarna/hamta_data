hamta_nr_hush_kons_andamalcoicop_tid_scb <- function(
			andamalcoicop_klartext = "*",			 #  Finns: "livsmedel och alkoholfria drycker", "livsmedel", "bröd och spannmålsprodukter", "kött", "fisk", "mjölk, ost och ägg", "oljor och fetter", "frukt", "grönsaker", "sötsaker, glass, sylt, marmelad och konfekt", "salt, kryddor, såser o homogeniserad barnmat", "alkoholfria drycker", "kaffe, te och choklad", "läsk, juice, saft och mineralvatten", "alkoholhaltiga drycker och tobak", "alkoholhaltiga drycker", "sprit", "vin", "öl", "starköl", "öl klass I o II", "tobak", "narkotika", "kläder och skor", "kläder mm", "material till kläder", "kläder", "klädaccessoarer, sybehör och garn", "lagning, hyra och tvätt av kläder", "skor samt reparation och hyra av skor", "skor", "reparation och hyra av skor", "bostad, elektricitet, gas och uppvärmning", "hyra i flerfamiljshus, kallhyra", "faktisk hyra i hyresrätter, kallhyra", "bostadsrätt, nyttjandevärde kallhyra", "småhus och fritidshus, nyttjandevärde kallhyra", "småhus, nyttjandevärde kallhyra", "fritidshus, nyttjandevärde kallhyra", "varor och tjänster för underhåll av bostaden", "el, gas, olja och andra bränslen", "elström", "gas", "flytande bränslen; olja, fotogen och gasol", "fasta bränslen; ved, kol, pellets och flis", "fjärrvärme", "möbler, inredningsartiklar, hushållsutrustning o förbrukn.varor", "möbler,mattor o inredningsartiklar", "möbler, armatur, inredningsartiklar o tavlor", "mattor, inkl inläggning", "möbelreparationer", "hushållstextilier", "hushållsapparater", "större hushållsapparater som tilläggsutrustning", "mindre elektriska hushållsapparater", "rep av hushållsapparater", "husgeråd", "verktyg och utrustning för hus och trädgård", "större motordrivna apparater och verktyg", "mindre verktyg, trädgårdsutrustn, tillbehör, batterier o lampor", "hushållsvaror och -tjänster", "förbrukningsvaror och rengöringsartiklar", "hushållstjänster; städning, tvätt o hyra av hushållsutrustning", "hälso- och sjukvård", "medicinska och farmaceptiska produkter o sjukvårdsprodukter", "läkemedel och vitaminer", "andra sjukvårdsprodukter", "glasögon, linser, etc", "sjukvård, patientavgifter läkarvård, tandvård, sjukgymnastik mm", "öppen hälso- o sjukvård, patientavgifter", "tandvård, patientavgifter", "sjukgymnast, chiropraktor, terapeut, mm; patientavgifter", "sluten sjukvård, patientavgifter", "transporter och fordon", "fordon", "bilar", "motorcyklar, skotrar, mopeder o motorcross", "cyklar", "driftskostnader för personlig transportutrustning", "reservdelar och tillbehör", "driv- o smörjmedel; bensin, diesel, olja, glykol o k-sprit", "underhåll och reparation", "andra fordonstjänster; parkering, körkort o förmånsbil", "körkort;utbildning ,körprov, adm.avgift kort o register", "bilbesiktning", "broavgifter", "parkering", "bilförmån och bilhyra", "transporttjänster", "järnvägstransporter", "vägtransporter; taxi o långväga busstransporter", "lufttransporter", "sjötransport", "kollektivtrafik", "andra transporttjänster; flyttning", "post- och telekommunikationer", "post- och telekommunikationer", "posttjänster", "teleutrustning", "teletjänster; fast ,mobil och internet", "rekreation och kultur, varor och tjänster", "audio-visuell, foto- och datorutrustning", "utrust för att ta emot, spela in o återge ljud/bild; tv, radio mm", "kameror, övrig fotoutrustning och optiska instrument", "It-utrustning; pc, skrivare, tillbehör o kalkylatorer, skrivmask.", "film, cd, kasetter; inspelade och oinspelade", "rep av audiovisuell, foto- och, It-utrustning", "större varakt fritidsvaror; husvagnar, båtar, musikinstr o sportutrust", "större varaktiga fritidsvaror; husvagnar, båtar o sportutrustning", "musikinstrument och utrustning för inomhusaktiviteter", "rep och underhåll av större fritidsvaror", "andra fritids varor, växter, blommor, husdjur, djurmat o djurutrustning", "leksaker, spel, juldekorationer, fyrverkeriutrustning o  hobbyartiklar", "sport-, fiske-, och campingutrustning mm", "blommor, trädgårdsväxter, julgranar, jord, gödning o krukor", "husdjur, djurmat o djurutrustning", "veterinärs- och andra tjänster för djur; djurpensionat etc", "rekreation o kulturella tjänster", "sport- och rekreationstjänster; hyra av utrustning, deltagaravgifter", "kulturella tjänster; bio, museer, tv-avgifter, foto- och framkallning", "spel; nettot av satsade belopp minus utbetalda vinster", "tidningar, böcker och skrivmaterial", "böcker inkl läroböcker, exkl frimärksalbum", "tidningar och tidskrifter", "övriga trycksaker", "skrivmaterial", "paketresor", "utbildning", "utbildning", "restauranger, caféer, hotell och annan övernattningsservice", "restauranger, caféer, andra matserveringar, kiosker o automater", "hotell- och annan övernattningsservice", "övriga varor o tjänster", "personlig omvårdnad", "hår- och skönhetsvård", "elektriska apparater för personlig omvårdnad", "andra varor för kropps- och skönhetsvård", "prostitution", "personliga artiklar", "smycken, ur inkl reparationer", "and personl varor, tex väskor, barnvagnar, -stolar o div accesoarer", "omsorgstjänster för barn, äldre och funktionshindrade", "omsorgstjänster för barn, äldre och funktionshindrade", "barnomsorg", "Äldreomsorg", "personlig assistent", "Individomsorg", "försäkringstjänster", "finansiella tjänster", "diverse övriga tjänster; begravning, avgifter för intyg o service", "hushållens kons i utlandet, ofördelad", "utländska besökares konsumtion i Sverige, ofördelad", "hushållens totala konsumtionsutgifter", "hushållens icke-vinstdrivande organisationer", "total konsumtion"
			cont_klartext = "*",			 #  Finns: "Löpande priser, mnkr", "Fasta priser referensår 2015, mnkr", "Prisförändring, procent", "Volymförändring, procent"
			tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021"
			long_format = TRUE,			# TRUE = konvertera innehållsvariablerna i datasetet till long-format 
			wide_om_en_contvar = TRUE,			# TRUE = om man vill behålla wide-format om det bara finns en innehållsvariabel, FALSE om man vill konvertera till long-format även om det bara finns en innehållsvariabel
			output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
			excel_filnamn = "nr_hush_kons.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
			returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
){

  # ====================================================================================================
  #
  # Funktion för att hämta data från SCB:s API med hjälp av pxweb-paketet
  # Automatgenererat av en funktion i R som skrivits av Peter Möller, Region Dalarna
  #
  # Skapad av: moepet den 18 mars 2024
  # Senast uppdaterad: 18 mars 2024
  #
  # url till tabellens API: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__NR__NR0103__NR0103E/NR0103ENS2010T03NA/
  #
  # ====================================================================================================

if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
    			tidyverse,
    			writexl)

  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

  # Url till SCB:s databas
  url_uttag <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/NR/NR0103/NR0103E/NR0103ENS2010T03NA"
  px_meta <- pxweb_get(url_uttag)

  varlist_koder <- pxvarlist(px_meta)$koder
  varlist_bada <- pxvarlist(px_meta)

  # Gör om från klartext till kod som databasen förstår
  andamalcoicop_vekt <- hamta_kod_med_klartext(px_meta, andamalcoicop_klartext, skickad_fran_variabel = "andamalcoicop")


  cont_vekt <-  hamta_kod_med_klartext(px_meta, cont_klartext, "contentscode")
  if (length(cont_vekt) > 1) wide_om_en_contvar <- FALSE

  # Hantera tid-koder
  giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "tid")
  if (all(tid_koder != "*")) tid_koder <- tid_koder %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar] %>% unique()

# query-lista till pxweb-uttag
  varlista <- list(
  "AndamalCOICOP" = andamalcoicop_vekt,
  "ContentsCode" = cont_vekt,
  "Tid" = tid_koder)

  px_uttag <- pxweb_get(url = url_uttag, query = varlista)

  var_vektor <- c(andamalskod = "AndamalCOICOP")
  var_vektor_klartext <- "ändamål COICOP "

  px_df <- as.data.frame(px_uttag)
  if (!is.na(var_vektor)) {      px_df <- px_df %>%
            cbind(as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>%
            select(any_of(var_vektor)))

      px_df <- map2(names(var_vektor), var_vektor_klartext, ~ px_df %>% relocate(all_of(.x), .before = all_of(.y))) %>% list_rbind()
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
