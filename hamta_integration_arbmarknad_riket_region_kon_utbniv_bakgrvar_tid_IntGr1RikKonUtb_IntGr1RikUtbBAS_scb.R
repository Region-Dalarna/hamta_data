hamta_integration_arbmarknad_riket_region_kon_utbniv_bakgrvar_tid_scb <- function(
			kon_klartext = "*",			 #  Finns: "män och kvinnor", "män", "kvinnor"
			utbniva_klartext = "*",			 #  Finns: "samtliga utbildningsnivåer", "utbildningsnivå: förgymnasial utbildning", "utbildningsnivå: gymnasial utbildning", "utbildningsnivå: eftergymnasial utbildning", "utbildningsnivå: uppgift saknas"
			bakgrund_klartext = "*",			 #  Finns: "samtliga 20-64 år", "20-24 år", "ålder: 25-34 år", "ålder: 35-44 år", "ålder: 45-54 år", "ålder: 55-64 år", "samtliga", "födelseregion: Sverige", "födelseregion: Norden exkl. Sverige", "födelseregion: EU/EFTA exkl. Norden", "födelseregion: övriga världen", "samtliga utrikes födda invandrare", "skäl till invandring: skyddsbehövande och deras anhöriga", "skäl till invandring: övriga utrikes födda invandrare", "samtliga utrikes födda", "vistelsetid 0-1 år", "vistelsetid 2-3 år", "vistelsetid 4-9 år", "vistelsetid 10- år", "ålder: 55-65 år"
			cont_klartext = "*",			 #  Finns: "Andel förvärvsarbetande (ny definition från och med 2019)", "Andel företagare av de förvärvsarbetande (ny definition från och med 2019)", "Andel inskrivna arbetslösa, procent", "Andel öppet arbetslösa, procent", "Andel sökande i program med aktivitetsstöd, procent", "Andel långtidsarbetslösa, procent", "Andel fortfarande förvärvsarbetande efter ett år (ny def. från och med 2019)", "Andel fortfarande företagare efter ett år (ny definition från och med 2019)", "Andel i chefsposition, procent", "Andel med eftergymn. utb. med arbete inom yrkesomr. 2-3 enligt SSYK, procent", "Andel med eftergymn. utb. med arbete på kval.nivå 3-4 enligt SSYK, procent", "Andel sysselsatta", "Andel företagare av de sysselsatta", "Andel inskrivna arbetslösa", "Andel öppet arbetslösa", "Andel sökande i program med aktivitetsstöd", "Andel långtidsarbetslösa,", "Andel fortfarande sysselsatta efter ett år", "Andel fortfarande företagare efter ett år", "Andel i chefsposition", "Andel med eftergymn. utb. med arbete inom yrkesomr. 2-3 enligt SSYK", "Andel med eftergymn. utb. med arbete på kval.nivå 3-4 enligt SSYK"
			tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024"
			long_format = TRUE,			# TRUE = konvertera innehållsvariablerna i datasetet till long-format 
			wide_om_en_contvar = TRUE,			# TRUE = om man vill behålla wide-format om det bara finns en innehållsvariabel, FALSE om man vill konvertera till long-format även om det bara finns en innehållsvariabel
			output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
			excel_filnamn = "integration_arbmarknad_riket.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
			returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
){

  # ====================================================================================================
  #
  # Funktion för att hämta data från SCB:s API med hjälp av pxweb-paketet
  # Automatgenererat av en funktion i R som skrivits av Peter Möller, Region Dalarna
  #
  # Skapad av: moepet den 07 mars 2026
  # Senast uppdaterad: 07 mars 2026
  #
  # url till tabellens API: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AA__AA0003__AA0003X/IntGr1RikKonUtb/
  #												https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AA__AA0003__AA0003B/IntGr1RikUtbBAS/
  #
  # ====================================================================================================

  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
    			tidyverse,
    			writexl)

  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

  # definiera bakgrund-nyckel för att hantera två olika versioner
  cont_nyckel <- list(
    "Andel sysselsatta" = c(
      "Andel sysselsatta",
      "Andel förvärvsarbetande (ny definition från och med 2019)"
    ),
    "Andel företagare av de sysselsatta" = c(
      "Andel företagare av de sysselsatta",
      "Andel företagare av de förvärvsarbetande (ny definition från och med 2019)"
    ),
    "Andel inskrivna arbetslösa" = c(
      "Andel inskrivna arbetslösa",
      "Andel inskrivna arbetslösa, procent"
    ),
    "Andel öppet arbetslösa" = c(
      "Andel öppet arbetslösa",
      "Andel öppet arbetslösa, procent"
    ),
    "Andel sökande i program med aktivitetsstöd" = c(
      "Andel sökande i program med aktivitetsstöd",
      "Andel sökande i program med aktivitetsstöd, procent"
    ),
    "Andel långtidsarbetslösa" = c(
      "Andel långtidsarbetslösa,",
      "Andel långtidsarbetslösa, procent"
    ),
    "Andel fortfarande sysselsatta efter ett år" = c(
      "Andel fortfarande sysselsatta efter ett år",
      "Andel fortfarande förvärvsarbetande efter ett år (ny def. från och med 2019)"
    ),
    "Andel fortfarande företagare efter ett år" = c(
      "Andel fortfarande företagare efter ett år",
      "Andel fortfarande företagare efter ett år (ny definition från och med 2019)"
    ),
    "Andel i chefsposition" = c(
      "Andel i chefsposition",
      "Andel i chefsposition, procent"
    ),
    "Andel med eftergymn. utb. med arbete inom yrkesomr. 2-3 enligt SSYK" = c(
      "Andel med eftergymn. utb. med arbete inom yrkesomr. 2-3 enligt SSYK",
      "Andel med eftergymn. utb. med arbete inom yrkesomr. 2-3 enligt SSYK, procent"
    ),
    "Andel med eftergymn. utb. med arbete på kval.nivå 3-4 enligt SSYK" = c(
      "Andel med eftergymn. utb. med arbete på kval.nivå 3-4 enligt SSYK",
      "Andel med eftergymn. utb. med arbete på kval.nivå 3-4 enligt SSYK, procent"
    )
  )

  cont_kolnamn_nyckel <- cont_nyckel %>%  
    imap(~ set_names(rep(.y, length(.x)), .x) %>% keep(names(.) != .y)) %>% 
    { setNames(unlist(., use.names = FALSE), unlist(map(., names))) } 
  
  # Url till databas
  url_list <- c("https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AA/AA0003/AA0003X/IntGr1RikKonUtb",
                "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AA/AA0003/AA0003B/IntGr1RikUtbBAS")

  giltig_tid <- map(url_list, ~ hamta_giltiga_varden_fran_tabell(.x, "tid")) %>% unlist() %>% unique()      # hämta alla giltiga år som finns i alla medskickade tabeller
  senaste_ar <- giltig_tid %>% max()      # hämta senaste år som finns i alla medskickade tabeller
  hamta_tid <- if (all(tid_koder == "*")) hamta_tid <- giltig_tid else {
    tid_koder <- tid_koder %>% str_replace("9999", senaste_ar)
    hamta_tid <- tid_koder[tid_koder %in% giltig_tid]
  }
  
 hamta_data <- function(url_uttag) {

  px_meta <- pxweb_get(url_uttag)

  varlist_koder <- pxvarlist(px_meta)$koder
  varlist_bada <- pxvarlist(px_meta)

  # Gör om från klartext till kod som databasen förstår
  kon_vekt <- hamta_kod_med_klartext(px_meta, kon_klartext, skickad_fran_variabel = "kon")
  utbniva_vekt <- hamta_kod_med_klartext(px_meta, utbniva_klartext, skickad_fran_variabel = "utbniv")
  bakgrund_vekt <- hamta_kod_med_klartext(px_meta, bakgrund_klartext, skickad_fran_variabel = "bakgrvar")
  
  
  # vi hittar rätt position i cont-nyckel 
  cont_position <- which(sapply(cont_nyckel, function(x) cont_klartext %in% x))
  sok_element <- cont_nyckel[[cont_position]]
  
  cont_vekt <- map(sok_element, ~ hamta_kod_med_klartext(px_meta, .x, "contentscode")) %>% keep(~ length(.x) > 0) %>% unlist()
  
  # cont_giltiga <- hamta_giltiga_varden_fran_tabell(px_meta, "contentscode", klartext = TRUE)
  # cont_hamta <- if(any(cont_klartext == "*")) cont_giltiga else cont_klartext
  # cont_vekt <-  hamta_kod_med_klartext(px_meta, cont_hamta, "contentscode")
  if (length(cont_vekt) > 1) wide_om_en_contvar <- FALSE

  # Hantera tid-koder
  giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "tid")
  tabell_tid <- hamta_tid[hamta_tid %in% giltiga_ar]
  
  if (length(tabell_tid) > 0) {
    # query-lista till pxweb-uttag
    varlista <- list(
    	"Region" = "00",
    	"Kon" = kon_vekt,
    	"UtbNiv" = utbniva_vekt,
    	"BakgrVar" = bakgrund_vekt,
    	"ContentsCode" = cont_vekt,
    	"Tid" = tabell_tid)
  
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
  
    # px_df <- px_df %>% 
    #   rename(any_of(cont_kolnamn_nyckel))
    # 
    px_df <- px_df %>% 
      rename(any_of(setNames(names(cont_kolnamn_nyckel), unname(cont_kolnamn_nyckel))))
      
    # man kan välja bort long-format, då låter vi kolumnerna vara wide om det finns fler innehållsvariabler, annars
    # pivoterar vi om till long-format, dock ej om det bara finns en innehållsvariabel
    if (long_format & !wide_om_en_contvar) px_df <- px_df %>% konvertera_till_long_for_contentscode_variabler(url_uttag)
  
    return(px_df)
    } # slut test om det finns något giltigt år i denna tabell av de användaren valt

  } # slut hämta data-funktion 

  px_alla <- map(url_list, ~ hamta_data(.x)) %>% list_rbind()

  # Om användaren vill spara data till en Excel-fil
  if (!is.na(output_mapp) & !is.na(excel_filnamn)){
    write.xlsx(px_alla, paste0(output_mapp, excel_filnamn))
  }

  # Returnera data som en dataframe om användern valt det
  if (returnera_df) return(px_alla)
} # slut hämta data-funktion
