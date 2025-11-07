hamta_pendling_kommun_bostad_arbete_ArRegPend2_scb <- function(
    arbetsstallekommun_klartext = "*",	# NA = tas inte med i uttaget,  formatet ska vara "kommunkod" eller "Kommunkod Kommunnamn (bostad)"
    kon_klartext = NA,			            # "*" = alla, NA = tas inte med i uttaget,  Finns: "män", "kvinnor", "totalt"
    bostadskommun_klartext = "dala",		# "*" = alla kommuner, "dala" = endast dalakommuner, NA = tas inte med i uttaget,  formatet ska vara "kommunkod" eller "Kommunkod Kommunnamn (arbetsställe)"
    tid_koder = "*",			              # "*" = alla år, "9999" = senaste, finns: "2020", "2021", "2022", "2023"
    cont_klartext = "*",
    long_format = TRUE,			            # TRUE = konvertera innehållsvariablerna i datasetet till long-format 
    wide_om_en_contvar = TRUE,			    # TRUE = om man vill behålla wide-format om det bara finns en innehållsvariabel, FALSE om man vill konvertera till long-format även om det bara finns en innehållsvariabel
    output_mapp = NA,			              # anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
    returnera_df = TRUE,			            # TRUE om man vill ha en dataframe i retur från funktionen
    excel_filnamn = "pendling_kommun_bostad_arbete.xlsx"
){
  
  # ====================================================================================================
  #
  # Funktion för att hämta data från SCB:s API med hjälp av pxweb-paketet
  # Årligt register efter bostadskommun, arbetsställekommun, kön och år
  #
  # Skapad av: Joacim Gärds den 6 november 2025
  # Senast uppdaterad: 6 november 2025
  #
  # url till tabellens API: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0210__AM0210F/ArRegPend2
  #
  # ====================================================================================================

  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
         tidyverse,
         writexl)
  
  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  # Url till SCB:s databas
  url_list <- c("https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0210/AM0210F/ArRegPend2")
  
  senaste_ar <- map(url_list, ~ hamta_giltiga_varden_fran_tabell(.x, "tid")) %>% unlist() %>% max()      # hämta senaste år som finns i alla medskickade tabeller
  hamta_tid <- if(any(tid_koder == "9999")) tid_koder %>% str_replace("9999", senaste_ar) else tid_koder                       # byt ut "9999" till senaste tillgängliga året i tabellerna
  hamta_tid <- hamta_tid %>% unique()              # ta bort eventuella dubletter
  
  hamta_data <- function(url_uttag) {
    
    px_meta <- pxweb_get(url_uttag)
    
    varlist_koder <- pxvarlist(px_meta)$koder
    varlist_bada <- pxvarlist(px_meta)
    
    # Gör om från klartext till kod som databasen förstår
    kon_vekt <- if (!all(is.na(kon_klartext))) hamta_kod_med_klartext(px_meta, kon_klartext, skickad_fran_variabel = "kon") else NA
    arbetsstallekommun_vekt <- if (!all(is.na(arbetsstallekommun_klartext))) hamta_kod_med_klartext(px_meta, arbetsstallekommun_klartext, skickad_fran_variabel = "arbetsstallekommun") else NA
    
    # Om "dala", sätt bostadskommun_klartext till en lista med kommunkoder
    if (!is.null(bostadskommun_klartext)) {
      if (bostadskommun_klartext == "dala") {
        bostadskommun_vekt <- c("2021","2023","2026","2029","2031",
                                "2034","2039","2061","2062","2080",
                                "2081","2082","2083","2084","2085")
      } else if (bostadskommun_klartext == "*") {
        bostadskommun_vekt <- "*"  # alla
      } else {
        # om användaren angav specifika kommuner med namn:
        bostadskommun_vekt <- hamta_kod_med_klartext(px_meta, bostadskommun_klartext, skickad_fran_variabel = "bostadskommun")
      }
    }  
    
    cont_vekt <-  hamta_kod_med_klartext(px_meta, cont_klartext, "contentscode")
    if (length(cont_vekt) > 1) wide_om_en_contvar <- FALSE
    
    # Hantera tid-koder
    giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "tid")
    tid_vekt <- if (all(hamta_tid != "*")) hamta_tid %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar] %>% unique() else giltiga_ar
    
    if (length(tid_vekt) > 0) {
      # query-lista till pxweb-uttag
      varlista <- list(
        "Kon" = kon_vekt,
        "Arbetsstallekommun" = arbetsstallekommun_vekt,
        "Bostadskommun" = bostadskommun_vekt,
        "Tid" = tid_vekt)
      
      if (all(is.na(kon_klartext))) varlista <- varlista[names(varlista) != "Kon"]
      if (all(is.na(arbetsstallekommun_vekt))) varlista <- varlista[names(varlista) != "Arbetsstallekommun"]
      if (all(is.na(bostadskommun_vekt))) varlista <- varlista[names(varlista) != "Bostadskommun"]
      
      # Hämta data med varlista
      px_uttag <- pxweb_get(url = url_uttag, query = varlista)
      
      var_vektor <- c(regionkod = "Region")
      var_vektor_klartext <- "region"
      
      # gör om pxweb-uttaget till en dataframe
      px_df <- as.data.frame(px_uttag)

      # man kan välja bort long-format, då låter vi kolumnerna vara wide om det finns fler innehållsvariabler, annars
      # pivoterar vi om till long-format, dock ej om det bara finns en innehållsvariabel
      if (long_format & !wide_om_en_contvar) px_df <- px_df %>% konvertera_till_long_for_contentscode_variabler(url_uttag)
      
      return(px_df)
    } # test om de valda tid-koderna finns i denna tabell
  } # slut hämta data-funktion 
  
  px_alla <- map(url_list, ~ hamta_data(.x)) %>% list_rbind()
  
  # Om användaren vill spara data till en Excel-fil
  if (!is.na(output_mapp) & !is.na(excel_filnamn)){
    write.xlsx(px_alla, paste0(output_mapp, excel_filnamn))
  }
  
  # Returnera data som en dataframe om användern valt det
  if (returnera_df) return(px_alla)
}
