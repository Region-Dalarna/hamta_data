hamta_data_yrken_bransch <- function(region_vekt = "20", # Val av region. Finns såväl regioner som kommuner 
                                        output_mapp = NA, # Här hamnar sparad data. Ändra till en sökväg för att data skall sparas
                                        kon_klartext = c("män","kvinnor"), # Män och kvinnor eller var och en uppdelad (inte totalt).
                                        yrke_klartext = "*", # Se nedan för alternativa val. "*" för alla
                                        bransch_klartext = "*", # Se nedan för alternativa val. "*" för alla
                                        returnera_data = TRUE, # Vill användaren returnera data som en dataframe
                                        tid = "9999", # Sätts till "9999" om man enbart vill ha senaste år,"*" . Se nedan för ytterligare förklaring
                                        filnamn = "yrke_bransch.xlsx"){ # Filnamn
  
  
  # ===========================================================================================================  #
  # Skript för att hämta data för förvärvsarbetande baserat på yrke och bransch. Funkar för regioner, kommuner och Sverige
  # Funkar bara för regioner (eller Sverige)
  # För att välja specifika yrken och/eller branscher, använd namn på yrken/branscher från funktionen nedan
  # pxvardelist("https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0208/AM0208D/YREG56N", "SNI2007")
  #
  # tid: För ett intervall som slutar med sista tillgängliga år, skriv startår:"9999". Exempelvis 2010:"9999".
  # Funkar även med enstaka år c(2010,2015,"9999")
  #
  # Skapad av Jon Frank
  # Uppdaterad senast 2024-01-12 (justering tid)
  # Justerad: Ändrade variabelnamn till näringslivet (SCB har bytt) 2024-03-07. Bytte sedan tillbaka när de ändrade tillbaka
  # Har ändrat `Anställda 16-64 år (dagbef)` till "Anställda 16-64 år med arbetsplats i regionen (dagbef)"
  # ===========================================================================================================
  # Paket som behövs
  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
         tidyverse,
         openxlsx)
  
  # Funktioner som behövs
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  # Url till SCB:s databas
  url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0208/AM0208D/YREG56N"
  px_meta <- pxweb_get(url)          # vi hämtar metadata till tabellen här och gör inga fler uttag nedan = färre API-anrop (och elegantare lösning)
  cont_kod <- "000003T3"
    
  # Gör om från klartext till kod som databasen förstår
  kon_vekt <- if (!all(is.na(kon_klartext))) hamta_kod_med_klartext(px_meta, kon_klartext, skickad_fran_variabel = "kon") else NA
  yrke_vekt <- if (!all(is.na(yrke_klartext))) hamta_kod_med_klartext(px_meta, yrke_klartext, skickad_fran_variabel = "Yrke2012") else NA
  bransch_vekt <- if (!all(is.na(bransch_klartext))) hamta_kod_med_klartext(px_meta, bransch_klartext, skickad_fran_variabel = "SNI2007") else NA

  # Om tid har satts till 9999, välj senaste år
  # if("9999" %in% tid) tid = max(hamta_giltiga_varden_fran_tabell(url, "tid"))
  
  giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "tid")
  if (all(tid != "*")) tid <- tid %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar] %>% unique()
  
  varlista <- list(
    Region = region_vekt,
    Yrke2012 = yrke_vekt,
    SNI2007 = bransch_vekt,
    Kon = kon_vekt,
    ContentsCode = cont_kod,
    Tid = tid)
  
  if (all(is.na(kon_klartext))) varlista <- varlista[names(varlista) != "Kon"]
  if (all(is.na(yrke_klartext))) varlista <- varlista[names(varlista) != "Yrke2012"]
  if (all(is.na(bransch_klartext))) varlista <- varlista[names(varlista) != "SNI2007"]
  
  px_uttag <- pxweb_get(url = url,
                        query = varlista) 
  
  cont_var <- hamta_klartext_med_kod(px_meta, cont_kod, skickad_fran_variabel = "ContentsCode")
  # Lägg API-uttaget i px_df, lägg på ytterligare ett uttag men med koder istället för klartext
  px_df <- as.data.frame(px_uttag) %>% 
    cbind(as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>% 
            select(any_of(c(regionkod = "Region", yrkeskod = "Yrke2012", branschkod = "SNI2007", `Anställda 16-64 år med arbetsplats i regionen (dagbef)` = cont_var)))) %>% 
    # rename(any_of(c(regionkod = "Region",
    #                 yrkeskod = "Yrke2012",
    #                 branschkod = "SNI2007",
    #                 `Anställda 16-64 år (dagbef)` = cont_var))) %>%
    select(any_of(c("år", "regionkod", "region", "yrkeskod", "Yrke (SSYK 2012)", "branschkod", "näringsgren SNI 2007", "kön", "Anställda 16-64 år med arbetsplats i regionen (dagbef)")))



  # Om användaren vill spara data
  if (!is.na(output_mapp) & !is.na(filnamn)){
    write.xlsx(px_df,paste0(output_mapp,filnamn))
    }
  
  # Om användaren vill returnera data som en DF.
    if(returnera_data == TRUE) return(px_df)
  
}
