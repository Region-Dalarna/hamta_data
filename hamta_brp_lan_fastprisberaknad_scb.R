# =================================================================================================
#
# Kod för att fastprisberäkna BRP för regioner i Sverige utifrån volymökning i procent. Koden är 
# en reviderad version av kod som Anton Larsson, Region Halland har skrev hösten 2023.
# Revideringen har gjorts av Peter Möller, Region Dalarna, våren 2024.
#
# Fastprisberäkningen görs genom att multiplicera BRP för basåret med volymutvecklingen för varje
# år utifrån basåret, enligt SCB:s metod som beskrivs här: 
# https://www.scb.se/hitta-statistik/statistik-efter-amne/nationalrakenskaper/nationalrakenskaper/nationalrakenskaper-kvartals-och-arsberakningar/produktrelaterat/Fordjupad-information/vanliga-fragor--nationalrakenskaper/
#
# =================================================================================================

if (!require("pacman")) install.packages("pacman")
p_load(tidyverse,
       pxweb)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8")
source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_brp_lan_fastprisberaknad_scb.R", encoding = "utf-8")


hamta_brp_lan_fastprisberaknad <- function(region_vekt = "*",
                             valda_ar = "*",
                             bas_ar = "2000"){
  

  
  # Hämtar BRP, löpande priser och volymutveckling
  volymutveckling <- suppressWarnings(
    hamta_brp_lan_region_tid_scb(
    region_vekt = region_vekt,			# Val av region. Finns: "00", "01", "03", "04", "05", "06", "07", "08", "09", "10", "12", "13", "14", "17", "18", "19", "20", "21", "22", "23", "24", "25", "RIKS1", "RIKS2", "RIKS3", "RIKS4", "RIKS5", "RIKS6", "RIKS7", "RIKS8", "90"
    cont_klartext = c("BRP, löpande priser, mnkr", "BRP, volymutveckling i procent"),			 #  Finns: "BRP, löpande priser, mnkr", "BRP, volymutveckling i procent", "BRP per invånare, löpande priser, tkr", "BRP per sysselsatt, löpande priser, tkr", "Medelantal sysselsatta, personer i 1000-tal", "Egentlig lön, löpande priser, mnkr"
    tid_koder = valda_ar,			 # "*" = alla år eller månader, "9999" = senaste, finns: "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022"
    long_format = FALSE,		 # TRUE = långt format, FALSE = brett format
    ))
  
  # Någon form av felhantering för basår
  if (!bas_ar %in% volymutveckling$år) bas_ar <- min(volymutveckling$år)
  if (bas_ar < min(volymutveckling$år)) bas_ar <- min(volymutveckling$år)
  if (bas_ar > max(volymutveckling$år)) bas_ar <- max(volymutveckling$år)
  
  volymutveckling <- volymutveckling %>%
    rename(volymutveckling = `BRP, volymutveckling i procent`) %>%
    mutate(volymutveckling = volymutveckling / 100 + 1) %>%
    rename(brp_lopande = `BRP, löpande priser, mnkr`)
  
  # Skapar funktionen fastpris
  
  fastpris <- function(skickad_regionkod, skickat_basar, skickad_df){
    
    brp_geografi <- skickad_df %>%
      filter(regionkod == skickad_regionkod)
    
    brpbasar <- brp_geografi %>%
      filter(år == skickat_basar)
    
    brp_efter_basar <- brp_geografi %>% 
      filter(år >= skickat_basar) %>%
      mutate(volymutveckling2 = if_else(år == skickat_basar, 1, volymutveckling)) %>%
      mutate(volymutveckling2 = cumprod(volymutveckling2))
    
    brp_innan_basar <- brp_geografi %>% 
      filter(år <= skickat_basar) %>%
      mutate(volymutveckling2 = lead(volymutveckling)) %>%
      filter(år != skickat_basar) %>%
      arrange(desc(år)) %>%
      mutate(volymutveckling2 = cumprod(volymutveckling2)) %>%
      arrange(år)
    
    brp_fastpris <- bind_rows(brp_innan_basar, brp_efter_basar) %>%
      mutate(brp_fastprisberaknad = if_else(år == skickat_basar, brpbasar$brp_lopande, 
                                            if_else(år > skickat_basar, brpbasar$brp_lopande * volymutveckling2, 
                                                    brpbasar$brp_lopande / volymutveckling2))) %>%
      select(-volymutveckling2)
    
    return(brp_fastpris)
  } # slut funktion fastpris()
  
  # Skapar en vektor med datasetets unika regionkoder
  lan_vekt <- unique(volymutveckling$regionkod)
  
  # Använder map för att köra funktionen fastpris för alla regioner, och binda samman i ett dataset
  retur_df <- map(lan_vekt, ~ fastpris(.x, bas_ar, skickad_df = volymutveckling)) %>% 
    list_rbind()
  
  # skapa ett kolumnnamn för BRP i fasta priser där basåret är med i namnet
  fastpris_kolumn <- paste0("BRP, fasta priser, mnkr (basår ", bas_ar, ")")
  
  # döp om kolumnerna till deras originalnamn + det nya kolumnnamnet för fasta priser
  # (ja, jag vet. Vi hade kunnat låta bli att döpa om kolumnerna för löpande priser och volymutveckling ovan, men jag orkade inte och döpte om dem till originalnamnen här istället)
  retur_df <- retur_df %>% 
    rename(`BRP, löpande priser, mnkr` = brp_lopande,
           `BRP, volymutveckling i procent` = volymutveckling, 
           !!fastpris_kolumn := brp_fastprisberaknad)
  
  return(retur_df)
  
} # slut funktion
