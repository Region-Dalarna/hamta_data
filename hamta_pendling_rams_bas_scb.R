

hamta_pendling_rams_bas_scb <- function(region_vekt = "20",
                                    kon_klartext_vekt = "män och kvinnor",      # finns: "män", "kvinnor", "män och kvinnor"
                                    tid_vekt = "*"                              # "9999" = senaste år
                                    ) {

  # ===========================================================================================================
  #
  # Skript för att hämta pendlingsdata från RAMS och BAS, SCB. Årsvis. Skriptet hämtar data från fyra olika tabeller,
  # pendling år 1993-2003, 2004-2018 samt 2019-2021. Man får vara lite försiktig i sina analyser då det kan 
  # skilja sig något i definitioner, metod etc. Det kan vara en bra idé att i visualiseringen tydliggöra att data
  # kommer från olika tabeller, med ex. olika färger. 
  # 
  # 
  # Parametrar som skickas med (= variabler i SCB-tabellen) är:
  # - Region                                                      # tabellen innehåller bara kommuner och riket men länssiffror kan beräknas genom aggregering 
  # - Kön                                                         # det funkar dock inte för andel av befolkningen 20-64 år, då skickas bara NA-värden med
  # - tid (dvs. år)                                               # 
  #
  # Innehåll skickas inte med, då det bara är ett val. 
  #
  # Skapat av Peter Möller i november 2023.
  # Senast ändrad: 21 dec 2023
  #
  # ===========================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 pxweb)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  options(dplyr.summarise.inform = FALSE)
  
  # hantering av att klartext för kön hanteras olika i BAS och RAMS
  kon_klartext_alla <- c("totalt", "män och kvinnor")
  if (any(kon_klartext_alla %in% kon_klartext_vekt)) kon_klartext_vekt <- c(kon_klartext_vekt, kon_klartext_alla) %>% unique()
  
  regionnyckel <- hamtaregtab()
  
  kommun_vekt <- region_vekt[nchar(region_vekt) == 4]
  lan_vekt <- region_vekt[nchar(region_vekt) == 2]
  lan_kommuner_vekt <- hamtakommuner(lan_vekt, tamedlan = FALSE, tamedriket = FALSE)

  hamta_region_vekt <- c(kommun_vekt, lan_kommuner_vekt)

  url_rams_vekt <- c("https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0207/AM0207Z/AM0207PendlKomA04N",
                     "https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0207/AM0207L/AM0207PendlKomA04",
                     "https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0207/AM0207L/AM0207PendlKomA9303",
                     "https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0210/AM0210F/ArRegPend2")

#pxvarlist(url_rams)
#hamta_giltiga_varden_fran_tabell(url_rams_vekt[1], "tid", klartext = T)

  senaste_ar <- map(url_rams_vekt, ~ hamta_giltiga_varden_fran_tabell(.x, "tid")) %>% unlist() %>% max()      # hämta senaste år som finns i alla medskickade tabeller
  hamta_tid <- if(any(tid_vekt == "9999")) tid_vekt %>% str_replace("9999", senaste_ar) else hamta_tid <- tid_vekt                       # byt ut "9999" till senaste tillgängliga året i tabellerna
  hamta_tid <- hamta_tid %>% unique()              # ta bort eventuella dubletter
  
# funktion för att hämta data från pendlingstabeller
hamta_data <- function(url_rams) {
  
  px_meta <- pxweb_get(url_rams)
  
  # hantering för att klartext för antal pendlare heter olika i RAMS och BAS
  pendlare_klartext <- hamta_giltiga_varden_fran_tabell(px_meta, "contentscode", klartext = TRUE)
  
  alla_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "tid")
  akt_tid_vekt <- if (all(hamta_tid == "*")) alla_ar else hamta_tid[hamta_tid %in% alla_ar]
  
  
  # hantering av att RAMS och BAS båda innehåller åren 2020 och 2021, vi tar bort dessa år i RAMS
  if (url_rams == "https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0207/AM0207Z/AM0207PendlKomA04N") akt_tid_vekt <- akt_tid_vekt[!akt_tid_vekt %in% c("2020", "2021")]
  
  if (length(akt_tid_vekt) > 0 ) {
    px_kommun_in <- matrix(nrow = 0, ncol = 7) %>% as.data.frame()
    px_kommun_ut <- matrix(nrow = 0, ncol = 7) %>% as.data.frame()
    px_lan_in <- matrix(nrow = 0, ncol = 7) %>% as.data.frame()
    px_lan_ut <- matrix(nrow = 0, ncol = 7) %>% as.data.frame()
  
    # hantering av att klartext för kön hanteras olika i BAS och RAMS
    kon_alla_giltiga_varden <- hamta_giltiga_varden_fran_tabell(px_meta, variabel = "kon", klartext = TRUE)
    kon_klartext_tab <- kon_klartext_vekt[kon_klartext_vekt %in% kon_alla_giltiga_varden]
    kon_vekt <- hamta_kod_med_klartext(px_meta, kon_klartext_tab, "kon")
      
    varlista_in <- list(
      Bostadskommun = "*",
      Arbetsstallekommun = hamta_region_vekt,
      Kon = kon_vekt,
      ContentsCode = '*',
      Tid = akt_tid_vekt)
    
    px_uttag <- pxweb_get(url = url_rams, query = varlista_in)
    
    px_in <- px_uttag %>% as.data.frame() %>% 
      bind_cols(as.data.frame(px_uttag, variable.value.type = "code") %>%        # för att få med regionkod
                  select (regionkod_bo = bostadskommun, regionkod_arb = arbetsställekommun)) %>%
      rename(bostadsregion = bostadskommun, arbetsställeregion = arbetsställekommun) %>% 
      relocate(regionkod_bo, .before = bostadsregion) %>% 
      relocate(regionkod_arb, .before = arbetsställeregion)
    
    # ta med kommuner som är med i kommun_vekt
    if (length(kommun_vekt) > 0) {
      px_kommun_in <- px_in %>% 
        filter(regionkod_bo %in% kommun_vekt | regionkod_arb %in% kommun_vekt) %>% 
        rename(pendlare = {{pendlare_klartext}})
    }
    
    # aggregera på län och det finns län
    if (length(lan_vekt) > 0) { 
      px_lan_in <- px_in %>% 
        mutate(bolan_kod = str_sub(regionkod_bo,1,2),
               arblan_kod = str_sub(regionkod_arb,1,2)) %>% 
        filter(bolan_kod %in% lan_vekt | arblan_kod %in% lan_vekt) %>% 
        group_by(år, regionkod_bo = bolan_kod, regionkod_arb = arblan_kod, kön) %>% 
        summarise(!!pendlare_klartext := sum(!!sym(pendlare_klartext), na.rm = TRUE)) %>% 
        ungroup() %>% 
        left_join(regionnyckel %>% rename(regionkod_bo = regionkod, bostadsregion = region), by = "regionkod_bo") %>% 
        left_join(regionnyckel %>% rename(regionkod_arb = regionkod, arbetsställeregion = region), by = "regionkod_arb") %>% 
        rename(pendlare = {{pendlare_klartext}})
    }
    
  
    varlista_ut <- list(
      Bostadskommun = hamta_region_vekt,
      Arbetsstallekommun = "*",
      Kon = kon_vekt,
      ContentsCode = '*',
      Tid = akt_tid_vekt)
    
    px_uttag_ut <- pxweb_get(url = url_rams, query = varlista_ut)
    px_ut <- px_uttag_ut %>% as.data.frame() %>% 
      bind_cols(as.data.frame(px_uttag_ut, variable.value.type = "code") %>% 
                  select (regionkod_bo = bostadskommun, regionkod_arb = arbetsställekommun)) %>%
      rename(bostadsregion = bostadskommun, arbetsställeregion = arbetsställekommun) %>% 
      relocate(regionkod_bo, .before = bostadsregion) %>% 
      relocate(regionkod_arb, .before = arbetsställeregion)
    
    # ta med kommuner som är med i kommun_vekt
    if (length(kommun_vekt)>0) {
      px_kommun_ut <- px_ut %>% 
        filter(regionkod_bo %in% kommun_vekt | regionkod_arb %in% kommun_vekt) %>% 
        rename(pendlare = {{pendlare_klartext}})
    }
      
    if (length(lan_vekt)>0) { 
      px_lan_ut <- px_ut %>% 
        mutate(bolan_kod = str_sub(regionkod_bo,1,2),
               arblan_kod = str_sub(regionkod_arb,1,2)) %>% 
        filter(bolan_kod %in% lan_vekt | arblan_kod %in% lan_vekt) %>% 
        group_by(år, regionkod_bo = bolan_kod, regionkod_arb = arblan_kod, kön) %>% 
        summarise(!!pendlare_klartext := sum(!!sym(pendlare_klartext), na.rm = TRUE)) %>% 
        ungroup() %>% 
        left_join(regionnyckel %>% rename(regionkod_bo = regionkod, bostadsregion = region), by = "regionkod_bo") %>% 
        left_join(regionnyckel %>% rename(regionkod_arb = regionkod, arbetsställeregion = region), by = "regionkod_arb") %>% 
        rename(pendlare = {{pendlare_klartext}})
    }
    
    px_tot <- px_kommun_in %>% 
      bind_rows(px_kommun_ut) %>% 
      bind_rows(px_lan_in) %>% 
      bind_rows(px_lan_ut) %>% 
      select(-any_of(c("V1", "V2", "V3", "V4", "V5", "V6", "V7")))
    
  
    return(px_tot)
  } # slut if-sats om det finns några år som finns i just denna tabell
} # slut hamta_data-funktion

# hämta data från alla tabeller i url_rams_vekt
px_df <- map_dfr(url_rams_vekt, ~hamta_data(url_rams = .x)) %>% 
  filter(pendlare > 0) %>% 
  distinct(.keep_all = TRUE) %>% 
  mutate(bostadsregion = bostadsregion %>% str_remove(" \\(bostad\\)"),
         arbetsställeregion = arbetsställeregion %>% str_remove(" \\(arbetsställe\\)")) %>% 
  relocate(pendlare, .after = last_col())

if ("kön" %in% names(px_df)) px_df <- px_df %>% mutate(kön = ifelse(kön == "totalt", "män och kvinnor", kön))

return(px_df)

} # slut funktion
