
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)
options(dplyr.summarise.inform = FALSE)

regionnyckel <- hamtaregtab()

hamta_pendling_rams_scb <- function(region_vekt = "20",
                                    kon_klartext_vekt = "män och kvinnor",      # finns: "män", "kvinnor", "män och kvinnor"
                                    tid_vekt = "*"                              # "9999" = senaste år
                                    ) {

  
  kommun_vekt <- region_vekt[nchar(region_vekt) == 4]
  lan_vekt <- region_vekt[nchar(region_vekt) == 2]
  lan_kommuner_vekt <- hamtakommuner(lan_vekt, tamedlan = FALSE, tamedriket = FALSE)

  hamta_region_vekt <- c(kommun_vekt, lan_kommuner_vekt)

  url_rams_vekt <- c("https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0207/AM0207Z/AM0207PendlKomA04N",
                     "https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0207/AM0207L/AM0207PendlKomA04",
                     "https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0207/AM0207L/AM0207PendlKomA9303")

#pxvarlist(url_rams)
#hamta_giltiga_varden_fran_tabell(url_rams_vekt[1], "tid", klartext = T)

  hamta_tid <- if(tid_vekt == "9999") hamta_giltiga_varden_fran_tabell(url_rams_vekt[1], "tid") %>% max() else tid_vekt
  
  
# funktion för att hämta data från pendlingstabeller
hamta_data <- function(url_rams) {
  
  if (hamta_tid != "*") {
    alla_ar <- hamta_giltiga_varden_fran_tabell(url_rams, "tid")
    akt_tid_vekt <- hamta_tid[hamta_tid %in% alla_ar]
  } else akt_tid_vekt <- "*"
  
  if (length(akt_tid_vekt) > 0 ) {
    px_kommun_in <- matrix(nrow = 0, ncol = 7) %>% as.data.frame()
    px_kommun_ut <- matrix(nrow = 0, ncol = 7) %>% as.data.frame()
    px_lan_in <- matrix(nrow = 0, ncol = 7) %>% as.data.frame()
    px_lan_ut <- matrix(nrow = 0, ncol = 7) %>% as.data.frame()
  
    kon_vekt <- hamta_kod_med_klartext(url_rams, kon_klartext_vekt, "kon")
      
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
    if (length(kommun_vekt)>0) {
      px_kommun_in <- px_in %>% 
        filter(regionkod_bo %in% kommun_vekt | regionkod_arb %in% kommun_vekt)
    }
    
    # aggregera på län och det finns län
    if (length(lan_vekt)>0) { 
      px_lan_in <- px_in %>% 
        mutate(bolan_kod = str_sub(regionkod_bo,1,2),
               arblan_kod = str_sub(regionkod_arb,1,2)) %>% 
        filter(bolan_kod %in% lan_vekt | arblan_kod %in% lan_vekt) %>% 
        group_by(år, regionkod_bo = bolan_kod, regionkod_arb = arblan_kod, kön) %>% 
        summarise(`pendlare, antal` = sum(`pendlare, antal`, na.rm = TRUE)) %>% 
        ungroup() %>% 
        left_join(regionnyckel %>% rename(regionkod_bo = regionkod, bostadsregion = region), by = "regionkod_bo") %>% 
        left_join(regionnyckel %>% rename(regionkod_arb = regionkod, arbetsställeregion = region), by = "regionkod_arb")
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
        filter(regionkod_bo %in% kommun_vekt | regionkod_arb %in% kommun_vekt)
    }
      
    if (length(lan_vekt)>0) { 
      px_lan_ut <- px_ut %>% 
        mutate(bolan_kod = str_sub(regionkod_bo,1,2),
               arblan_kod = str_sub(regionkod_arb,1,2)) %>% 
        filter(bolan_kod %in% lan_vekt | arblan_kod %in% lan_vekt) %>% 
        group_by(år, regionkod_bo = bolan_kod, regionkod_arb = arblan_kod, kön) %>% 
        summarise(`pendlare, antal` = sum(`pendlare, antal`, na.rm = TRUE)) %>% 
        ungroup() %>% 
        left_join(regionnyckel %>% rename(regionkod_bo = regionkod, bostadsregion = region), by = "regionkod_bo") %>% 
        left_join(regionnyckel %>% rename(regionkod_arb = regionkod, arbetsställeregion = region), by = "regionkod_arb")
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
  filter(`pendlare, antal` > 0)

return(px_df)

} # slut funktion
