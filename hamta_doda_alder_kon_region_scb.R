hamta_doda_alder_kon_region_scb <- function(
    region_vekt = "20",                       # Dalarna defaultvärde
    kon_klartext = NA,                        # "kvinnor", "män", c("män", "kvinnor"), NA
    alder_koder = NA,                         # 1 årsgrupper, 1 - 100+ samt "tot"
    tid_koder = "*",                          # * = alla år, från 1997, "9999" = senaste år
    returnera_df = TRUE,                      # FALSE = ej returnera df
    mapp_excelfil = NA,                       # mapp för Excelfil
    filnamn_excelfil = NA,                    # filnamn för Excelfil 
    long_format = FALSE                       # TRUE = long-format, FALSE = wide-format
) {
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse, pxweb, writexl)
  
  # Använd hjälpfunktioner
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R", encoding = "utf-8", echo = FALSE)
  options(dplyr.summarise.inform = FALSE)
  
  skriv_excelfil <- if (!is.na(mapp_excelfil) & !is.na(filnamn_excelfil)) TRUE else FALSE
  
  if (returnera_df | skriv_excelfil) {
    
    
    url_list <- c("https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101I/DodaFodelsearK",
                  "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101I/DodaFodelsearKCKM")
    
    giltig_tid <- map(url_list, ~ hamta_giltiga_varden_fran_tabell(.x, "tid")) %>% unlist() %>% unique()      # hämta alla giltiga år som finns i alla medskickade tabeller
    senaste_ar <- giltig_tid %>% max()      # hämta senaste år som finns i alla medskickade tabeller
    hamta_tid <- if (all(tid_koder == "*")) hamta_tid <- giltig_tid else {
      tid_koder <- tid_koder %>% str_replace("9999", senaste_ar)
      hamta_tid <- tid_koder[tid_koder %in% giltig_tid]
    }
    
    hamta_data <- function(url_uttag) {
      
      px_meta <- pxweb_get(url_uttag)
      ar_ckm <- str_detect(tolower(url_uttag), "ckm")
  
      cont_kod <- hamta_giltiga_varden_fran_tabell(px_meta, "contentscode")
      
      # hantering av ålder
      alder_koder <- alder_koder %>% as.character()
      if (!all(is.na(alder_koder))) {
        alder_giltiga_varden <- hamta_giltiga_varden_fran_tabell(px_meta, "alder") %>% 
          .[!str_detect(., "-")] %>% 
          .[!(str_detect(tolower(.), "tot") & . != "TOT1") | tolower(.) == "tot"] %>% 
          .[!(str_detect(tolower(.), "100") & . != "100+1") | . == "100+"] %>% 
          as.character()      # hämta giltiga värden för ålder och gör om till character
        # om det finns fler total ålder-grupper, behåller vi bara en
        if (sum(str_count(tolower(alder_giltiga_varden), "tot")) > 1) {
          tot_traffar <- alder_giltiga_varden[str_detect(tolower(alder_giltiga_varden), "tot")]
          tot_tabort <- tot_traffar[2:length(tot_traffar)]
          alder_giltiga_varden <- alder_giltiga_varden %>%
            .[!. %in% tot_tabort]
        }
        alder_hamta <- if (all(alder_koder == "*")) alder_giltiga_varden else alder_koder
        if (ar_ckm) {
          alder_hamta <- alder_hamta %>%                             # byt ut till gitliga koder om man skickat med 100 eller totalt 
            as.character() %>%                                       # säkerställ att alder_koder är character och inte numeric
            .[. %in% alder_giltiga_varden]
        } else alder_hamta <- alder_hamta[alder_hamta %in% alder_giltiga_varden]
      } else {
        alder_hamta <- if (ar_ckm) "TotSA" else NA
      }
      
      kon_giltiga <- hamta_giltiga_varden_fran_tabell(px_meta, "kon") %>% .[. %in% c("1", "2")]
      kon_koder <- if (all(!is.na(kon_klartext))) {
        # om * ta alla giltiga 
        if (all(kon_klartext == "*")) kon_giltiga else hamta_kod_med_klartext(px_meta, kon_klartext, skickad_fran_variabel = "kon") %>% .[. %in% kon_giltiga]
      } else NA
      
      
      # hantering av tid (i detta fall år) och att kunna skicka med "9999" som senaste år
      giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "tid")
      tabell_tid <- hamta_tid[hamta_tid %in% giltiga_ar]
      
      if (length(tabell_tid) > 0) {
      varlista <- list(
        Region = region_vekt,
        Kon = kon_koder,
        Alder = alder_hamta,
        ContentsCode = cont_kod,
        Tid = tabell_tid
      )
      
      if (all(is.na(kon_klartext))) varlista <- varlista[names(varlista) != "Kon"]
      if (all(is.na(alder_koder)) & !ar_ckm) varlista <- varlista[names(varlista) != "Alder"]
      
      px_uttag <- pxweb_get(url = url_uttag, query = varlista) 
      
      retur_df <- as.data.frame(px_uttag) %>% 
        cbind(as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>%
                select(Region)) %>% 
        rename(regionkod = Region) %>% 
        relocate(regionkod, .before = region)
      
      if (all(is.na(alder_koder)) & ar_ckm) retur_df <- retur_df %>% select(-ålder)
      
      if ("ålder" %in% names(retur_df) & ar_ckm) {
        retur_df <- retur_df %>% 
          mutate(ålder = ifelse(ålder == "totalt, samtliga åldrar", "totalt ålder", ålder))
      }
      
      # man kan välja bort long-format, då låter vi kolumnerna vara wide om det finns fler innehållsvariabler, annars
      # pivoterar vi om till long-format, dock ej om det bara finns en innehållsvariabel
      if (long_format) {
        retur_df <- retur_df %>% 
          konvertera_till_long_for_contentscode_variabler(url_uttag)
        
      }
      
      return(retur_df)
      } # slut if-sats som kontrollerar om det finns giltiga år i tabellen
    } # slut hamta_data-funktionen
    
    px_df <- map(url_list, ~hamta_data(url_uttag = .x)) %>% 
      list_rbind()
    
    if (returnera_df) return(px_df)
    if (skriv_excelfil) write_xlsx(px_df, paste0(mapp_excelfil, filnamn_excelfil))
  } else {
    print("Inget returneras. Välj 'returnera_df' och/eller ange mapp och filnamn för excelfil.")
  }
} 
