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
    url_uttag <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101I/DodaFodelsearK"
    px_meta <- pxweb_get(url_uttag)
    cont_kod <- "BE0101D8"
    
    kon_koder <- if (all(!is.na(kon_klartext))) hamta_kod_med_klartext(px_meta, kon_klartext, skickad_fran_variabel = "kon") else NA
    alder_koder <- if (all(!is.na(alder_koder))) alder_koder %>% as.character() %>% ifelse(. == "100", "-100+", .) %>% ifelse(. == "tot", "totalt ålder", .) else NA
    
    giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "tid")
    if (all(tid_koder != "*")) tid_koder <- tid_koder %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar] %>% unique()
    
    
    varlista <- list(
      Region = region_vekt,
      Kon = kon_koder,
      Alder = alder_koder,
      ContentsCode = cont_kod,
      Tid = tid_koder
    )
    
    if (all(is.na(kon_klartext))) varlista <- varlista[names(varlista) != "Kon"]
    if (all(is.na(alder_koder))) varlista <- varlista[names(varlista) != "Alder"]
    
    px_uttag <- pxweb_get(url = url_uttag, query = varlista) 
    
    retur_df <- as.data.frame(px_uttag) %>% 
      cbind(as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>%
              select(Region)) %>% 
      rename(regionkod = Region) %>% 
      relocate(regionkod, .before = region)
    
    # man kan välja bort long-format, då låter vi kolumnerna vara wide om det finns fler innehållsvariabler, annars
    # pivoterar vi om till long-format, dock ej om det bara finns en innehållsvariabel
    if (long_format) {
      retur_df <- retur_df %>% 
        konvertera_till_long_for_contentscode_variabler(url_uttag)
      
    }
    
    if (returnera_df) return(retur_df)
    if (skriv_excelfil) write_xlsx(retur_df, paste0(mapp_excelfil, filnamn_excelfil))
  } else {
    print("Inget returneras. Välj 'returnera_df' och/eller ange mapp och filnamn för excelfil.")
  }
} 
