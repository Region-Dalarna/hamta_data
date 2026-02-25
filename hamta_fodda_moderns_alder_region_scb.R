hamta_fodda_moderns_alder_region_scb <- function(
    region_vekt = "20",                       # Dalarna defaultvärde
    kon_klartext = NA,                        # "kvinnor", "män", c("män", "kvinnor"), NA
    alder_moder = NA,                         # 1 årsgrupper, 1 - 100 samt "totalt"
    tid_koder = "*",                          # * = alla år, från 1997, "9999" = senaste år
    returnera_df = TRUE,                      # FALSE = ej returnera df
    mapp_excelfil = NA,                       # mapp för Excelfil
    filnamn_excelfil = NA                    # filnamn för Excelfil 
) {
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse, pxweb, writexl)
  
  # I sektionen retur_df har jag ändrat från `Levande födda` till Antal då SCB verkar ha ändrat namnet på variabeln. /Jon 20250109
  
  # Använd hjälpfunktioner
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R", encoding = "utf-8", echo = FALSE)
  options(dplyr.summarise.inform = FALSE)
  
  skriv_excelfil <- if (!is.na(mapp_excelfil) & !is.na(filnamn_excelfil)) TRUE else FALSE
  
  if (returnera_df | skriv_excelfil) {
    
    url_list <- c("https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101H/FoddaK",
                  "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101H/FoddaKCKM")
    
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
      if (!all(is.na(alder_moder))) {
        alder_moder <- alder_moder %>% 
          str_replace("14", "-14") %>% 
          str_replace("49", "49+") %>% 
          as.character()
        
        # ta bort åldersgrupper som bara finns i CKM
        alder_giltiga_varden <- hamta_giltiga_varden_fran_tabell(px_meta, "aldermoder") %>% 
          .[!str_detect(., "\\d+-\\d+")] %>%
          .[!str_detect(., "\\d+\\+\\d+")] %>%
          .[!str_detect(., "-\\d{3,}")] %>% 
          as.character() 
        
        # om det finns fler total ålder-grupper, behåller vi bara en
        if (sum(str_count(tolower(alder_giltiga_varden), "tot")) > 1) {
          tot_traffar <- alder_giltiga_varden[str_detect(tolower(alder_giltiga_varden), "tot")]
          tot_tabort <- tot_traffar[2:length(tot_traffar)]
          alder_giltiga_varden <- alder_giltiga_varden %>%
            .[!. %in% tot_tabort]
        }
          
        # om det finns fler uppgift saknas-grupper, behåller vi bara en
        if (sum(str_count(tolower(alder_giltiga_varden), "us")) > 1) {
          tot_traffar <- alder_giltiga_varden[str_detect(tolower(alder_giltiga_varden), "us")]
          tot_tabort <- tot_traffar[2:length(tot_traffar)]
          alder_giltiga_varden <- alder_giltiga_varden %>%
            .[!. %in% tot_tabort]
        }
          
        alder_hamta <- if (all(alder_moder == "*")) alder_giltiga_varden else alder_moder
        alder_hamta <- alder_hamta %>%                             # byt ut till gitliga koder om man skickat med 100 eller totalt 
          str_replace(regex("^totalt$", ignore_case = TRUE), ifelse(ar_ckm, "TotSA", "tot")) %>% 
          as.character() %>%                                       # säkerställ att alder_moder är character och inte numeric
          .[. %in% alder_giltiga_varden]
      } else {
        alder_hamta <- if (ar_ckm) "TotSA" else NA
      }
      
      # om inget av medskickad alder_koder är giltigt så hämtas totalen
      if (length(alder_hamta) == 0) alder_hamta <- if (ar_ckm) "TotSA" else NA
      
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
          AlderModer = alder_hamta,
          ContentsCode = cont_kod,
          Tid = tabell_tid
        )
        
        if (all(is.na(kon_klartext))) varlista <- varlista[names(varlista) != "Kon"]
        if (all(is.na(alder_moder)) & !ar_ckm) varlista <- varlista[names(varlista) != "AlderModer"]
        
        px_uttag <- pxweb_get(url = url_uttag, query = varlista) 
        
        retur_df <- as.data.frame(px_uttag) %>% 
          cbind(as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>%
                  select(Region)) %>% 
          rename(regionkod = Region,
                 födda = Antal) %>% 
          relocate(regionkod, .before = region)
        
        if (all(is.na(alder_moder)) & ar_ckm) retur_df <- retur_df %>% select(-`moderns ålder`)
        
        if ("moderns ålder" %in% names(retur_df) & ar_ckm) {
          retur_df <- retur_df %>% 
            mutate(`moderns ålder` = ifelse(`moderns ålder` == "totalt, samtliga åldrar", "totalt ålder", `moderns ålder`))
        }
        
        return(retur_df)
      } # slut if-sats om det finns giltiga år för just denna tabell
    } # slut hämta data funktion 
    
    px_df <- map(url_list, ~hamta_data(url_uttag = .x)) %>% 
      list_rbind()
    
    if (returnera_df) return(px_df)
    if (skriv_excelfil) write_xlsx(px_df, paste0(mapp_excelfil, filnamn_excelfil))
  } else {
    print("Inget returneras. Välj 'returnera_df' och/eller ange mapp och filnamn för excelfil.")
  }
} 
