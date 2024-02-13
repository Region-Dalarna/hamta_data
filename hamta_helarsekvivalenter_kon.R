
hamta_helarsekvivalenter_kon <- function(
    region_vekt = "20",                       # Dalarna defaultvärde
    kon_klartext_vekt = c("män", "kvinnor"),                   # både kvinnor (2) och män (1), finns också "män och kvinnor totalt"
    cont_klartext_vekt = c("Summa helårsekvivalenter",
                           "Folkmängd"),          # för att kunna beräkna andel av bef 20-64 år, finns också: "sjukpenning", "Sjuk- och aktivitetsersersättning"
                                                           #        "Arbetslöshet", "Arbetsmarknadsåtgärder", "Ekonomiskt bistånd", "Etableringsersättning" samt
                                                           #        "Andel av befolkningen 20-64 år", den sista ska inte användas när vi aggregerar till län
    aldersgrupp_klartext = "20-64 år",       # finns: "20-64 år", "20-65 år"
    tid_vekt = "*",                           # alla år-månader
    long_format = TRUE                        # TRUE om vi vill ha df i long-format, annars kommer alla innehållsvariabler i wide-format
    ) {
  
  # ===========================================================================================================
  #
  # Skript för att hämta data från SCB om helårsekvivalenter, dvs hur många som är försörjda av offentliga
  # trygghetssystem, som ekonomiskt bistånd, etableringsersättning, sjupenning, sjuk- och aktivitetsersättning,
  # arbetslöshet samt arbetsmarknadsåtgärder. Månadsvis. 
  # 
  # Parametrar som skickas med (= variabler i SCB-tabellen) är:
  # - Innehåll                                                    # ersättningar enligt ovan, samt summa helårsekvivalenter och andel av befolkningen
  # - Region                                                      # tabellen innehåller bara kommuner och riket men länssiffror kan beräknas genom aggregering 
  # - Kön                                                         # det funkar dock inte för andel av befolkningen 20-64 år, då skickas bara NA-värden med
  # - tid (dvs. år och månad)
  #
  # Skapat av Peter Möller
  # Senast ändrad: nov 2023
  #
  # ===========================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 pxweb)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R")

  options(dplyr.summarise.inform = FALSE)
  
  # url till tabellen i SCB:s statistikdatabas
  url_uttag <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/HE/HE0000/HE0000T02N2"
  
  px_meta <- pxweb_get(url = url_uttag)
  
  
  #cont_koder <- hamta_kod_med_klartext(url_uttag, cont_klartext_vekt, skickad_fran_variabel = "contentscode")        #        hamta_kod_med_klartext(url_uttag, cont_klartext_vekt)                            # vi använder klartext i parametrar för innehållsvariabel, koder i övriga
  cont_koder <- hamta_kod_eller_klartext_fran_lista(px_meta, cont_klartext_vekt, skickad_fran_variabel = "contentscode", hamta_kod = TRUE)
  #kon_koder <- hamta_kod_med_klartext(url_uttag, kon_klartext_vekt, skickad_fran_variabel = "kon")
  kon_koder <- hamta_kod_eller_klartext_fran_lista(px_meta, kon_klartext_vekt, skickad_fran_variabel = "kon", hamta_kod = TRUE)
  aldersgrupp_koder <- hamta_kod_eller_klartext_fran_lista(px_meta, aldersgrupp_klartext, skickad_fran_variabel = "aldersgrupp", hamta_kod = TRUE)
  
  # hantering av tid (i detta fall år) och att kunna skicka med "9999" som senaste år
  giltiga_ar <- hamta_kod_eller_klartext_fran_lista(px_meta, "*", "tid")
  tid_koder <- tid_vekt %>% 
    as.character() %>% 
    str_replace("9999", max(giltiga_ar)) %>%
    str_replace("\\*", giltiga_ar) %>% 
    .[. %in% giltiga_ar] %>% 
    unique()
  
  # speciallösning för att aggregera till länssiffror - vi behöver lägga in en spärr mot att aggregera och ta ut andel av befolkningen samtidigt
  lanskoder <- region_vekt[str_length(region_vekt) == 2 & region_vekt != "00"]                      # plocka ut alla länskoder ur skickade regionkoder
  kommuner_riket <- region_vekt[!region_vekt %in% lanskoder]                                        # plocka ut riket + alla kommunkoder ur skickade regionkoder
  
  lanskommuner <- if (length(lanskoder) > 0)  hamtakommuner(lanskoder, tamedlan = FALSE, tamedriket = FALSE) else NA     # hämta alla kommuner för de länskoder vi har, om inga länskoder finns blir det NA
  
  hamta_region <- c(kommuner_riket, lanskommuner) %>% unique() %>% .[!is.na(.)]
  
  # variabler som vi vill ha med i uttaget
  varlista <- list(
    Region = hamta_region,
    Kon = kon_koder,
    Aldersgrupp = aldersgrupp_koder,
    ContentsCode = cont_koder,
    Tid = tid_vekt
    )
  
  # =============================================== API-uttag ===============================================
  
  px_uttag <- pxweb_get(url = url_uttag, query = varlista) 
  
  # Lägg API-uttaget i px_df, lägg på ytterligare ett uttag men med koder istället för klartext,
  # välj ut bara regionkolumnen i det andra uttaget, döp om den till regionkod och lägg den först av kolumnerna
  px_df <- as.data.frame(px_uttag) %>% 
    cbind(as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>%
            select(Region)) %>% 
    rename(regionkod = Region) %>% relocate(regionkod, .before = region)
  
  # om det finns länskoder medskickade aggregerar vi de här
  
  if (length(lanskoder) > 0) {                                        # kör koden nedan om det finns länskoder medskickade
    
    if (any(str_detect(cont_klartext_vekt, "Andel av befolkningen"))) {
      warning(paste0("Andel av befolkningen kan inte aggregeras och tas därför bort ur aggregeringen"))
      cont_klartext_vekt <- cont_klartext_vekt[!str_detect(cont_klartext_vekt, "Andel av befolkningen")]
      cont_andel <- TRUE
    } else cont_andel <- FALSE
    
    lansnamn <- hamtaregion_kod_namn(lanskoder) %>%                   # skapa en df med alla länskoder och länsnamn som skickats med
      rename(lanskod = regionkod,
             lan = region)
    
    px_df_lan <- px_df %>%                                            
      mutate(lanskod = str_sub(regionkod, 1, 2)) %>%                  # vi skapar länskoder av kommunkoderna och kopplar på länsnamn för de som är medskickade
      left_join(lansnamn, by = c("lanskod")) %>%                                         # vi kopplar på länsnamnet via länskoden som vi skapade ovan
      filter(!is.na(lan)) %>%                                         # de som inte får någon träff (dvs. inte är medskickade) tar vi bort ur denna df (de är medskickade kommuner eller riket som inte ingår i län som är medskickat)
      mutate(regionkod = lanskod,                                     # vi ersätter regionkod och region med länskod och län
             region = lan) %>%  
      select(-c(lanskod, lan)) %>%                                    # därefter tar vi bort länskod och län som inte längre behövs (då vi har dem i regionkod och region) 
      group_by(regionkod, region, åldersgrupp, kön, månad) %>%                     # vi aggregerar ihop länen genom att alla kommuner i ett län nu har fått län och länskod
      summarise(across(all_of(cont_klartext_vekt), \(x) sum(x, na.rm = TRUE))) %>%        # vi summerar alla innehållsvariabler, funkar för alla utom för 
      ungroup()   
      
    # om vi har andel med i uttaget så lägger vi med den i länsdatasetet men som NA (så att det funkar att binda ihop den med df för kommuner och riket)
    if (cont_andel) px_df_lan <- px_df_lan %>% 
      mutate(`Andel av befolkningen` = NA)
    
    px_df_riket_kommun <- px_df %>%                                   # skapa en df med bara kommuner och riket
      filter(regionkod %in% kommuner_riket)
    
    px_df <- px_df_lan %>%                                            # sätt ihop df med aggregerade län och de med bara kommuner och riket
      bind_rows(px_df_riket_kommun)
    
  } # slut if-sats för att kolla om det finns länssiffror med i datasetet
  
  retur_df <- px_df %>% 
    rename(tid = månad) %>% 
    mutate(år = str_sub(tid, 1, 4),
           månad = format(as.Date(paste(år, str_sub(tid, 6,7), "1", sep = "-")), "%B"),
           år_månad = paste0(år, " - ", månad),
           månad_år = paste0(månad, " ", år)) %>%                                          # skapa kolumner för år, månad, år_månad och månad_år
    arrange(kön) %>%                                                             # sortera raderna och...
    group_by(kön) %>% 
    mutate(sort = row_number()) %>%                                                        # skapa en kolumn som heter sort
    ungroup() %>% 
    mutate(månad_år = factor(månad_år),
           år_månad = factor(år_månad),
           år = factor(år),
           månad = factor(månad, levels = c("januari", "februari", "mars", "april", "maj", "juni", "juli", "augusti", "september", "oktober", "november", "december")), 
           månad_år = månad_år %>% reorder(sort),
           år_månad = år_månad %>% reorder(sort),
           år = år %>% reorder(sort)) %>%                                       # som vi använder för att sortera factor  
    select(-sort) %>%                                                           # ta bort sort-kolumnen när vi använt den för att sortera tids-kolumnerna
    relocate(år, .after = tid) %>%                                              # vi sorterar om kolumnerna så att innehållsvariabeler alltid ligger sist
    relocate(månad, .after = år) %>% 
    relocate(år_månad, .after = månad) %>% 
    relocate(månad_år, .after = år_månad)
  
  # man kan välja bort long-format, då låter vi kolumnerna vara wide om det finns fler innehållsvariabler, annars
  # pivoterar vi om till long-format, dock ej om det bara finns en innehållsvariabel
  if (long_format & length(cont_koder) > 1) {
    retur_df <- retur_df %>% 
      konvertera_till_long_for_contentscode_variabler(url_uttag)
    
  } # slut if-sats som kontrollera om vi vill ha df i long-format
  
  return(retur_df)
  
} # slut funktion
