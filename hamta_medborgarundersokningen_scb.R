library(tidyverse)
library(pxweb)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)


hamta_data_medborgarundersokningen_scb <- function(region_vekt = "20",             # går att skicka flera som en vektor, både riket, län och kommuner
                                                   bakgrund_klartext = "*",        # finns: "samtliga", "män", "kvinnor", "ålder - 18-29 år", "ålder - 30-49 år", "ålder - 50-64 år", "ålder - 65 år eller äldre", "utbildning- Ingen gymnasial utbildning eller gymnasial utbildning [högst 2 år]", "utbildning -Gymnasial utbildning ", "utbildning - Eftergymnasial utbildning", "var man bor i kommunen -  Centralorten", "var man bor i kommunen - Annan tätort", "var man bor i kommunen - Utanför tätort", "boendetid i kommun - 2 år eller kortare tid", "boendetid i kommun - 3-5 år", "boendetid i kommun - 6-10 år", "boendetid i kommun - 11 år eller längre tid", "inkomst - Låg", "inkomst - Mellan", "inkomst - Hög"
                                                   svarsalternativ_klartext = "*", # finns: "andel mycket  dåligt", "andel mycket dåligt - Osäkerhetstal", "andel ganska dåligt", "andel ganska dåligt - Osäkerhetstal", "andel ganska bra", "andel ganska bra - Osäkerhetstal", "andel mycket bra", "andel mycket bra - Osäkerhetstal", "andel höga betyg (mycket och ganska bra)", "andel höga betyg (mycket och ganska bra) - Osäkerhetstal", "andel låga betyg (mycket och ganska dåligt)", "andel låga betyg (mycket och ganska dåligt) - Osäkerhetstal", "antal svarande på enkäten"
                                                   tid_koder = "*"                 # "9999" = senaste år
                                                   ) {                             # "*" = alla värden, går att skicka med flera värden ovan i en vektor, alltså: c("samtliga", "kvinnor", "ålder - 50-64 år")
  
                             

  
  alla_urler <- c("https://api.scb.se/OV0104/v1/doris/sv/ssd/ME/ME0003/ME0003A/MedborgSkolaOms",
                  "https://api.scb.se/OV0104/v1/doris/sv/ssd/ME/ME0003/ME0003B/MedborgBoende",
                  "https://api.scb.se/OV0104/v1/doris/sv/ssd/ME/ME0003/ME0003C/MedborgArbUtb",
                  "https://api.scb.se/OV0104/v1/doris/sv/ssd/ME/ME0003/ME0003J/MedborgKlimatMiljo",
                  "https://api.scb.se/OV0104/v1/doris/sv/ssd/ME/ME0003/ME0003K/MedborgBemotande",
                  "https://api.scb.se/OV0104/v1/doris/sv/ssd/ME/ME0003/ME0003K/MedborgInfo",
                  "https://api.scb.se/OV0104/v1/doris/sv/ssd/ME/ME0003/ME0003K/MedborgInflytande",
                  "https://api.scb.se/OV0104/v1/doris/sv/ssd/ME/ME0003/ME0003K/MedborgInfoForandr"
                  )
  
  # funktionen som kommer att användas i map-funktionen nedan, dvs. det är här alla data laddas ner och bearbetas tabell för tabell
  hamta_medb_data <- function(url_tab) {
  
    bakgr_koder <- if(all(bakgrund_klartext == "*")) "*" else hamta_kod_med_klartext(url_tab, bakgrund_klartext, skickad_fran_variabel = "medbakgrund")
    svarsalt_koder <- if(all(svarsalternativ_klartext == "*")) "*" else hamta_kod_med_klartext(url_tab, svarsalternativ_klartext, skickad_fran_variabel = "ContentsCode")
    
    # hantering av tid (i detta fall år) och att kunna skicka med "9999" som senaste år
    giltiga_ar <- hamta_giltiga_varden_fran_tabell(url_tab, "tid")
    if (all(tid_koder != "*")) tid_koder <- tid_koder %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar]
    
    
    
    varlista <- list(
      Region = region_vekt,
      MedbVariabel = "*",
      MedBakgrund = bakgr_koder,
      ContentsCode = svarsalt_koder,
      Tid = tid_koder
    )
  
    # Hämta namn på medborgarvariabel i just den här tabellen och döp om den om den heter MedbBariabel med siffra på slutet
    Medb_var <- pxvarlist(url_tab)$koder[str_detect(pxvarlist(url_tab)$koder, "MedbVariabel")]
    names(varlista)[str_detect(names(varlista), "MedbVariabel")] <- Medb_var     
    
    px_uttag <- pxweb_get(url = url_tab, query = varlista)              # hämta data från pxweb
    
    px_df <- as.data.frame(px_uttag) %>%                        # spara i en dataframe, plocka med regionkoder 
      cbind(as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>%
              select(Region)) %>% 
      rename(regionkod = Region) %>% relocate(regionkod, .before = region)
    
    # hämta frågan som är variabeln MedbVariabel i klartext
    medb_fraga_klartext <- pxvarlist(url_tab)$klartext[str_detect(pxvarlist(url_tab)$koder, "MedbVariabel")]
    
    retur_df <- px_df %>% 
      pivot_longer(all_of(medb_fraga_klartext), names_to = "fraga", values_to = "delfraga") %>% 
      relocate(år, .before = 1) %>% 
      relocate(fraga, .after = `medborgarnas bakgrund`) %>% 
      relocate(delfraga, .after = fraga)
  
   return(retur_df)
    
  } # slut funktion hamta_medb_data
  
  
  medb_list <- map(alla_urler, ~ hamta_medb_data(url_tab = .x), .progress = TRUE)                    # hämta hem alla tabeller till en lista 
  medb_df <- medb_list %>% list_rbind()                                            # lägger ihop alla dataframes i listan med rbind
  
  return(medb_df)
}
