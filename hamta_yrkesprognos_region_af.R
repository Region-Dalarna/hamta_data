library(tidyverse)
library(rvest)
library(httr)
library(readxl)

hamta_yrkesprognos_region_af <- function() {

    yrken_url <- "https://arbetsformedlingen.se/statistik/yrkes--och-kompetensanalyser"
  
  af_grund_url <- "https://arbetsformedlingen.se"
  # Hämta sidan
  yrken <- read_html(yrken_url)
  
  # extrahera alla länkar på sidan
  yrken_lankar <- yrken %>% 
    html_nodes("a") %>% 
    html_attr("href")
  
  # behåll endast länkar som innehåller "yrkesbarometer" och ".xlsx"
  yrken_lankar <- yrken_lankar %>% 
    str_subset("yrkesbarometer") %>% 
    str_subset(".xlsx")
  
  # extrahera år som är fyra siffror som ligger mellan två bindestreck
  yrken_ar <- yrken_lankar %>% 
    str_extract("(?<=-)(\\d{4})(?=-)")
  
  # och månad som är en sträng som inte innehåller bindestreck eller punkt
  yrken_manad <- yrken_lankar %>% 
    str_extract("[^-]+(?=\\.xlsx)")
  
  # skapa en vektor med alla månader
  manader <- c("januari", "februari", "mars", "april", "maj", "juni", 
               "juli", "augusti", "september", "oktober", "november", "december")
  
  # sätt nummer på månad utifrån namn
  yrken_manad_num <- match(yrken_manad, manader)
  
  # bestäm index för senaste yrkesbarometern
  senaste_index <- paste0(yrken_ar, yrken_manad_num) %>% which.max()
  
  # skapa textsträng med år och månad för aktuell fil
  ar_manad_txt <- paste(yrken_manad[senaste_index], yrken_ar[senaste_index])
  
  nedladdnings_url <- paste0(af_grund_url, yrken_lankar[senaste_index])
  
  yrkesbarometer <- GET(nedladdnings_url)
  
  tempfil_sokvag <- tempfile(fileext = ".xlsx")
  
  # Spara innehållet i den temporära filen
  writeBin(httr::content(yrkesbarometer, as = "raw"), tempfil_sokvag)
  
  # Läs in Excel-filen
  
  flikar <- excel_sheets(tempfil_sokvag)
  yrkesprognos <- read_xlsx(tempfil_sokvag, sheet = flikar[str_detect(flikar, "barometer")])
  
  yrkesprognos_join <- yrkesprognos %>% 
    mutate(ssyk_en = strsplit(ssyk, ", ")) %>%  # Dela upp koderna till listor
    unnest_longer(ssyk_en)  # Gör om listorna till rader
  
  # Läs in yrkesstatistik från scb:s öppna statistikdatabas
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_yrke_SSYK4_region_yrke2012_kon_tid_YREG60BAS_YREG60N_YREG60_scb.R")
  ssyk4_df <- hamta_yrke_SSYK4_region_yrke2012_kon_tid_scb(region_vekt = "*",
                                                           tid_koder = "9999") %>% 
    filter(år == max(år)) %>% 
    pivot_wider(names_from = "kön", values_from = "Antal")
  
  regionnyckel <- hamtaregtab()
  
  konsbalans_vekt <- c("kraftigt kvinnodominerat yrke", 
                       "kvinnodominerat yrke",
                       "yrke med jämn könsbalans",
                       "kraftigt mansdominerat yrke",
                       "mansdominerat yrke")
  
  # Lägg till regionnyckel och yrkesstatistik
  retur_df <- yrkesprognos_join %>%
    mutate(lan_namn = if_else(lan_namn == "Nationellt", "Riket", lan_namn)) %>% 
    left_join(regionnyckel, by = c("lan_namn" = "region")) %>%
    left_join(ssyk4_df, by = c("ssyk_en" = "yrke2012kod", "regionkod")) %>% 
    mutate(antal_tot = män + kvinnor) %>% 
    group_by(omgang, yrkesomrade, yb_yrke, regionkod, region, jobbmojligheter,
             rekryteringssituation, paradox, prognos, text_jobbmojligheter,
             text_rekryteringssituation, ssyk, ssyk_text) %>%
    summarise(antal_tot = sum(antal_tot, na.rm = TRUE),
              män = sum(män, na.rm = TRUE),
              kvinnor = sum(kvinnor, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(
           andel_kvinnor = kvinnor / antal_tot *100,
           konsbalans = case_when(between(andel_kvinnor, 80, 100) ~ "kraftigt kvinnodominerat yrke",
                                  between(andel_kvinnor, 60, 80) ~ "kvinnodominerat yrke",
                                  between(andel_kvinnor, 40, 60) ~ "yrke med jämn könsbalans",
                                  between(andel_kvinnor, 20, 40) ~ "mansdominerat yrke",
                                  between(andel_kvinnor, 0, 20) ~ "kraftigt mansdominerat yrke"),
           konsbalans = factor(konsbalans, levels = konsbalans_vekt))
  
  return(retur_df)
} # slut funktion
