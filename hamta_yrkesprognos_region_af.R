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
  writeBin(content(yrkesbarometer, as = "raw"), tempfil_sokvag)
  
  # Läs in Excel-filen
  
  flikar <- excel_sheets(tempfil_sokvag)
  yrkesprognos <- read_xlsx(tempfil_sokvag, sheet = flikar[str_detect(flikar, "barometer")])
  
  return(yrkesprognos)
} # slut funktion
