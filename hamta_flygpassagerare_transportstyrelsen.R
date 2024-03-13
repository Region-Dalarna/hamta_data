library(tidyverse)
library(rvest)
library(openxlsx)


hamta_flygpassagerare_transportstyrelsen <- function() {

  url_flyg <- "https://www.transportstyrelsen.se/sv/luftfart/statistik/Flygplatsstatistik-/"
  
  # =========================================== funktioner ===========================================
  # skapa funktion som fyller på med värdet till höger på en rad om det saknas värden, om den stöter på ett värde så fyller den på med det 
  # till höger om det saknas värden där
  fyll_pa <- function(df, rad) {
    varde <- NA
    for (kol in 1:ncol(df)) {
      if (!is.na(df[rad,kol])) varde <- df[rad,kol] else if (!is.na(varde)) df[rad, kol] <- varde
    }
    return(df)
  }  
  
  # skapa funktion som tar värdena i de rader som skickas med och lägger ihop till en sträng för varje kolumn och returnerar en vektor med alla text-
  # strängar, en för varje kolumn
  kombinera_rader_till_en_strang <- function(df, rader) {
    kombinerad <- c()
    for (kol in 1:ncol(df)) {
      kombinerad <- c(kombinerad, paste(df[rader,kol], collapse = " "))
    }
    return(kombinerad)
  }
  
  # Funktion för att ta bort dubbletter
  ta_bort_dubbla_ord <- function(vektor) {
    retur_strang <- map_chr(vektor, ~ {
    words <- unlist(strsplit(.x, " "))  # Dela upp strängen i ord
    words <- tolower(words)  # Konvertera alla ord till gemener
    words <- unique(words)  # Ta bort dubbletter
    paste(words, collapse = " ")  # Sammanfoga tillbaka till en sträng
    })
    return(retur_strang)
  }
  # =========================================================================================================
  
  
  # läs in url som html
  doc <- read_html(url_flyg)
  
  ### Skrapa alla länkar på webbsidan
  links <- doc %>%
    html_nodes("a") %>%
    html_attr("href")
  
  # hitta den länk som innehåller produkt och "Marknadsbalans"
  lank_fil <- links[str_detect(links, "passagerarfrekvens") & str_detect(links, "samtliga")] %>% 
    .[!is.na(.)] %>% 
    paste0("https://www.transportstyrelsen.se", .)
  
  
  flyg_list <- map(lank_fil, ~ read.xlsx(.x))
  
  las_in_flygpassagerare <- function(inlast_flik) {
    
    innehall <- inlast_flik[1,1]
    ar <- inlast_flik[2,1]
    flygplats <- inlast_flik[3,1] %>% str_to_sentence()
    trafiktyp <- inlast_flik[4,2]
    
    retur_df <- inlast_flik %>% 
      select(-3) %>% 
      fyll_pa(5) %>% 
      fyll_pa(6) %>% 
      fyll_pa(7) %>% 
      slice(-c(1:4)) %>% 
      janitor::remove_empty("rows")
    
    kolumn_namn <- kombinera(retur_df, c(1:3))
    kolumn_namn[1] <- "År"
    kolumn_namn[2] <- "Månad"
    kolumn_namn <- ta_bort_dubbla_ord(kolumn_namn) %>% str_to_title()
    
    na_columns <- which(is.na(retur_df[nrow(retur_df), ])) %>% .[. != 1]         # hitta kolumner som har na-värden på sista raden (ska tas bort)
    
    retur_df <- retur_df %>% 
      slice(-c(1:3)) %>% 
      setNames(kolumn_namn) %>% 
      select(-(na_columns)) %>% 
      mutate(År = ar) %>% 
      pivot_longer(cols = -c(År, Månad), names_to = "Kategori", values_to = "Passagerare") %>% 
      mutate(Månad = ifelse(Månad == "Summa", "Hela året", Månad),
             Kategori = case_when(Kategori == "Summa Totalt Inrikes" ~ "Summa totalt",
                                  Kategori == "Inrikes Summa Avg" ~ "Inrikes Avg/Ank",
                                  TRUE ~ Kategori),
             Passagerare = Passagerare %>% str_trim() %>% str_remove_all(" ") %>% parse_number())

    return(retur_df)
  }
  
  flyg_df <- map(flyg_list, ~ las_in_flygpassagerare(.x)) %>% 
    bind_rows()
  
  return(flyg_df)
  
} # slut funktion
