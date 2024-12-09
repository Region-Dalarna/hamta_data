
#source("G:/skript/hamta_data/func_skolverket_webbtabeller.R", encoding = "utf-8", echo = FALSE)


skolverket_komvux_yrkesinriktning <- function(region_vekt = "*",                        # NA = riket, alla län och alla kommuner
                                              konvertera_andel_till_numerisk = TRUE,      # TRUE = numerisk kolumn av andel, då försvinner prickar och liknande och blir NA. Vill man se vad som är prickar och hur många det är kan man sätta denna till FALSE
                                              ta_bort_na_varden = TRUE                    # TRUE = tar bort när antal är NA, annars behålls de raderna
                                              ){
  
  # ==================================================================================================================
  #
  # Skript för att hämta yrkesinriktningar på komvux från Skolverket per län och kommun, samt för riket. Detta skript hämtar alla 
  # kommuner i en excelfil, en per år. Det går således inte att snabba upp skriptet genom att välja någon eller några 
  # få kommuner. Däremot kan man begränsa hur många år man hämtar. 
  #
  # Skapat av: Peter Möller, Region Dalarna
  #
  # ==================================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         readxl,
         glue,
         httr)
  
 source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R", encoding = "utf-8", echo = FALSE)
  
    # om region_Vekt är NA så hämtas alla län, kommuner och riket
    if (all(is.na(region_vekt))) region_vekt <- hamtaregtab()$regionkod %>% 
      .[. != "00"]
    
      GET(fil_url, write_disk(tf_excelfil <- tempfile(fileext = ".xlsx")))
      flikar <- excel_sheets(tf_excelfil) %>% .[!str_detect(., "Definition")]
      ar_txt <- suppressMessages(read_excel(tf_excelfil, sheet = 1, range = "A1:A2", col_types = "text")) %>%
        dplyr::pull(1) %>% 
        str_extract("\\b\\d{4}\\b")
      
      vux_yrkesinr_df <- suppressMessages(map(flikar, ~ read_excel(tf_excelfil, sheet = .x, skip = 3, col_types = "text") %>%
                                                rename(regionkod = 1,
                                                       region = 2) %>% 
                                                filter(regionkod != "Samtliga kommuner") %>% 
                                                mutate(regionkod = ifelse(regionkod == "Totalt i Riket", "00", regionkod),
                                                       region = ifelse(region == "Totalt i Riket", "Riket", region)) %>% 
                                                pivot_longer(-c(regionkod, region), values_to = "antal", names_to = "yrkesinriktning") %>% 
                                                mutate(typ = case_when(str_detect(.x, "A") ~ "Totalt",
                                                                       str_detect(.x, "B") ~ "Nationella yrkespaket",
                                                                       str_detect(.x, "C") ~ "Regionala yrkespaket"),
                                                       ar = ar_txt,
                                                       antal = antal %>% as.numeric()))) %>% 
        list_rbind() %>% 
        select(ar, regionkod, region, typ, yrkesinriktning, antal)
  
      if (ta_bort_na_varden) vux_yrkesinr_df <- vux_yrkesinr_df %>% 
        filter(!is.na(antal))
      
      if (!all(region_vekt == "*")) vux_yrkesinr_df <- vux_yrkesinr_df %>% 
        filter(regionkod %in% region_vekt)
      
      return(vux_yrkesinr_df)
      
} # slut funktion
   
  