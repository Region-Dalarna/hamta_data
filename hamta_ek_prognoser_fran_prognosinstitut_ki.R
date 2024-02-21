
hamta_ek_prognoser_fran_prognosinstitut_ki <- function() {
 
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         readxl,
         rvest,
         xml2,
         openxlsx)
  
  options(dplyr.summarise.inform = FALSE)
  url_progn <- "https://www.konj.se/prognosjamforelse"       # url till Konjunkturinstitutets webbsida med ekonomiska prognoser
  
  # ======================== hämta fil med nyckeltabell för prognosinstitut från Konjunkturinstitutet ========================
  # extrahera tabell med koder och klartext för prognosinstitut direkt från konjunkturinstitutets webbsida
  progn_html <- read_html(url_progn)
  
  nyckel_tabell_raw <- progn_html %>% 
    html_nodes(xpath = "//*[@id='svid12_1998392e14f7d36ec6bbe77a']") %>% 
    as.character()
    # xml_contents() %>% 
    # html_text(trim = FALSE) %>% 
    # .[. != ""]
  
  # nyckel_tabell <- tabell_raw %>% 
  #   str_replace_all("(?<=\\p{Lu})(?=\\p{Lu})", "\n") %>% 
  #   str_replace_all(" = ", "\t")
  
  ## lösning när inte rvest och html_nodes och xml_contents fungerade
  # html_txt <- readLines(url_progn)
  #nyckel_tabell_raw <- html_txt[html_txt %>% str_detect("svid12_1998392e14f7d36ec6bbe77a")]
  nyckel_tabell <- nyckel_tabell_raw %>%
    str_extract("<p class=\\\"normal\\\">(.*?)</p></div>\\n</div>") %>%
    str_replace_all("<p class=\\\"normal\\\">|</p></div>\\n</div>", "") %>%
    str_replace_all("<br>", "\n") %>%
    str_replace_all(" = ", "\t")
    
  nyckel_df <- read_delim(nyckel_tabell, delim = "\t", col_names = c("Prognosinstitut", "Prognosinstitut_namn"), show_col_types = FALSE) %>% 
    mutate(Prognosinstitut = Prognosinstitut %>% toupper(),
           Prognosinstitut_namn = case_when(Prognosinstitut == "OECD" ~ "OECD",
                                            Prognosinstitut == "EU" ~ "EU (Kommissionen)",
                                            Prognosinstitut == "LO" ~ "LO",
                                            Prognosinstitut == "REG" ~ "Regeringen",
                                            Prognosinstitut == "Reg" ~ "Regeringen",
                                            Prognosinstitut == "SEB" ~ "SEB",
                                            Prognosinstitut == "SKR" ~ "SKR",
                                            TRUE ~ Prognosinstitut_namn))
  
  # ======================== hämta fil med ekonomiska prognoser från Konjunkturinstitutet ========================
  # hämta html från webbsida
  progn_html <- read_html(url_progn)
  
  # extrahera länk till Progtab.xlsx som finns på webbsidan
  progn_lank <- progn_html %>% 
    html_nodes("a") %>% 
    html_attr("href") %>% 
    str_subset("Progtab.xlsx")
  
  prognos_fil_url <- paste0("https://www.konj.se", progn_lank)
  
  # ladda ner filen som temporär fil som vi läser in och därefter raderar
  temp_fil <- tempfile(fileext = ".xlsx")
  capture.output(download.file(prognos_fil_url, temp_fil, mode = "wb"), type = "message")
  
  # läs in excelfil och lista flikar
  progn_flikar <- excel_sheets(temp_fil)
  
  # ta bort flik som innehåller ordet "Utskrift"
  progn_flikar <- progn_flikar[!grepl("Utskrift", progn_flikar)]
  
  # Funktion för att bearbeta excelfil med prognoser
  bearbeta_excelflik <- function(excelfil, excelflik, tabort_na_varden = FALSE) {
  
    inlast_df <- read_excel(excelfil, sheet = excelflik, range = cell_cols("A:YA"), .name_repair = "minimal")
    
    # hitta prognosåret i kolumnamnet eller i första kolumnen
    # Steg 1: Sök efter kolumnnamn som innehåller "Prognosjämförelse"
    finns_i_kolumn <- inlast_df %>% 
      select(contains("Prognosjämförelse")) %>% 
      names()
    
    names(inlast_df) <- paste0("Kolumn_", 1:ncol(inlast_df))
    
    # Steg 2: Extrahera numeriskt värde om kolumn finns, annars leta i raderna
    prognos_ar <- if (length(finns_i_kolumn) > 0) {
      finns_i_kolumn %>% 
        parse_number()
    } else {
      inlast_df %>% 
        filter(!is.na(inlast_df[[1]]),
          str_detect(inlast_df[[1]], "Prognosjämförelse")) %>%
        pull(1) %>%
        map_dbl(parse_number) %>%
        first()
    }
    
    start_rad <- which(!is.na(inlast_df[[2]]))[1]
    slut_kolumn <- which(is.na(inlast_df[2,]))[1]-1
    kolumn_namn <- inlast_df[start_rad,]
    
    slut_kolumn <- kolumn_namn %>% 
      select(-c(1:2)) %>%  # Exkluderar första kolumnen
      map_lgl(~ .[1] == kolumn_namn[1, 2]) %>% # Jämför med första raden i kolumn 2
      which(. == TRUE) %>% 
      first() %>% 
      "+"(1)
    
    kolumn_namn <- kolumn_namn %>% unlist(use.names = FALSE) %>% .[c(1:slut_kolumn)]
    
    prognoser_df <- inlast_df %>% 
      slice(start_rad:nrow(inlast_df)) %>% 
      select(c(1:slut_kolumn)) %>% 
      #set_names(kolumn_namn) %>% 
      filter(!is.na(Kolumn_1)) %>% 
      rename(variabel = Kolumn_1)
    
    nya_kolumnnamn <- as.character(prognoser_df[[1]])
    
    prognoser_long <- t(prognoser_df[,-1]) %>% 
      as.data.frame() %>% 
      set_names(nya_kolumnnamn) %>%
      mutate(Publiceringsdatum = as.Date(as.Date("1899-12-30") + as.numeric(Publiceringsdatum)),
             prognos_ar = prognos_ar) %>%
      pivot_longer(cols = -c("Prognosinstitut", "prognos_ar", "Publiceringsdatum", "Publiceringsvecka"),
                   names_to = "variabel",
                   values_to = "varde") %>% 
      mutate(varde = varde %>% str_remove_all("\\*") %>% str_replace_all(",", "\\.") %>% as.numeric()) %>% 
      filter(!is.na(varde)) %>% 
      group_by(prognos_ar, Prognosinstitut, variabel) %>% 
      mutate(senaste_progn = max(Publiceringsdatum)) %>% 
      ungroup() %>% 
      filter(Publiceringsdatum == senaste_progn)
  
    if (tabort_na_varden) prognoser_long <- prognoser_long %>% filter(!is.na(varde))
    
    return(prognoser_long)
  } # slut funktion
  
  
  prognoser_df <- map(progn_flikar, ~ bearbeta_excelflik(excelfil = temp_fil, 
                                                         excelflik = .x,
                                                         tabort_na_varden = TRUE),) %>% 
    list_rbind()
  
  prognoser_df2 <- prognoser_df %>% 
    left_join(nyckel_df, by = "Prognosinstitut")

  unlink(temp_fil)
  return(prognoser_df2)
} # slut funktion hamta_ek_prognoser_fran_prognosinstitut_ki
