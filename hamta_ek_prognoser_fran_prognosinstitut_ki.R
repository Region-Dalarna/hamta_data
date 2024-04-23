
hamta_ek_prognoser_fran_prognosinstitut_ki <- function(
    prognos_ar = "*", 
    bara_senaste_prognos = TRUE) {
 
  
  # Laddar hem data som Konjunkturinstitutet sammanställt från olika banker och andra institut på hur de prognosticerar 
  # den ekonomiska utvecklingen framåt
  #
  # Man kan ange prognos_ar = "*" för att hämta alla prognosår, eller enskilda år som tex c("2024", "2025", "2026"), ett eller flera
  # bara_senaste_prognos = TRUE om man enbart vill ha varje instituts senaste prognos, vid FALSE så får man alla prognoser de gjort under åren
  
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
                                            TRUE ~ Prognosinstitut_namn),
           Prognosinstitut = Prognosinstitut %>% toupper())
  
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
  
  if (prognos_ar != "*") progn_flikar <- progn_flikar[str_detect(progn_flikar, prognos_ar %>% as.character())]
  
  if (length(progn_flikar) == 0) stop("Valt/valda prognosår finns inte i datasetet")
  
  # Funktion för att bearbeta excelfil med prognoser
  bearbeta_excelflik <- function(excelfil, excelflik, bara_senaste = TRUE) {
  
    inlast_df <- read_excel(excelfil, sheet = excelflik, range = cell_cols("A:YA"), .name_repair = "minimal")
    
    # hitta prognosåret i kolumnamnet eller i första kolumnen
    # Steg 1: Sök efter kolumnnamn som innehåller "Prognosjämförelse"
    finns_i_kolumn <- inlast_df %>% 
      select(all_of(contains("Prognosjämförelse"))) %>% 
      names()
    
    names(inlast_df) <- paste0("Kolumn_", 1:ncol(inlast_df))
    
    # Steg 2: Extrahera numeriskt värde om kolumn finns, annars leta i raderna
    flik_prognos_ar <- if (length(finns_i_kolumn) > 0) {
      finns_i_kolumn %>% 
        parse_number()
    } else {
      inlast_df %>% 
        filter(!is.na(inlast_df[[1]]),
               str_detect(inlast_df[[1]], "Prognosjämförelse")) %>%
        dplyr::pull(1) %>% 
        map_dbl(parse_number) %>%
        first()
    }
    
    # ta bort rader som inte ska vara kvar i datasetet
    retur_df <- inlast_df %>% 
      filter(!if_all(everything(), is.na),
             !str_detect(.[[1]], "där inte annat anges|Kalenderkorrigerad|Årsgenomsnitt|Prognosjämförelse")) 
    
    # transformera datasetet genom att först göra om kolumner till rader och tvärtom, därefter göra om det till long-format
    retur_df <- retur_df %>% t() %>% as.data.frame() %>% 
      setNames(., unlist(.[1, ])) %>%  # Sätt kolumnnamnen till de värden som finns i rad 1
      slice(-1) %>% 
      pivot_longer(cols = 4:20, names_to = "variabel", values_to = "varde", values_drop_na = TRUE) %>% 
      mutate(Publiceringsdatum = convertToDate(Publiceringsdatum),
             prognos_ar = flik_prognos_ar)
    
    # markera det senaste värdet för varje Prognosinstitut och variabel
    retur_df <- retur_df %>% 
      group_by(Prognosinstitut, variabel) %>% 
      mutate(senaste_progn = ifelse(Publiceringsdatum == max(Publiceringsdatum), 1, 0)) %>% 
      ungroup()

    if (bara_senaste) retur_df <- retur_df %>% filter(senaste_progn == 1)
    
    return(retur_df)
  } # slut funktion
  
  
  prognoser_df <- map(progn_flikar, ~ bearbeta_excelflik(excelfil = temp_fil, 
                                                         excelflik = .x,
                                                         bara_senaste = bara_senaste_prognos),) %>% 
    list_rbind() %>% 
    mutate(Prognosinstitut = Prognosinstitut %>% toupper())
  
  prognoser_df2 <- prognoser_df %>% 
    left_join(nyckel_df, by = "Prognosinstitut") %>% 
    select(Prognosinstitut, Prognosinstitut_namn, Publiceringsdatum, Publiceringsvecka, prognos_ar, senaste_progn, 
           variabel, varde)

  unlink(temp_fil)
  return(prognoser_df2)
} # slut funktion hamta_ek_prognoser_fran_prognosinstitut_ki
