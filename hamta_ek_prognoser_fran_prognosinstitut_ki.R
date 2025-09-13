
hamta_ek_prognoser_fran_prognosinstitut_ki <- function(
    prognos_ar = "*", 
    bara_senaste_prognos = TRUE,           # tar bara med vara prognosinstituts senaste år
    ta_bort_gamla_ar = TRUE                # tar bort år som är innan nu, det år som är nu kommer med
    ) {
 
  
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
  url_progn <- "https://www.konj.se/publikationer/prognosjamforelse"       # url till Konjunkturinstitutets webbsida med ekonomiska prognoser
  
  # ======================== hämta fil med nyckeltabell för prognosinstitut från Konjunkturinstitutet ========================
  # extrahera tabell med koder och klartext för prognosinstitut direkt från konjunkturinstitutets webbsida
  progn_html <- read_html(url_progn)
  
  prognos_xlsx_url <- progn_html %>%
    html_nodes("a") %>%
    html_attr("href") %>% 
    .[str_detect(., "prognos") & str_detect(., "xlsx")]
  
  temp_xlsx <- tempfile(fileext = ".xlsx")
  
  # Ladda ner excelfilen till temp-filen
  GET(prognos_xlsx_url, write_disk(temp_xlsx, overwrite = TRUE))
  
  # Se till att tempfilen tas bort när R-sessionen avslutas
  on.exit(unlink(temp_xlsx), add = TRUE)
  
  # Läs in filen som en data.frame (tibble)
  
  progn_flikar <- excel_sheets(temp_xlsx) %>% 
    .[!str_detect(tolower(.), "utskrift")]
  
  institutnyckelflik <- progn_flikar %>% 
    .[str_detect(tolower(.), "inst")]
  
  progn_list <- map(progn_flikar, ~ read.xlsx(temp_xlsx, sheet = .x, colNames = FALSE))
  names(progn_list) <- progn_flikar
  
  institutionnyckel <- pluck(progn_list, institutnyckelflik) %>% 
    rename(inst_kod = 1, Prognosinstitut = 2, Prognosinstitut_eng = 3) %>% 
    mutate(inst_kod = inst_kod %>% toupper())
  
  las_in_flik <- function(inlast_flik, inlast_flik_namn) {
    
    fliknamn <- inlast_flik_namn %>% str_extract("-?[0-9]+\\.?[0-9]*")
    
    inlast_flik <- inlast_flik %>%
      mutate(across(everything(), ~ na_if(., ""))) %>%
      filter(if_any(everything(), ~ !is.na(.))) %>%       # ta bort helt tomma rader
      select(where(~ any(!is.na(.)))) %>%                 # ta bort helt tomma kolumner
      filter(if_any(2:ncol(.), ~ !is.na(.)))
    
    bnp_rad <- inlast_flik %>%
      filter(.[[1]] == "BNP") 
    
    varde_vektor <- unlist(bnp_rad[ , -1])
    sista_kolumn <- max(which(!is.na(varde_vektor))) + 1 
    
    inlast_flik <- inlast_flik[, 1:sista_kolumn]
    
    inst_kod   <- unlist(inlast_flik[1, -1])
    publiceringsdatum <- unlist(inlast_flik[2, -1])
    publiceringsvecka <- unlist(inlast_flik[3, -1])
    
    data <- inlast_flik[-c(1:3), ]
    
    inlast_flik_long <- data %>%
      pivot_longer(
        cols = -1,
        names_to = "col",
        values_to = "varde"
      ) %>%
      mutate(
        variabel           = .[[1]],
        inst_kod    = rep(inst_kod,   times = nrow(data)),
        publiceringsdatum  = as.Date(as.integer(rep(publiceringsdatum, times = nrow(data))), origin = "1899-12-30"),
        publiceringsvecka  = rep(publiceringsvecka, times = nrow(data))
      ) %>%
      select(inst_kod, publiceringsdatum, publiceringsvecka, variabel, varde) %>% 
      mutate(publiceringsdatum = publiceringsdatum %>% as.Date(origin = "1899-12-30"),
             prognos_for_ar = fliknamn) %>% 
      filter(!is.na(varde))
    
    return(inlast_flik_long)
  }
  
  # läs in alla flikar som inte är nyckel för prognosinstitut, koppla på nyckeln och ändra vissa 
  # prognosinstituts namn till kortare versioner
  prognoser_df <- imap(progn_list[!str_detect(tolower(names(progn_list)), "inst")], ~ las_in_flik(.x, .y)) %>% 
    list_rbind() %>% 
    mutate(inst_kod = inst_kod %>% toupper() %>% str_remove("¹")) %>% 
    left_join(institutionnyckel %>% select(-Prognosinstitut_eng), by = "inst_kod") %>% 
    relocate(Prognosinstitut, .after = "inst_kod") %>%
    mutate(Prognosinstitut = case_when(inst_kod == "OECD" ~ "OECD",
                                       inst_kod == "EU" ~ "EU (Kommissionen)",
                                       inst_kod == "LO" ~ "LO",
                                       inst_kod == "SEB" ~ "SEB",
                                       inst_kod == "SKR" ~ "SKR",
                                       inst_kod == "TFIA" ~ "Teknikföret./Industriarbetsg.",
                                       TRUE ~ Prognosinstitut),
           varde = varde %>% parse_number())
    
  # ta bara med varje prognosinstituts senaste prognos för aktuellt år om bara_senaste_prognos == TRUE
  if (bara_senaste_prognos) {
    prognoser_df <- prognoser_df %>%
      group_by(inst_kod, Prognosinstitut, variabel, prognos_for_ar) %>%
      filter(publiceringsdatum == max(publiceringsdatum, na.rm = TRUE)) %>%
      ungroup()
  }
  
  # ta bort år som är före det år som är nu om ta_bort_gamla_ar == TRUE
  if (ta_bort_gamla_ar) {
    prognoser_df <- prognoser_df %>%
      filter(prognos_for_ar >= now() %>% year() %>% as.character())
  }
  
  # om prognos_ar har skickats med, ta bara med dessa
  if (all(prognos_ar != "*")) {
    prognoser_df <- prognoser_df %>%
      filter(prognos_for_ar %in% prognos_ar)
    if (nrow(prognoser_df) == 0) stop("Valt/valda prognosår finns inte i datasetet")
  }
  
  return(prognoser_df)

} # slut funktion hamta_ek_prognoser_fran_prognosinstitut_ki
