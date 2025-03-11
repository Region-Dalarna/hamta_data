if (!require("pacman")) install.packages("pacman")
p_load(rvest,
       tidyverse,
       readxl)

hamta_marknadsbalans_sjv <- function(produkt = "mejeriprodukter", flik_sokord = "ekvivalenter") {
  # Funktion för att hämta marknadsbalans från Jordbruksverket, för en viss produkt
  # produkt = "mejeriprodukter", "griskott", "farkott", "matfagel", "notkott", "hastkott", "mejeriprodukter" eller "viltkott"
  # flik_sokord = t.ex. "ekvivalenter" eller "balans" men man får kolla fliken manuellt för att se vad den heter
  
  # Ändrar download.file till GET på rad 31-32 för att se om det funkar. / Jon 2025-03-11
  
  ## Ange URL till sidan som innehåller länken till marknadsbalans
  url_inlas_sjv <- "https://jordbruksverket.se/mat-och-drycker/handel-och-marknad/priser-och-marknadsinformation-for-livsmedel"
  
  ## Parsa HTML-innehållet
  doc <- read_html(url_inlas_sjv)
  
  ### Skrapa alla länkar på webbsidan
  links <- doc %>%
    html_nodes("a") %>%
    html_attr("href")
  
  # hitta den länk som innehåller produkt och "Marknadsbalans"
  lank_fil <- links[str_detect(links, "Marknadsbalans") & str_detect(links, produkt)] %>% .[!is.na(.)] %>%
    paste0("https://jordbruksverket.se", .)
  
  td = tempdir()              # skapa temporär mapp
  marknadsbalans_fil <- tempfile(tmpdir=td, fileext = ".xlsx")
  
  #download.file(lank_fil, destfile = marknadsbalans_fil, mode = "wb")       # ladda hem hela filen, mode = "wb" viktigt, annars blir det fel
  GET(lank_fil, write_disk(marknadsbalans_fil, overwrite = TRUE))
  # hämta namn på den flik som innehåller texten i flik_sokord
  
  vald_flik <- excel_sheets(marknadsbalans_fil) %>%        # kolla vilka flikar som finns i filen
    .[str_detect(., flik_sokord)]
  
  # läs in data från fliken
  marknadsbalans_df <- read_excel(marknadsbalans_fil, sheet = vald_flik, col_names = FALSE)
  
  # hitta forsta raden med texten "År" i kolumn 1
  forsta_rad <- marknadsbalans_df %>% 
    dplyr::pull(1) %>% 
    str_which("År")
  
  # spara kolumnnamnen i en vektor
  kolnamn <- unlist(marknadsbalans_df[forsta_rad,], use.names = FALSE)
  
  # ta bort tomma rader innan och efter datasetet, döp kolumnerna
  marknadsbalans_df <- marknadsbalans_df %>% 
    slice((forsta_rad+1):nrow(.)) %>%
    filter(!is.na(.[[2]])) %>% 
    setNames(kolnamn)

  return(marknadsbalans_df)
}
