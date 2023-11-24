# Skript som hämtar data för varsel på branschnivå (år) genom webscraping och sedan sparar till Excel (om användaren vill)
# Notera att källan bara har data för år som är fullständiga (dvs. med data upp till och med december)
library(tidyverse)
library(openxlsx)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
options(dplyr.summarise.inform = FALSE)

#test=hamta_varsel_bransch_ar(output_mapp="G:/skript/projekt/laget_i_Dalarna/Data/",spara_till_excel=TRUE)
hamta_varsel_bransch_lan_af <- function(region_vekt = NA               # NA = alla regioner, annars länskoder
                                        ) {
  
  regionnyckel <- hamtaregtab() %>%                          # ladda ner en nyckel för regioner och regionkoder
    filter(nchar(regionkod) == 2) %>% 
    mutate(region = region %>% skapa_kortnamn_lan(byt_ut_riket_mot_sverige = TRUE)) 
  
  # ================================== nedladdning av fil ============================================
  
  # hämta webbsidan med tidigare statistik på Arbetsförmedlingen och spara som en vektor
  webbsida <- suppressWarnings(readLines("https://arbetsformedlingen.se/statistik/sok-statistik/tidigare-statistik"))
  
  varsel_index <- which(str_detect(webbsida, "varsel"))   # sök alla rader där "varsel" finns med
  xlsx_index <- which(str_detect(webbsida, ".xlsx"))      # sök alla rader där ".xlsx" finns med
  
  fil_index <- varsel_index[varsel_index %in% xlsx_index]    # ta index där båda är med
  fil_strang <- webbsida[fil_index]                          # skapa sträng med det element där båda är med
  
  # i den strängen, ta ut startposition för alla "/download/" som hittar i strängen (det är sökvägar)
  start_sokvagar <- str_locate_all(fil_strang, "/download/")[[1]][,1]  
  
  # funktion för att ta ut fullständig url från de startpositioner vi hittade i raden ovan
  extrahera_sokvag <- function(strang, startpos) {
    
    nystrang <- str_sub(strang, startpos, nchar(strang))
    slutpos <- str_locate(nystrang, '\"')[[1]]-1
    
    retur_strang <- str_sub(nystrang, 1, slutpos)
    retur_strang <- paste0("https://arbetsformedlingen.se", retur_strang)
    return(retur_strang)
  }       
  
  # vi skapar en vektor med fullständiga sökvägar för samtliga excelfiler som finns på webbsidan
  af_urler <- start_sokvagar %>% map_chr(~ extrahera_sokvag(fil_strang, .x))
  
  # ta ut sökväg för varsel per län, dvs en sökväg som innehåller "varsel" och "lan" men inte "bransch"
  varsel_lan_url <- af_urler[str_detect(af_urler, "varsel") & str_detect(af_urler, "lan") & str_detect(af_urler, "bransch")]   
  
  # spara filen temporärt för att kunna extrahera fliknamn och kolla startrad
  
  td = tempdir()              # skapa temporär mapp
  varsel_fil <- tempfile(tmpdir=td, fileext = ".xlsx")
  
  download.file(varsel_lan_url, destfile = varsel_fil, mode = "wb")       # ladda hem hela filen, mode = "wb" viktigt, annars blir det fel
  fliknamn <- getSheetNames(varsel_fil)               # hämta alla fliknamn ur Excelfilen
  
  las_in_flik <- function(valt_ar) {
  
    # Väljer startrad
    startrads_test_df <- read.xlsx(varsel_lan_url, sheet = valt_ar, skipEmptyRows = FALSE)
    startrad <- which(!is.na(startrads_test_df[[1]]))[1]+1
    
    # läs in Excelfilen till en df direkt från url:en
    varsel_df <-read.xlsx(varsel_lan_url, sheet = valt_ar, startRow = startrad) 
    slutrad <- which(varsel_df[[2]] == "Summa")[1]-1
    varsel_df[slutrad,1] <- "US"
    # ===================================== slut på hämtning av fil =====================================
    
    # pivotera df:n så att det blir long-format och gör diverse filtreringar och bearbetningar av data
    varsel_df <- varsel_df %>% 
      slice(1:slutrad) %>% 
      pivot_longer(3:ncol(varsel_df), names_to = "Region", values_to = "Antal") %>%
      mutate(Region = Region %>% str_replace("\\.", " ") %>% 
               str_replace("\\.", " ") %>% 
               skapa_kortnamn_lan(byt_ut_riket_mot_sverige = TRUE),
             År = valt_ar) %>% 
      left_join(regionnyckel, by = c("Region" = "region")) %>% 
      rename(Regionkod = regionkod) %>% 
      relocate(Regionkod, .before = "Region") %>% 
      relocate(År, .before = 1)
    
    if (!is.na(region_vekt)) {         # om man valt bara någon eller några regioner
      
      varsel_df <- varsel_df %>% 
        filter(Regionkod %in% region_vekt)
    }
      
  return(varsel_df)
    
  } # slut funktion las_in_flik
  
  retur_df <- map_dfr(fliknamn, ~ las_in_flik(.x))
  return(retur_df)
}

