hamta_gis_nvdb_homogeniserat_lager_trafikverket <- function(leveransnamn = "Dalarna_med_grannlan",
                                                            filformat = "gdb",                           # filformat för leveransen
                                                            leveransdatum = NA,                          # NA = senaste versionen, annars anges datum som YYYY-MM-DD
                                                            sparafil_sokvag = NA                         # sökväg med avslutande "/" om man vill behålla filen man laddar ner, annars tas den bort
                                                            ) { 
  # =======================================================================================================================
  #
  # Hämta homogeniserat dataset från Lastkajen, Trafikverket, och returnera en lista med ett dataset och en metadatatabell
  # man måste skicka med vad leveransen heter, filformat (zip är default), leveransdatum (eller NA för senaste),
  # om man vill byta filformat (tex från zip till gdb) och om man vill ta bort en del av filnamnet
  #
  # =======================================================================================================================
  
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         keyring,
         jsonlite,
         sf,
         httr)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_filer.R", encoding = "utf-8", echo = FALSE)
  
  # inställningar 
  if (is.na(sparafil_sokvag)) {
    sparafilmapp <- tempfile()
    dir.create(sparafilmapp)
  } else sparafilmapp <- sparafil_sokvag
  
  # Skapa själva mappen (eftersom tempfile() skapar en fil)
  
  # tabortzipfil <- TRUE                  # ta bort zipfilen när alla filer är uppackade
  # temp_mapp <- "/temp"

  inlogg <- POST(url = "https://lastkajen.trafikverket.se/api/Identity/Login", 
                 body = list(UserName = key_list(service = "lastkajen")$username,                                           # användarnamn är sparat under service "lastkajen" i paketet keyring (där lösenord sparas i Windows lösenordshanterare)
                             Password = key_get("lastkajen", key_list(service = "lastkajen")$username)), encode = "json")   # lösenord är sparat under service "lastkajen"
  
  
  if (inlogg$status_code == 200){
    
    resp_inlogg <- fromJSON(httr::content(inlogg, as = "text"), flatten = TRUE)
    bearer_token <- resp_inlogg$access_token
    
    headers <- c("Authorization" = paste0("Bearer ", bearer_token))  # Lägg in Bearer-token i headers, som vi skickar med i nästa GET-anrop 
    
    # ==== hitta rätt mapp för vårt homogeniserade väglager
    path_egnafiler <- "https://lastkajen.trafikverket.se/api/file/GetUserFiles"
    egnafiler <- GET(url = path_egnafiler, config = add_headers(headers))
    resp_egnafiler <- fromJSON(httr::content(egnafiler, as = "text"), flatten = TRUE)
    
    # om man inte skickat med något leveransdatum så blir det det senaste leveransdatumet som finns för leveransen, annars det datum man skickat med
    leveransdatum <- if (is.na(leveransdatum))  max(resp_egnafiler$dateTime) %>% str_sub(1,10) else leveransdatum
    
    # vi sorterar ut den fil som innehåller det leveransnamn man skickat med, är en zip-fil och 
    valt_gislager_filnamn <- resp_egnafiler$name[str_detect(resp_egnafiler$name, paste0("(?=.*", filformat, ")(?=.*", leveransnamn, ")")) & resp_egnafiler$dateTime %>% str_sub(1,10) == leveransdatum]
    valt_gislager_datum_tid <- resp_egnafiler$dateTime[str_detect(resp_egnafiler$name, filformat) & resp_egnafiler$dateTime %>% str_sub(1,10) == leveransdatum]
    
    # här hämtar vi en token för att kunna ladda ner filen vi vill ha - den är giltig i 1 min
    basurl_get_token <- "https://lastkajen.trafikverket.se/api/file/GetUserFileDownloadToken"
    url_get_token <- paste0(basurl_get_token, "?fileName=", valt_gislager_filnamn)
    token_nedl <- GET(url = url_get_token, config = add_headers(headers))
    nedl_token <- fromJSON(httr::content(token_nedl, as = "text"), flatten = TRUE)
    
    # här sker själva nedladdningen
    nedladdnings_basurl <- "https://lastkajen.trafikverket.se/api/file/GetFileStream"
    nedl_url_med_token <- paste0(nedladdnings_basurl, "?token=", nedl_token)
    GET(url = nedl_url_med_token, write_disk(paste0(sparafilmapp, "\\", valt_gislager_filnamn), overwrite = TRUE), progress())
    
    
    # ladda upp filen(filerna) i zip-filen, gör bakcup av filer med samma namn i samma mapp om sådana finns
  
    zipfil_innehall <- unzip(paste0(sparafilmapp, "\\", valt_gislager_filnamn), list = TRUE) %>%
      filter(!str_detect(Name, "/") | str_detect(Name, "/gdb")) %>% 
      mutate(Name = Name %>% str_remove("/gdb"))
    
    zipmapp <- sparafilmapp # substr(sparafilmapp, 1, nchar(sparafilmapp)-1)    # skapa mapp-variabel för zipuppackning
    #zipmapp <- paste0(zipmapp, temp_mapp)
    unzip(paste0(sparafilmapp, "\\", valt_gislager_filnamn), exdir = zipmapp)                        # packa upp zip-fil
    file.remove(paste0(sparafilmapp, "\\", valt_gislager_filnamn))                                   # ta bort zip fil
    
    metadatafil <- read_csv(paste0(sparafilmapp, "\\", "Leveransinformation.txt")) %>% 
      dplyr::pull(`Information om beställning`)
    
    extraherad_info <- metadatafil %>%
      # Extrahera allt som kommer efter kolon, om kolon finns
      str_extract("(?<=: ).*") %>%
      # Byt ut NA med ursprunglig rad för de rader som inte hade kolon
      coalesce(metadatafil) %>%
      # Filtrera bort rader som är tomma eller bara har streck
      .[!str_detect(., "^-{2,}$")]
    
    index_omrade <- which(extraherad_info == "Område:")
    index_dataprodukter <- which(extraherad_info == "Dataprodukter")
    
    # Extrahera olika delar av informationen
    huvuddata <- extraherad_info[1:(index_omrade - 1)]
    geografiskt_omrade <- extraherad_info[(index_omrade + 1):(index_dataprodukter - 1)]
    variabler <- extraherad_info[(index_dataprodukter + 1):length(extraherad_info)]
    
    # Skapa en data frame med kolumnerna baserat på nyckelorden
    metadata_df <- tibble(
      bestallning_namn = huvuddata[1],
      format = huvuddata[3],
      epsg = str_extract(huvuddata[4], "\\d+"),
      epsg_namn = str_remove(huvuddata[4], "EPSG:\\d+\\s*"),
      antal_objekt = huvuddata[5],
      uppdaterad_datum = huvuddata[2] %>% str_sub(1,10),
      uppdaterad_tid = huvuddata[2] %>% str_sub(12,16),
      geografiskt_omrade = list(geografiskt_omrade),
      variabler = list(variabler)
    )
    
    
    
    rm(inlogg)               # ta bort inlogg som innehåller anvnamn och lösenord 
  }
  
  # hämta filnamnet ur zipfil-inneållet, ta bort textfilerna med leveransinformation så blir det bara leveransfilen kvar
  nvdb_homogeniserat <- zipfil_innehall$Name %>% .[!str_detect(zipfil_innehall$Name, ".txt")] %>% paste0(zipmapp, "\\", .)                                                       # paste0(zipmapp, "/", fdb_filnamn)
  
  nvdb_homgen_sf <- st_read(nvdb_homogeniserat)
  
  nvdb_lager <- st_layers(nvdb_homogeniserat)
  
  metadata_df <- metadata_df %>% 
    mutate(lager_namn  = nvdb_lager$name,
    geo_typ = nvdb_lager$geomtype %>% unlist()) %>% 
    relocate(lager_namn, .after = bestallning_namn) %>% 
    relocate(geo_typ, .after = epsg_namn)
  
  nvdb_list <- list(dataset = nvdb_homgen_sf,
                    metadata = metadata_df)
  
  if (is.na(sparafil_sokvag)) unlink(sparafilmapp, recursive = TRUE)
  
  return(nvdb_list)
}

