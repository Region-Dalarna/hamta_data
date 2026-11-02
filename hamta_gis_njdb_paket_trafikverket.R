hamta_gis_datapaket_trafikverket <- function(paketnamn,
                                             filnamn_monster = NA,       # del av filnamnet om paketet har flera filer, NA = alla filer är kandidater
                                             lager = NA,                 # NA = alla lager i geofilen, annars en vektor med lagernamn
                                             kolumnnyckel = NA,          # sökväg (eller url) till en csv med kolumnerna "attribut", "alias" och ev. "behall", eller en tabell med samma kolumner. NA = alla kolumner, originalnamn
                                             sparafil_sokvag = NA) {     # sökväg om man vill behålla den nedladdade filen, annars tas den bort
  # =======================================================================================================================
  #
  # Hämta den senast uppdaterade filen i ett färdigt datapaket från Lastkajen, Trafikverket, och returnera en lista med
  #   - dataset:  en namngiven lista med ett sf-objekt per lager i geofilen (gpkg eller gdb)
  #   - metadata: en tabell med en rad per lager
  #
  # Skiljer sig från hamta_gis_njdb_trafikverket() genom att filen hämtas från Lastkajens publicerade datapaket
  # och inte från "Mina filer". Inloggningsuppgifter ska ligga i keyring under servicen "lastkajen".
  #
  # =======================================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse, keyring, jsonlite, sf, glue, httr, lubridate)
  
  # ---- hjälpfunktion: välj först ut kolumnerna (urval) och döp sedan om dem till alias
  # alias_tabell: tabell med kolumnerna "attribut" och "alias"
  # urval:        vektor med attribut att behålla. NULL = alla attribut som finns i alias_tabell
  tillampa_kolumnnyckel <- function(sf_lager, alias_tabell, urval = NULL) {
    if (!all(c("attribut", "alias") %in% names(alias_tabell))) stop("Kolumnnyckeln måste ha kolumnerna 'attribut' och 'alias'.")
    
    nyckel <- alias_tabell %>%
      transmute(attribut = str_remove_all(attribut, "\\s+"),     # ta bort mellanslag som kan ha kommit in i attributnamnen
                alias = str_squish(alias)) %>%
      filter(!is.na(attribut), !is.na(alias), alias != "")
    
    if (is.null(urval)) urval <- nyckel$attribut
    urval <- str_remove_all(urval, "\\s+")
    
    # ---- steg 1: urval av kolumner (matchar oberoende av stora/små bokstäver)
    traff <- match(str_to_upper(urval), str_to_upper(names(sf_lager)))
    if (any(is.na(traff))) warning("Attribut i urvalet som inte finns i lagret: ", paste(urval[is.na(traff)], collapse = ", "), call. = FALSE)
    
    orig_namn <- names(sf_lager)[traff[!is.na(traff)]]
    if (length(orig_namn) == 0) stop("Ingen av kolumnerna i urvalet finns i lagret.")
    
    sf_lager <- sf_lager %>% select(all_of(orig_namn))            # geometrikolumnen följer med automatiskt i sf
    
    # ---- steg 2: koppla på aliasnamnen för de kolumner som blev kvar
    alias_for_valda <- nyckel$alias[match(str_to_upper(orig_namn), str_to_upper(nyckel$attribut))]
    
    saknar_alias <- orig_namn[is.na(alias_for_valda)]
    if (length(saknar_alias) > 0) {
      warning("Kolumner i urvalet som saknar alias i kolumnnyckeln (behåller originalnamnet): ", paste(saknar_alias, collapse = ", "), call. = FALSE)
      alias_for_valda[is.na(alias_for_valda)] <- saknar_alias
    }
    
    if (anyDuplicated(alias_for_valda) > 0) stop("Aliasen för de valda kolumnerna är inte unika: ", paste(alias_for_valda[duplicated(alias_for_valda)], collapse = ", "))
    
    sf_lager %>% rename(all_of(set_names(orig_namn, alias_for_valda)))     # nytt namn = alias
  }
  
  # ---- hjälpfunktion: läs kolumnnyckeln från en csv (sökväg eller url)
  # Kolumner: attribut, alias och (valfritt) behall. Är behall med behålls bara rader markerade ja/j/1/x/true.
  # Avgränsare (tab, semikolon eller komma) hittas automatiskt.
  las_kolumnnyckel <- function(fil) {
    forsta_rad <- readLines(fil, n = 1, warn = FALSE)
    avgransare <- c("\t", ";", ",")[which.max(str_count(forsta_rad, c("\t", ";", ",")))]
    
    nyckel <- read_delim(fil, delim = avgransare, col_types = cols(.default = col_character()), trim_ws = TRUE, show_col_types = FALSE)
    names(nyckel) <- names(nyckel) %>% str_to_lower() %>% str_squish()
    
    if (!all(c("attribut", "alias") %in% names(nyckel))) stop("Kolumnnyckeln måste ha kolumnerna 'attribut' och 'alias'. Hittade: ", paste(names(nyckel), collapse = ", "))
    nyckel
  }
  
  # ---- läs kolumnnyckeln först, så att ett fel i filen upptäcks innan vi laddar ner något
  nyckel_att_anvanda <- NULL
  if (is.character(kolumnnyckel) && !is.na(kolumnnyckel)) kolumnnyckel <- las_kolumnnyckel(kolumnnyckel)
  
  if (is.data.frame(kolumnnyckel)) {
    ar_ja <- function(x) str_to_lower(str_squish(as.character(x))) %in% c("ja", "j", "1", "x", "true", "sant", "yes", "y")
    
    # steg 1 (urval): rader markerade i kolumnen behall. Saknas kolumnen behålls alla attribut i nyckeln
    urval <- if ("behall" %in% names(kolumnnyckel)) kolumnnyckel$attribut[ar_ja(kolumnnyckel$behall)] else NULL
    nyckel_att_anvanda <- list(urval = urval, alias = kolumnnyckel)
    
    message("Kolumnnyckel inläst: ", nrow(kolumnnyckel), " attribut, ", if (is.null(urval)) "alla behålls" else paste0(length(urval), " valda"), ".")
  }
  
  # mapp där vi packar upp filen
  if (is.na(sparafil_sokvag)) {
    sparafilmapp <- tempfile()
    dir.create(sparafilmapp)
  } else {
    sparafilmapp <- sparafil_sokvag
    dir.create(sparafilmapp, showWarnings = FALSE, recursive = TRUE)
  }
  
  # ---- logga in
  anvandare <- key_list(service = "lastkajen")$username[1]
  inlogg <- POST(url = "https://lastkajen.trafikverket.se/api/Identity/Login",
                 body = list(UserName = anvandare, Password = key_get("lastkajen", anvandare)),
                 encode = "json")
  
  if (inlogg$status_code != 200) stop("Inloggningen till Lastkajen misslyckades (statuskod ", inlogg$status_code, ").")
  
  bearer_token <- fromJSON(httr::content(inlogg, as = "text"), flatten = TRUE)$access_token
  rm(inlogg)                                              # ta bort inloggningen så att uppgifterna inte ligger kvar
  headers <- c("Authorization" = paste0("Bearer ", bearer_token))
  
  # ---- hitta datapaketet på namnet
  paket_df <- GET("https://lastkajen.trafikverket.se/api/DataPackage/GetPublishedDataPackages",
                  config = add_headers(headers)) %>%
    httr::content(as = "text") %>%
    fromJSON(flatten = TRUE)
  
  valt_paket <- paket_df %>% filter(str_detect(name, regex(paketnamn, ignore_case = TRUE)))
  if (nrow(valt_paket) != 1) stop("Förväntade exakt ett datapaket som matchar '", paketnamn, "' men hittade ", nrow(valt_paket),
                                  if (nrow(valt_paket) > 1) paste0(": ", paste(valt_paket$name, collapse = ", ")) else ".")
  paket_id <- valt_paket$id
  
  # ---- hitta senaste uppdaterade filen i paketet
  filer_df <- GET("https://lastkajen.trafikverket.se/api/DataPackage/GetDataPackageFiles",
                  query = list(Id = paket_id),
                  config = add_headers(headers)) %>%
    httr::content(as = "text") %>%
    fromJSON(flatten = TRUE)
  
  kandidater <- filer_df %>% filter(!isFolder)
  if (!is.na(filnamn_monster)) kandidater <- kandidater %>% filter(str_detect(name, regex(filnamn_monster, ignore_case = TRUE)))
  if (nrow(kandidater) == 0) stop("Hittade ingen fil i datapaketet '", valt_paket$name, "' (filnamn_monster = ", filnamn_monster, ").")
  
  senaste_fil <- kandidater %>% slice_max(ymd_hms(dateTime), n = 1, with_ties = FALSE)
  message("Hämtar fil: ", senaste_fil$name, " (uppdaterad ", senaste_fil$dateTime, ")")
  
  # ---- token (giltig i 60 sekunder, kan användas en gång) och nedladdning
  nedl_token <- GET("https://lastkajen.trafikverket.se/api/file/GetDataPackageDownloadToken",
                    query = list(id = paket_id, fileName = senaste_fil$name),
                    config = add_headers(headers)) %>%
    httr::content(as = "text") %>%
    fromJSON()
  
  filsokvag <- file.path(sparafilmapp, senaste_fil$name)
  nedladdning <- GET("https://lastkajen.trafikverket.se/api/File/GetDataPackageFile",
                     query = list(token = nedl_token),
                     write_disk(filsokvag, overwrite = TRUE), progress())
  
  if (nedladdning$status_code != 200) stop("Nedladdningen misslyckades (statuskod ", nedladdning$status_code, ").")
  
  # ---- packa upp om det är en zip
  if (str_detect(filsokvag, regex("\\.zip$", ignore_case = TRUE))) {
    unzip(filsokvag, exdir = sparafilmapp)
    file.remove(filsokvag)
  }
  
  # ---- hitta geofilen: antingen en .gpkg-fil eller en .gdb-mapp
  gpkg_filer <- list.files(sparafilmapp, pattern = "\\.gpkg$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  gdb_mappar <- list.dirs(sparafilmapp, recursive = TRUE, full.names = TRUE) %>% .[str_detect(., "\\.gdb$")]
  geofiler <- c(gpkg_filer, gdb_mappar)
  
  if (length(geofiler) != 1) stop("Förväntade exakt en .gpkg eller .gdb men hittade ", length(geofiler), ": ", paste(geofiler, collapse = ", "))
  geofil_sokvag <- geofiler
  
  # ---- vilka lager finns och vilka ska vi läsa in
  geofil_lager <- st_layers(geofil_sokvag)
  message("Lager i geofilen: ", paste(geofil_lager$name, collapse = ", "))
  
  lager_att_lasa <- if (all(is.na(lager))) geofil_lager$name else lager
  saknas <- setdiff(lager_att_lasa, geofil_lager$name)
  if (length(saknas) > 0) stop("Lagret/lagren finns inte i geofilen: ", paste(saknas, collapse = ", "))
  
  # ---- läs in varje lager
  dataset <- lager_att_lasa %>%
    set_names() %>%
    map(function(lagernamn) {
      sf_lager <- st_read(geofil_sokvag, layer = lagernamn, quiet = TRUE)
      
      # välj först ut kolumnerna i urvalet och koppla sedan på aliasnamnen
      if (!is.null(nyckel_att_anvanda)) sf_lager <- tillampa_kolumnnyckel(sf_lager, nyckel_att_anvanda$alias, nyckel_att_anvanda$urval)
      
      # döp om geometrikolumnen till geom (bara om lagret har geometri)
      if (inherits(sf_lager, "sf")) {
        geom_kol <- attr(sf_lager, "sf_column")
        if (geom_kol != "geom") sf_lager <- sf_lager %>% dplyr::rename(geom = all_of(geom_kol))
      }
      
      sf_lager
    })
  
  # ---- en metadatarad per lager
  metadata_df <- tibble(lager_namn = geofil_lager$name,
                        geo_typ = map_chr(geofil_lager$geomtype, ~ as.character(.x[1])),
                        antal_rader = as.numeric(geofil_lager$features)) %>%
    filter(lager_namn %in% lager_att_lasa) %>%
    mutate(epsg = map_chr(lager_namn, ~ {
      e <- if (inherits(dataset[[.x]], "sf")) st_crs(dataset[[.x]])$epsg else NA
      as.character(e)
    }),
    paket_namn = valt_paket$name,
    paket_id = paket_id,
    fil_namn = senaste_fil$name,
    fil_uppdaterad = senaste_fil$dateTime,
    nedladdad = format(Sys.time(), "%Y-%m-%d %H:%M")) %>%
    relocate(lager_namn, geo_typ, antal_rader, epsg, .before = 1)
  
  if (is.na(sparafil_sokvag)) unlink(sparafilmapp, recursive = TRUE)
  
  list(dataset = dataset,
       metadata = metadata_df)
}