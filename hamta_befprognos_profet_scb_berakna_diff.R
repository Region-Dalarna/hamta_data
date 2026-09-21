
hamta_befprognos_diff_data <- function(region_vekt = "20", 
                                   eget_regionnamn = NA,                 # Till diagramrubriken - NULL om namnet ska hämtas automatiskt 
                                   jmfrtid = 10,                         # antal år i jämförelsen, alltså hur många års sikt vi vill titta på beräknat från sista året med befolkningsstatistik, alltså ett år före första prognosår
                                   #JmfrFleraPrognoser = FALSE,           # TRUE om vi vill jämföra med äldre prognoser, FALSE om vi bara vill se den senaste prognosen
                                   # om man skickar med flera url:er så görs en jämförelse
                                   tabeller_url = "G:/Samhällsanalys/Statistik/Befolkningsprognoser/Profet/datafiler/",
                                                                         #  "G:/Samhällsanalys/Statistik/Befolkningsprognoser/Profet/datafiler/"
                                                                         #  c("https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401A/BefProgOsiktRegN",
                                                                         #    "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401B/BefProgOsiktRegN21",
                                                                         #    "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401B/BefProgOsiktRegN20"),         # url-adresser till tabellerna med befolkningsprognoser
                                   output_fold ="G:/Samhällsanalys/API/Fran_R/Utskrift/",        # mapp på datorn som diagrammet skrivs till
                                   gruppera_namn = NA,                   # ange namn om medskickade regioner ska grupperas, annars NA (= grupperas inte)
                                   konsuppdelat = FALSE,                # data kommer könsuppdelat, har ingen lösning idag för att använda könsuppdelad data men den finns där om vi vill framöver
                                   anvand_senaste_befar = FALSE,        # TRUE om vi vill använda senaste tillgängliga år för befolkningsstatistik, annars används första tillgängliga befolkningsprognosår
                                   andel_istallet_for_antal = FALSE,    # om man vill ha procent istället för absolut antal, för skillnad mellan start- och slutår
                                   prognos_ar = "9999",                 # om vi vill ha en prognos för ett specfikt år, "9999" = senaste prognosår - detta gäller endast profet-data, när man skickar med url för SCB finns bara ett år per url
                                   spara_excelfil = FALSE
                                   ) {
  
  if (!requireNamespace("pxweb2r", quietly = TRUE)) remotes::install_github("FaluPeppe/pxweb2r")
  if (!requireNamespace("writexl", quietly = TRUE)) install.packages("writexl")
  if (!requireNamespace("rdverktyg", quietly = TRUE)) remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
  if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
  
  # Ladda skript som sköter regionuttaget och kommuner per region
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_befprognos_scb_data.R")
  
  options(dplyr.summarise.inform = FALSE)
  options(scipen = 999)
  
  # =============== jämförelse x år framåt från senaste tillgängliga år ==========================
  
  folkmangd_url <- c("TAB638", "TAB5557")
  
  senaste_ar_bef <- as.numeric(max(purrr::map(folkmangd_url, ~ pxweb2r::pxweb2_get_values(.x, "tid", quiet = TRUE)$code) |> unlist()))
  # jämförelsetid går mot senaste prognosår så vi behöver justera jmfrtid
  prognos_jmfr_ar <- if (anvand_senaste_befar) senaste_ar_bef + jmfrtid else paste0("+", jmfrtid)
  
  # ========== Hämta befolkningsprognos för tabell(er) i vektor url_tabeller  ====================
  
  befprogn_df <- hamta_befprognos_data(region_vekt = region_vekt,
                                       url_prognos_vektor = tabeller_url,
                                       kon_klartext = c("kvinnor", "män"),
                                       tid_vekt = prognos_jmfr_ar,                        # ny metod, funkar? gammal: paste0("+", jmfrtid), 
                                       cont_klartext = "Folkmängd",
                                       prognos_ar = prognos_ar           # prognos_ar funkar bara för profet-uttag (för uttag från SCB:s API styr url:en vilket år som hämtas men i Profet kan flera år hämtas med samma url om det finns data för flera år i mappen)
                                       ) 
  
  # Här skapar vi en rad med total folkmängd i dfmalar ==========================================
  total_df <- befprogn_df |>  
    dplyr::group_by(regionkod, region, kön, år, prognos_ar) |> 
    dplyr::summarize(dplyr::across(dplyr::where(is.numeric), ~ sum(.x, na.rm = TRUE))) |>
    dplyr::ungroup() |> 
    dplyr::mutate(ålder = "totalt ålder")

  # Lägg på total_df som rad på dfmalar
  befprogn_df <- dplyr::bind_rows(befprogn_df, total_df) 
  
  prognosar <- befprogn_df$prognos_ar |> unique()                         # lägg prognosår(en) i en vektor
  startar <- as.character(as.numeric(prognosar) - 1)    # lägg startår(en) i en vektor
  slutar <- as.character(as.numeric(startar) + jmfrtid)   # lägg slutår(en) i en vektor
  
  # =========================== hämta senaste tillgängliga år i befolkingsstatistiken ============================
  
  # Folkmängden efter region, civilstånd, ålder och kön. År 1968-  (äldre tidsserie + CKM)
  px_df_bef <- pxweb2r::pxweb2_get_data(
    table = c("TAB638", "TAB5557"),
    query = list(
      Region = region_vekt,
      Civilstand = c("OG", "G", "SK", "ÄNKL"),
      Alder = c(0:99, "100+1", "TOT1"),
      Kon = c("män", "kvinnor"),
      ContentsCode = "Folkmängd",
      Tid = startar
    ), quiet = TRUE, on_all_values_invalid = "null") |> 
    dplyr::group_by(regionkod = region_kod, region, ålder, kön, år) |> 
    dplyr::summarise(Folkmängd = sum(value, na.rm = TRUE), .groups = "drop") |> 
    dplyr::mutate(prognos_ar = as.character(as.numeric(år) + 1))
  
  # ================= lägg ihop och bearbeta prognos- och befolkningsdata ==================
  
  # skapa en df med både prognosvärden och befolkningssiffror för senaste året innan prognosen startar
  progn_bef <- dplyr::bind_rows(px_df_bef, befprogn_df) 
  
  suppressWarnings(progn_bef <- progn_bef |>              # skippa felmeddelanden då det inte är någon fara (kan inte översätta NA-värden till numeric)
                     dplyr::mutate(aldernum = readr::parse_number(ålder)))                # Skapa en numerisk åldersvariabel 
  
  # Lägg ihop i åldersgrupper
  progn_bef <- progn_bef |> 
    dplyr::mutate(aldergrp = dplyr::case_when(aldernum < 20 ~ "0-19 år", 
                                dplyr::between(aldernum, 20, 64) ~ "20-64 år",
                                dplyr::between(aldernum, 65, 79) ~ "65-79 år",
                                aldernum > 79 ~ "80+ år", 
                                TRUE ~ "totalt"),
                  start_ar = as.character(as.numeric(prognos_ar) - 1),
                  slut_ar = as.character(as.numeric(start_ar) + jmfrtid),
           ar_beskr = paste0("Förändring ", start_ar, "-", slut_ar, " (prognos våren ", 
                              prognos_ar, ")"),
           Folkmängd = round(Folkmängd))
  
  # om vi inte vill ha könsuppdelad data (finns inte stöd för det än) så tar vi bort det här
  if (!konsuppdelat) {
    progn_bef <- progn_bef |> 
      dplyr::group_by(dplyr::across(-c(kön, Folkmängd))) |> 
      dplyr::summarise(Folkmängd = sum(Folkmängd, na.rm = TRUE)) |>
      dplyr::ungroup()
  }
  
  # om vi vill gruppera ihop regionerna till en så gör vi det här (dvs. om gruppera_ihop = TRUE)
  if (!is.na(gruppera_namn)) {
    progn_bef <- progn_bef |> 
    dplyr::group_by(dplyr::across(c(-region, - regionkod, -Folkmängd))) |> 
    dplyr::summarise(Folkmängd = sum(Folkmängd)) |> 
    dplyr::mutate(regionkod = "grp", region = gruppera_namn) |> 
    dplyr::ungroup()
    
    eget_regionnamn <- gruppera_namn
  }
  
  # beräkna skillnad mellan startår och slutår, både som antal och som andel    
  prognos_diff_df <- progn_bef |> 
    dplyr::group_by(dplyr::across(-c(ålder, aldernum, Folkmängd))) |> 
    dplyr::summarise(Folkmängd = sum(Folkmängd, na.rm = TRUE)) |>
    dplyr::ungroup() |> 
    tidyr::pivot_wider(names_from = år, values_from = Folkmängd)
  
  # Kontrollera att de förväntade start-/slutårskolumnerna faktiskt finns i datan.
  # (pxweb2-uttaget returnerar inte alltid exakt lika många år som "+jmfrtid" tidigare gav.)
  tillgangliga_ar <- names(prognos_diff_df) |> purrr::keep(~ stringr::str_detect(.x, "^[0-9]{4}$"))
  saknas_slut <- setdiff(slutar, tillgangliga_ar)
  saknas_start <- setdiff(startar, tillgangliga_ar)
  
  if (length(saknas_slut) > 0 || length(saknas_start) > 0) {
    senaste <- max(as.numeric(tillgangliga_ar))
    aldst <- min(as.numeric(tillgangliga_ar))
    warning(paste0(
      "Förväntade årskolumner saknas i hämtad prognosdata: ",
      paste(c(saknas_start, saknas_slut), collapse = ", "),
      ". Tillgängliga år är ", aldst, "-", senaste,
      ". Kontrollera tid_vekt/jmfrtid-hanteringen i hamta_befprognos_scb_data.R (pxweb2)."
    ))
    slutar <- as.character(senaste)
    if (length(saknas_start) > 0) startar <- as.character(aldst)
  }
  
  prognos_diff_df <- dplyr::mutate(prognos_diff_df,
                                   antal = rowSums(dplyr::select(prognos_diff_df, dplyr::all_of(slutar)), na.rm = TRUE) -
                                     rowSums(dplyr::select(prognos_diff_df, dplyr::all_of(startar)), na.rm = TRUE),
                                   andel = round((rowSums(dplyr::select(prognos_diff_df, dplyr::all_of(slutar)), na.rm = TRUE) -
                                                    rowSums(dplyr::select(prognos_diff_df, dplyr::all_of(startar)), na.rm = TRUE)) /
                                                   rowSums(dplyr::select(prognos_diff_df, dplyr::all_of(startar)), na.rm = TRUE) * 100, 1),
                                   aldergrp = factor(aldergrp, levels = c("totalt", "0-19 år", "20-64 år", "65-79 år", "80+ år"))
  )
  # ---- HÄR SLUTAR ERSÄTTNINGEN ----
  
  return(prognos_diff_df)
}
 