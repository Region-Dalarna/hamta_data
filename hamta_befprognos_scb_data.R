
hamta_befprognos_data <- function(
    region_vekt = "20",
    alder_list = "*",
    kon_klartext = c("kvinnor", "män"),     # "män", "kvinnor"
    cont_klartext = "Folkmängd",       # "Folkmängd", "Födda", "Döda", "Inrikes inflyttning", "Inrikes utflyttning", "Invandring", "Utvandring"
    tid_vekt = "*",                    # kan vara enskilda år, om "+" eller "-" skickas med så tas prognosåret + eller - antalet år som skickas med,
    # ex. "+0" så tas själva prognosåret med. Om man vill ha pronosåret och ytterligare 10 år efter det så lägger man med: paste0("+", c(0:10))
    url_prognos_vektor = "G:/Samhällsanalys/Statistik/Befolkningsprognoser/Profet/datafiler/",

    # profet: "G:/Samhällsanalys/Statistik/Befolkningsprognoser/Profet/datafiler/"
    #  c("https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401A/BefProgOsiktRegN",
    #    "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401B/BefProgOsiktRegN21",
    #    "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401B/BefProgOsiktRegN20"),

    prognos_ar = "9999",               # NA = alla år, eller enskilda år, "9999" = senaste år
    long_format = FALSE
) {

  # ===========================================================================================================
  #
  # Skript för att hämta data från SCB för befolkningsprognoser alternativt från körningar som man gjort i
  # Profet. För Profet-filer så anges en mapp där Profetfilerna sparas. Samtliga filer i mappen läses in
  # och vilka prognosår (om det finns flera) som används styrs med parametern prognos_ar. För SCB:s API:er så
  # är däremot en url ett prognosår så vill man jämföra flera prognoser med varandra behöver man skicka med
  # en url per prognosår.
  #
  # Parametrar som skickas med (= variabler i SCB-tabellen) är:
  # - Innehåll                                                    # Folkmängd (standard) därutöver: "Födda", "Döda", "Inrikes inflyttning", "Inrikes utflyttning", "Invandring", "Utvandring"
  # - Region                                                      # tabellen innehåller kommuner och län, för Profet så är regioner begränsat till vad man har tillgång till där (OBS! För närvarande görs ingen kontroll av regioner i Profetfilerna, bör nog läggas till)
  # - Kön                                                         # finns enbart kvinnor och män (inte totalt)
  # - Ålder                                                       # * = alla åldrar (standard), finns i ettårsgrupper tom 100+ år (en utveckling att kunna skicka med NA och kunna returnera totalt och inte åldersuppdelat)
  # - url_prognos_vektor                                          # url:er för att hämta data, från SCB via API:er eller till en mapp där det finns Profetfiler
  # - prognos_ar                                                  # "9999" = senaste tillgängliga år, gäller endaste för Profetfiler, för SCB:s API:er så styrs det av den url man skickar med ovan
  # - tid_vekt (dvs. år)                                          # * = alla år (standard). Om man bara vill jämföra tex 10 år framåt så blir det billigare att bara hämta det året och inte alla däremellan
  # - long_format                                                 # TRUE = data returneras i longformat, FALSE = i wideformat (standard)
  #
  # Skapat av: Peter Möller, Region Dalarna
  #            november 2023
  # Senast uppdaterat:  september 2026
  #                     Migrerad till pxweb2r/rdverktyg för SCB-grenen (bort med p_load(pxweb)/source()
  #                     mot func_API.R). SCB:s v1-tabell BE0401A/BefProgOsiktRegN motsvaras i v2 av
  #                     TAB698 (verifierad identisk variabelstruktur: Region/Kon/Alder/ContentsCode med
  #                     samma sju innehållskoder/Tid 2024-2070). Profet-filgrenarna (xlsx/csv) är rena
  #                     lokala filläsningar utan SCB-beroende och är oförändrade i sak, bara namespace-
  #                     satta - utom long-format-konverteringen, som tidigare (i onödan) frågade SCB:s
  #                     API för att ta reda på innehållsvariablernas namn även för lokala filer; det görs
  #                     nu direkt mot den redan kända lokala variabellistan (contvar_vekt) i stället.
  #                     (tidigare: juni 2025, justerat så att filer från Hallands befolkningsprognosskript
  #                     läses in)
  #
  # ===========================================================================================================

  # Bara paket, ingen source() mot funktioner-repot och inget p_load(pxweb/tidyverse). Anropas med fullt
  # namespace (dplyr::/stringr::/purrr::/pxweb2r:: osv.) i stället.
  if (!requireNamespace("rdverktyg", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
  }
  if (!requireNamespace("pxweb2r", quietly = TRUE)) remotes::install_github("FaluPeppe/pxweb2r")
  if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
  if (!requireNamespace("data.table", quietly = TRUE)) install.packages("data.table")
  # dplyr/purrr/stringr/tidyr följer med som beroenden till rdverktyg.
  options(scipen = 999)

  # v2-motsvarigheten till BE0401A/BefProgOsiktRegN (SCB:s standardtabell för befolkningsprognos per
  # region/kön/ålder/innehåll). Om url_prognos_vektor pekar mot en annan SCB-url än den nedan (t.ex. en
  # av de utkommenterade alternativa scenariotabellerna ovan, vilka aldrig faktiskt använts någonstans i
  # diagram-repot) hanteras det inte här - den enda SCB-url som i praktiken förekommer är url_scbtabell.
  url_scbtabell <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401A/BefProgOsiktRegN"
  scb_tabell_id <- "TAB698"

  before_n <- length(url_prognos_vektor)

  # byt ut endast om det inte är https:// OCH inte en befintlig mapp
  to_replace <- !stringr::str_detect(url_prognos_vektor, "^https://") & !dir.exists(url_prognos_vektor)

  url_prognos_vektor[to_replace] <- url_scbtabell
  url_prognos_vektor <- unique(url_prognos_vektor)

  if (any(to_replace)) cat("Ersatte", sum(to_replace), "värde(n) med SCB-url.\n")
  if (length(url_prognos_vektor) < before_n) cat("Tog bort", before_n - length(url_prognos_vektor), "dubblett(er).\n")

  hamta_url_rad_i_vektor <- function(hamta_url) {

    # ================================== beräkna rätt tid_vekt ==============================================

    if (any(tid_vekt != "*")) {               # om inte "*" är valt
      jmfr_vekt <- as.numeric(tid_vekt[stringr::str_detect(tid_vekt, "\\+|\\-")])              # alla år som ska beräknas utifrån prognosår
      andra_ar_vekt <- as.numeric(tid_vekt[!stringr::str_detect(tid_vekt, "\\+|\\-")])         # övriga år = år som skickas med som de är, tex. "2015"

      if (stringr::str_detect(hamta_url, "https://api.scb.se")) {

        valt_ar <- min(as.numeric(pxweb2r::pxweb2_get_values(scb_tabell_id, "Tid")$code))
        if ("9999" %in% prognos_ar) prognos_ar <- as.numeric(stringr::str_replace(prognos_ar[prognos_ar == "9999"], "9999", as.character(valt_ar)))

      } else {

        filsokvagar <- list.files(hamta_url, pattern = ".csv|.xlsx", full.names = TRUE)
        filsokvagar_csv <- filsokvagar[stringr::str_detect(filsokvagar, ".csv")]
        filsokvagar_xlsx <- filsokvagar[stringr::str_detect(filsokvagar, ".xlsx")]

        # behåll de som finns i region_vekt i filskokvagar_csv (dvs. från Profet)
        if (!"20" %in% region_vekt) filsokvagar_csv <- filsokvagar_csv[!stringr::str_detect(filsokvagar_csv, "lan")]                        # ta bort länsfiler ur sokvagsvektorn om inte "lan" är med i sökvägen
        if (!any(rdverktyg::hamtakommuner(lan = "20", FALSE, FALSE, FALSE) %in% region_vekt)) filsokvagar_csv <- filsokvagar_csv[!stringr::str_detect(filsokvagar_csv, "kommun")]      # ta bort kommunfiler ur sokvagsvektorn om ingen av Dalarnas kommuners kommunkoder är med

        # och så sätter vi ihop csv- och xlsx-sökvägar igen
        filsokvagar <- c(filsokvagar_csv, filsokvagar_xlsx)

        # kontrollera vilka prognosår som finns bland profet-filerna i mappen
        existerande_ar <- unique(purrr::map_int(filsokvagar, ~ readr::parse_number(.)))
        if ("9999" %in% prognos_ar) prognos_ar <- as.numeric(stringr::str_replace(prognos_ar, "9999", as.character(max(existerande_ar))))
        prognos_ar <- prognos_ar[prognos_ar %in% existerande_ar]
      }

      # Bugfix (confirmed genom test mot SCB:s API, se commit e35978c): "-1" gjorde att t.ex. tid_vekt =
      # "+0" ("själva prognosåret", enligt parameterdokumentationen ovan) pekade på året FÖRE tabellens
      # första giltiga år och kraschade ("Assertion on 'Tid' failed"), och "+1" gav tyst prognosårets EGET
      # första år i stället för året efter. start_ar sätts nu till prognos_ar rakt av.
      start_ar <- prognos_ar
      jmfr_ar <- start_ar + jmfr_vekt
      hamta_tid_vekt <- if (length(jmfr_ar) > 0) c(jmfr_ar, andra_ar_vekt) else andra_ar_vekt

    } else hamta_tid_vekt <- tid_vekt                   # om tid_Vekt == "*" så blir hamta_tid_vekt det också



    hamta_data_fran_tabell <- function(url_prognos) {

      if (stringr::str_detect(url_prognos, "https://api.scb.se")) {

        valt_ar <- min(as.numeric(pxweb2r::pxweb2_get_values(scb_tabell_id, "Tid")$code))

        if (all(cont_klartext == "*")) cont_klartext <- pxweb2r::pxweb2_get_values(scb_tabell_id, "ContentsCode")$label

        # Ålder = "0" om bara Födda efterfrågas (Födda finns bara registrerat på nyfödda, dvs. ålder 0 -
        # samma specialfall som i v1-versionen), annars den medskickade åldersvektorn rakt av, eller
        # (om alder_list = "*", standardvärdet) utelämnad helt ur query_list nedan - att utelämna en
        # variabel i pxweb2r ger alla individuella värden (ingen färdigsummerad total), vilket motsvarar
        # v1:s Alder = "*".
        alder_vekt <- if (all(cont_klartext == "Födda")) "0" else if (!identical(alder_list, "*")) unlist(alder_list) else NULL

        # Kön/innehåll skickas som klartext rakt in i frågan - pxweb2r slår själv upp rätt kod.
        query_list <- purrr::compact(list(Region = region_vekt,
                           Alder = alder_vekt,
                           Kon = kon_klartext,
                           ContentsCode = cont_klartext,
                           Tid = as.character(hamta_tid_vekt)))

        px_df <- pxweb2r::pxweb2_get_data(table = scb_tabell_id, query = query_list, on_all_values_invalid = "null") |>
          dplyr::rename(regionkod = region_kod) |>
          dplyr::mutate(prognos_ar = as.character(valt_ar))

        # man kan välja bort long-format, då låter vi kolumnerna vara wide om det finns fler innehållsvariabler, annars
        # pivoterar vi om till long-format, dock ej om det bara finns en innehållsvariabel
        if (long_format) {
          px_df <- dplyr::rename(px_df, variabel = tabellinnehåll, varde = value)
        } else {
          px_df <- tidyr::pivot_wider(px_df, names_from = tabellinnehåll, values_from = value)
        } # slut if-sats som kontrollera om vi vill ha df i long-format
        retur_df <- px_df

      } else {                    # om vi inte har en url som innehåller en adress till SCB:s API så utgår vi från att det är en profet-fil

        filsokvagar <- list.files(hamta_url, pattern = ".csv|.xlsx", full.names = TRUE)

        alla_prognos_ar <- unique(purrr::map_chr(filsokvagar, ~ as.character(readr::parse_number(.))))
        sok_prognos_ar <- paste0(prognos_ar, collapse = "|")               # för att kunna använda nedan i str_detect
        # ta ut rätt år utifrån användarens val
        if (all(prognos_ar == "9999")) filsokvagar <- filsokvagar[stringr::str_detect(filsokvagar, max(alla_prognos_ar))]
        if (all(!is.na(prognos_ar) & prognos_ar != "9999")) filsokvagar <- filsokvagar[stringr::str_detect(filsokvagar, sok_prognos_ar)]
        if (any(stringr::str_detect(prognos_ar, "9999"))) prognos_ar <- stringr::str_replace(alla_prognos_ar, "9999", max(alla_prognos_ar))

        retur_profet <- NULL
        befskript_df <- NULL
        filsokvagar_xlsx <- filsokvagar[stringr::str_detect(filsokvagar, ".xlsx")]
        filsokvagar_csv <- filsokvagar[stringr::str_detect(filsokvagar, ".csv")]
        # här läser vi in filer som är skapade med Hallands skript
        if (length(filsokvagar_xlsx) > 0) {

          # kontrollera vilka prognosår som finns bland profet-filerna i mappen som ska användas
          progn_ar <- purrr::map_chr(filsokvagar_xlsx, ~ as.character(readr::parse_number(.)))

          # vektor för att döpa om kolumner i profetfilen så att de blir samma som i pxwebs befolkningsprognostabeller
          rename_befskript <- c("regionkod" = "lan_kod", "regionkod" = "kommun_kod", "ålder" = "alder", "kön" = "kon", "år" = "ar",
                                "Folkmängd" = "total_folkmangd", "Födda" = "fodda", "Döda" = "doda", "Inrikes inflyttning" = "inrikes_inflyttning",
                                "Inrikes utflyttning" = "inrikes_utflyttning", "Invandring" = "invandring", "Utvandring" = "utvandring")

          contvar_vekt <- c("Folkmängd", "Födda", "Döda", "Inrikes inflyttning", "Inrikes utflyttning", "Invandring", "Utvandring")
          if (all(cont_klartext == "*")) cont_klartext <- contvar_vekt

          # Felhantering (för att slippa en kryptisk krasch längre ned, tex. att en efterfrågad kolumn
          # helt saknas i den slutliga datan): de lokala Profet-filerna (Hallands skript-varianten) har
          # bara de sju innehållsvariablerna i contvar_vekt ovan - saknar tex. helt uppdelning på
          # inrikes/utrikes födda. Stoppa här med ett begripligt felmeddelande om något annat efterfrågas,
          # i stället för att fortsätta med tom/felaktig data.
          saknade_contvar <- setdiff(cont_klartext, contvar_vekt)
          if (length(saknade_contvar) > 0) {
            stop("hamta_befprognos_data(): cont_klartext innehåller variabler som ännu inte finns i de ",
                 "lokala Profet-filerna (", hamta_url, "): ", paste(saknade_contvar, collapse = ", "),
                 ". Tillgängliga variabler i filerna är: ", paste(contvar_vekt, collapse = ", "),
                 ". Hämta i stället direkt från SCB:s API (skicka en https://-url i url_prognos_vektor) ",
                 "om du behöver denna variabel.")
          }

          befskript_df <- purrr::list_rbind(purrr::map2(filsokvagar_xlsx, progn_ar, ~ dplyr::mutate(readxl::read_xlsx(.x),
                                 prognos_ar = as.character(.y),
                                 ar = as.character(ar))))

          if (all(hamta_tid_vekt != "*")) befskript_df <- dplyr::filter(befskript_df, ar %in% hamta_tid_vekt)            # ta bara ut jämförelseåret

          # Bugfix (confirmed genom kodgranskning): "!=" mellan två olika långa vektorer recyclar
          # element-för-element i stället för att jämföra mängder (fungerade "av misstag" hittills bara för att
          # cont_klartext i praktiken alltid varit hela contvar_vekt) - rätt jämförelse är %in%.
          if (all(cont_klartext != "*")) tabort_contvar <- contvar_vekt[!contvar_vekt %in% cont_klartext]

          befskript_df <- dplyr::rename(befskript_df, dplyr::any_of(rename_befskript))

          # Bugfix (confirmed genom test): case_when() kräver numera (dplyr 1.1+) att alla grenars
          # högersida har samma typ - "män"/"kvinnor" är text men TRUE ~ kön (fallback-grenen) var
          # fortfarande numerisk, vilket kraschar med "Can't combine <character> and <double>" så fort
          # grenen faktiskt körs mot en riktig xlsx-fil (otestad mot riktiga filer sedan tidigare).
          befskript_df <- dplyr::mutate(befskript_df,
                           kön = dplyr::case_when(kön == 1 ~ "män",
                                          kön == 2 ~ "kvinnor",
                                          TRUE ~ as.character(kön)),
                           ålder = ifelse(ålder > 99, "100+ år", paste0(ålder, " år")),
                           år = as.character(år))
          data.table::setDT(befskript_df)
          befskript_df <- befskript_df[, lapply(.SD, sum, na.rm = TRUE), by = setdiff(names(befskript_df), names(befskript_df)[sapply(befskript_df, is.numeric)])]

          # ta bort variabler som användaren inte valt
          befskript_df <- dplyr::select(befskript_df, -dplyr::all_of(tabort_contvar))

          if (all(cont_klartext == "Födda")) befskript_df <- dplyr::select(dplyr::filter(befskript_df, ålder == "0 år"), -ålder)

          # man kan välja bort long-format, då låter vi kolumnerna vara wide om det finns fler innehållsvariabler, annars
          # pivoterar vi om till long-format, dock ej om det bara finns en innehållsvariabel. Görs direkt mot den kända
          # lokala variabellistan (contvar_vekt) - ingen anledning att fråga SCB:s API om något som redan är lokal data.
          if (long_format) {
            befskript_df <- tidyr::pivot_longer(befskript_df, cols = dplyr::any_of(contvar_vekt), names_to = "variabel", values_to = "varde")
          } # slut if-sats som kontrollera om vi vill ha df i long-format


        } # slut test om det finns .xlsx-filer (= filer från Hallands skript)


        if (length(filsokvagar_csv) > 0) {
          # hämta regionnyckel
          regionnyckel <- rdverktyg::hamtaregtab()

          # behåll de som finns i region_vekt
          if (!"20" %in% region_vekt) filsokvagar_csv <- filsokvagar_csv[!stringr::str_detect(filsokvagar_csv, "lan")]                        # ta bort länsfiler ur sokvagsvektorn om inte "20" är med som regionkod
          if (!any(rdverktyg::hamtakommuner(lan = "20", FALSE, FALSE, FALSE) %in% region_vekt)) filsokvagar_csv <- filsokvagar_csv[!stringr::str_detect(filsokvagar_csv, "kommun")]      # ta bort kommunfiler ur sokvagsvektorn om ingen av Dalarnas kommuners kommunkoder är med

          # kontrollera vilka prognosår som finns bland profet-filerna i mappen som ska användas
          progn_ar <- purrr::map_chr(filsokvagar_csv, ~ as.character(readr::parse_number(.)))

          # vektor för att döpa om kolumner i profetfilen så att de blir samma som i pxwebs befolkningsprognostabeller
          rename_profet <- c("regionkod" = "lan_kod", "regionkod" = "kommun_kod", "regionkod", "region", "ålder" = "age", "kön" = "kon", "år" = "year",
                             "Folkmängd" = "pop", "Födda" = "fodda", "Döda" = "doda", "Inrikes inflyttning" = "inrikes_inflyttade",
                             "Inrikes utflyttning" = "inrikes_utflyttade", "Invandring" = "immigranter", "Utvandring" = "emigranter")

          contvar_vekt <- c("Folkmängd", "Födda", "Döda", "Inrikes inflyttning", "Inrikes utflyttning", "Invandring", "Utvandring")
          if (all(cont_klartext == "*")) cont_klartext <- contvar_vekt

          # Felhantering: se motsvarande kommentar i xlsx-grenen ovan.
          saknade_contvar <- setdiff(cont_klartext, contvar_vekt)
          if (length(saknade_contvar) > 0) {
            stop("hamta_befprognos_data(): cont_klartext innehåller variabler som ännu inte finns i de ",
                 "lokala Profet-filerna (", hamta_url, "): ", paste(saknade_contvar, collapse = ", "),
                 ". Tillgängliga variabler i filerna är: ", paste(contvar_vekt, collapse = ", "),
                 ". Hämta i stället direkt från SCB:s API (skicka en https://-url i url_prognos_vektor) ",
                 "om du behöver denna variabel.")
          }

          las_in_profet_fil <- function(profetfil_sokvag, fil_prognosar) {

            if (any(hamta_tid_vekt != "*")) {
              # Samma "-1"-bugg som i SCB-grenen ovan (samma kommentar fanns här: "ta bort -1 igen?") -
              # rättad på samma sätt, i konsekvens med SCB-fixet. OBS: den här grenen (Profet-filer) är
              # inte testad mot riktiga filer i samband med fixet, till skillnad från SCB-grenen.
              fil_start_ar <- as.numeric(fil_prognosar)
              fil_jmfr_ar <- fil_start_ar + jmfr_vekt
              fil_hamta_tid_vekt <- if (length(fil_jmfr_ar) > 0) c(fil_jmfr_ar, andra_ar_vekt) else andra_ar_vekt
            } else fil_hamta_tid_vekt <- "*"

            profet_df <- readr::read_csv(profetfil_sokvag, show_col_types = FALSE)
            kolnamn <- names(profet_df)                                                        # hämta kolumnnamn för att kunna avgöra om det är kommuner eller län

            # kolla om det är kommunfil, i så fall lägger vi ihop in- och utflyttning inom län och från utanför län
            if (all(c("utomin", "inomin", "utomut", "inomut") %in% kolnamn)) {
              profet_df <- dplyr::mutate(profet_df,
                       inrikes_inflyttade = inomin + utomin,
                       inrikes_utflyttade = inomut + utomut)
              profet_df <- dplyr::select(profet_df, -c(inomin, utomin, inomut, utomut))
            }

            if (all(fil_hamta_tid_vekt == "*")) fil_hamta_tid_vekt <- (as.numeric(fil_prognosar)):2100

            profet_df <- dplyr::rename(profet_df, dplyr::any_of(rename_profet))                                                    # döp om kolumner så de heter samma som i pxweb-befolkningsprognoserna

            regionkod_len <- if (any(stringr::str_detect(kolnamn, "lan_kod"))) 2 else 4                 # kolla om det är län eller kommuner, väljer längd på regionkoden utifrån vad vi har i datasetet

            profet_df <- dplyr::mutate(profet_df,
                     regionkod = stringr::str_sub(regionkod, 1, regionkod_len),
                     kön = ifelse(kön == 1, "män", "kvinnor"),
                     ålder = ifelse(ålder == 100, paste0(ålder, "+ år"), paste0(ålder, " år")),
                     år = as.character(år),
                     prognos_ar = fil_prognosar)
            profet_df <- dplyr::filter(profet_df, år %in% as.character(fil_hamta_tid_vekt))
            profet_df <- dplyr::relocate(dplyr::left_join(profet_df, regionnyckel, by = "regionkod"), region, .after = regionkod)

            # Bugfix (confirmed genom kodgranskning): se motsvarande kommentar i xlsx-grenen ovan.
            if (all(cont_klartext != "*")) tabort_contvar <- contvar_vekt[!contvar_vekt %in% cont_klartext]                      # ta bort de innehållsvariabler som användaren inte valt ur vektorn, använd den för att ta bort variabler

            # ta bort variabler som användaren inte valt
            profet_df <- dplyr::select(profet_df, -dplyr::all_of(tabort_contvar))

            if (all(cont_klartext == "Födda")) profet_df <- dplyr::select(dplyr::filter(profet_df, ålder == "0 år"), -ålder)

            # man kan välja bort long-format, då låter vi kolumnerna vara wide om det finns fler innehållsvariabler, annars
            # pivoterar vi om till long-format, dock ej om det bara finns en innehållsvariabel. Görs direkt mot den kända
            # lokala variabellistan (contvar_vekt) - ingen anledning att fråga SCB:s API om något som redan är lokal data.
            if (long_format) {
              profet_df <- tidyr::pivot_longer(profet_df, cols = dplyr::any_of(contvar_vekt), names_to = "variabel", values_to = "varde")
            } # slut if-sats som kontrollera om vi vill ha df i long-format
            return(profet_df)
          } # slut funktion för att läsa in profetfiler

          retur_profet <- purrr::list_rbind(purrr::map2(filsokvagar_csv, progn_ar, ~ las_in_profet_fil(.x, .y)))
        } # slut test om det finns csv-filer att läsa in (= filer från Profet)

        retur_filer <- dplyr::bind_rows(retur_profet, befskript_df)

        return(retur_filer)

    } # if-sats, else-delen som är om det är en sökväg till en profet-fil
  } # funktion att hämta data från url:er (scb-api:er eller profet-filer)

    # hamta_url är redan en enskild url/sökväg här (funktionen anropas en gång per element i
    # url_prognos_vektor från map_dfr() längst ner) - ingen map_dfr behövs för att hämta den.
    retur_df <- dplyr::filter(hamta_data_fran_tabell(url_prognos = hamta_url), regionkod %in% region_vekt)

    return(retur_df)

} # slut funktion för varje url-rad i url-vektorn

  purrr::list_rbind(purrr::map(url_prognos_vektor, ~ hamta_url_rad_i_vektor(hamta_url = .x)))

} # slut funktion
