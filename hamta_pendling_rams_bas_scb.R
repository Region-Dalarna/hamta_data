

hamta_pendling_rams_bas_scb <- function(region_vekt = "20",
                                    kon_klartext_vekt = "män och kvinnor",      # finns: "män", "kvinnor", "män och kvinnor"
                                    tid_vekt = "*"                              # "9999" = senaste år
                                    ) {

  # ===========================================================================================================
  #
  # Skript för att hämta pendlingsdata från RAMS och BAS, SCB. Årsvis. Skriptet hämtar data från fyra olika tabeller,
  # pendling år 1993-2003, 2004-2018 samt 2019-2021 (RAMS) samt 2020- (BAS). Man får vara lite försiktig i sina
  # analyser då det kan skilja sig något i definitioner, metod etc. Det kan vara en bra idé att i visualiseringen
  # tydliggöra att data kommer från olika tabeller, med ex. olika färger.
  #
  # Parametrar som skickas med (= variabler i SCB-tabellen) är:
  # - Region                                                      # tabellen innehåller bara kommuner och riket men länssiffror kan beräknas genom aggregering
  # - Kön                                                         # det funkar dock inte för andel av befolkningen 20-64 år, då skickas bara NA-värden med
  # - tid (dvs. år)                                               #
  #
  # Innehåll skickas inte med, då det bara är ett val.
  #
  # Skapat av Peter Möller i november 2023.
  # Senast ändrad: 21 dec 2023
  # Migrerad till pxweb2r/rdverktyg (bort med source() mot funktioner-repot och p_load/pxweb): 2026-09-14.
  # Fyra v1-tabeller (samtliga fortfarande kvar i SCB:s v1-API) motsvaras nu av fyra v2-tabeller - samma
  # mönster/tabellpar som redan hittats och verifierats i diag_rams_bas_pendling_pendlingsrelationer_over_tid.R
  # i diagram-repot (den här funktionens enda övriga användare, förutom karta_pendling_leaflet.R):
  #   AM0207/AM0207Z/AM0207PendlKomA04N (RAMS, ny tidsserie 2019-2021) -> TAB5850
  #   AM0207/AM0207L/AM0207PendlKomA04  (RAMS 2004-2018)               -> TAB333
  #   AM0207/AM0207L/AM0207PendlKomA9303 (RAMS 1993-2003)              -> TAB334
  #   AM0210/AM0210F/ArRegPend2 (BAS 2020-)                            -> TAB1830
  #
  # ===========================================================================================================

  # Bara paket, ingen source() mot funktioner-repot och inget p_load(tidyverse)/library(pxweb). Anropas
  # med fullt namespace (dplyr::filter() osv.) i stället.
  if (!requireNamespace("rdverktyg", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
  }
  if (!requireNamespace("pxweb2r", quietly = TRUE)) remotes::install_github("FaluPeppe/pxweb2r")
  # dplyr/purrr/stringr följer med som beroenden till rdverktyg.

  options(dplyr.summarise.inform = FALSE)

  regionnyckel <- rdverktyg::hamtaregtab()

  kommun_vekt <- region_vekt[nchar(region_vekt) == 4]
  lan_vekt <- region_vekt[nchar(region_vekt) == 2]
  lan_kommuner_vekt <- rdverktyg::hamtakommuner(lan_vekt, tamedlan = FALSE, tamedriket = FALSE)

  hamta_region_vekt <- c(kommun_vekt, lan_kommuner_vekt)

  tabell_id_vekt <- c("TAB5850", "TAB333", "TAB334", "TAB1830")

  # Bugfix (confirmed genom test): "9999" (senaste år) måste lösas EN gång över ALLA fyra tabeller
  # tillsammans, inte separat per tabell - annars skulle t.ex. TAB334 (1993-2003) tolka "9999" som sitt
  # eget senaste år 2003 och alltid bidra med föråldrad data, i stället för att (som här) bara bidra när
  # 2003 faktiskt är det verkliga senaste året över alla tabellerna. Samma "9999 löst per tabell i stället
  # för över alla tabeller"-buggmönster som redan hittats och rättats i andra migrerade skript. Görs på
  # samma sätt som i originalskriptet: räkna ut det gemensamma senaste året en gång, byt ut "9999" mot
  # det, och låt varje tabells egen filtrering (akt_tid_vekt <- ... %in% giltiga_ar) nedan naturligt
  # ge en tom (och därmed bortfiltrerad) träff för de tabeller som inte täcker just det året.
  if (any(tid_vekt == "9999")) {
    senaste_ar_alla_tabeller <- max(as.numeric(unlist(purrr::map(tabell_id_vekt, ~ suppressMessages(pxweb2r::pxweb2_get_values(.x, "Tid"))$code))))
    tid_vekt <- unique(stringr::str_replace(tid_vekt, "9999", as.character(senaste_ar_alla_tabeller)))
  }

  # hämtar en av de fyra pendlingstabellerna, både in- och utpendling mot hamta_region_vekt, och
  # aggregerar upp till läns- respektive kommunnivå beroende på vad som efterfrågades i region_vekt
  hamta_en_pendlingstabell <- function(tabell_id) {

    giltiga_ar <- suppressMessages(pxweb2r::pxweb2_get_values(tabell_id, "Tid"))$code
    akt_tid_vekt <- if (all(tid_vekt == "*")) giltiga_ar else tid_vekt[tid_vekt %in% giltiga_ar]
    # TAB5850 (RAMS, ny tidsserie) och TAB1830 (BAS) delar åren 2020-2021 - dessa år tas bort ur
    # TAB5850 här för att inte räknas dubbelt (samma hantering som i originalskriptet).
    if (tabell_id == "TAB5850") akt_tid_vekt <- akt_tid_vekt[!akt_tid_vekt %in% c("2020", "2021")]
    if (length(akt_tid_vekt) == 0) return(NULL)

    # "totalt" (BAS) och "män och kvinnor" (RAMS) är samma sak fast med olika klartextlabel per tabell -
    # be om båda, pxweb2r plockar bort den etikett som inte finns i just den här tabellen.
    kon_hamta <- if (any(kon_klartext_vekt %in% c("totalt", "män och kvinnor"))) {
      unique(c(kon_klartext_vekt, "totalt", "män och kvinnor"))
    } else {
      kon_klartext_vekt
    }

    hamta_riktning <- function(bostad_vekt, arbete_vekt) {
      suppressMessages(pxweb2r::pxweb2_get_data(
        table = tabell_id,
        query = list(
          Bostadskommun = bostad_vekt,
          Arbetsstallekommun = arbete_vekt,
          Kon = kon_hamta,
          ContentsCode = "*",
          Tid = akt_tid_vekt
        ),
        on_all_values_invalid = "null"
      ))
    }

    stada_riktning <- function(px) {
      if (is.null(px)) return(NULL)
      px |>
        dplyr::rename(regionkod_bo = bostadskommun_kod, bostadsregion = bostadskommun,
                       regionkod_arb = arbetsställekommun_kod, arbetsställeregion = arbetsställekommun,
                       pendlare = value) |>
        dplyr::mutate(bostadsregion = stringr::str_remove(bostadsregion, " \\(bostad\\)"),
                       arbetsställeregion = stringr::str_remove(arbetsställeregion, " \\(arbetsställe\\)")) |>
        dplyr::select(-tabellinnehåll)
    }

    px_in <- stada_riktning(hamta_riktning("*", hamta_region_vekt))
    px_ut <- stada_riktning(hamta_riktning(hamta_region_vekt, "*"))

    px_kommun_in <- if (length(kommun_vekt) > 0 && !is.null(px_in)) {
      dplyr::filter(px_in, regionkod_bo %in% kommun_vekt | regionkod_arb %in% kommun_vekt)
    } else NULL
    px_kommun_ut <- if (length(kommun_vekt) > 0 && !is.null(px_ut)) {
      dplyr::filter(px_ut, regionkod_bo %in% kommun_vekt | regionkod_arb %in% kommun_vekt)
    } else NULL

    aggregera_lan <- function(px) {
      if (is.null(px) || length(lan_vekt) == 0) return(NULL)
      px |>
        dplyr::mutate(bolan_kod = stringr::str_sub(regionkod_bo, 1, 2),
                       arblan_kod = stringr::str_sub(regionkod_arb, 1, 2)) |>
        dplyr::filter(bolan_kod %in% lan_vekt | arblan_kod %in% lan_vekt) |>
        dplyr::group_by(år, kön, regionkod_bo = bolan_kod, regionkod_arb = arblan_kod) |>
        dplyr::summarise(pendlare = sum(pendlare, na.rm = TRUE), .groups = "drop") |>
        dplyr::left_join(dplyr::rename(regionnyckel, regionkod_bo = regionkod, bostadsregion = region), by = "regionkod_bo") |>
        dplyr::left_join(dplyr::rename(regionnyckel, regionkod_arb = regionkod, arbetsställeregion = region), by = "regionkod_arb")
    }

    dplyr::bind_rows(px_kommun_in, px_kommun_ut, aggregera_lan(px_in), aggregera_lan(px_ut))
  }

  # hämta data från alla tabeller i tabell_id_vekt
  px_df <- purrr::map(tabell_id_vekt, hamta_en_pendlingstabell) |>
    purrr::list_rbind() |>
    dplyr::filter(pendlare > 0) |>
    dplyr::distinct(.keep_all = TRUE) |>
    dplyr::relocate(pendlare, .after = dplyr::last_col())

  if ("kön" %in% names(px_df)) px_df <- dplyr::mutate(px_df, kön = ifelse(kön == "totalt", "män och kvinnor", kön))

  px_df

} # slut funktion
