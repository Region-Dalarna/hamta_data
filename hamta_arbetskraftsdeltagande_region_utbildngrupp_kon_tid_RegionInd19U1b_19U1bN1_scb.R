hamta_arbetskraftsdeltagande_region_utbildngrupp_kon_tid_scb <- function(
    region_vekt = "20",			# Val av region. Finns: "00", "FA00"-"FA60", samtliga län
    utbildngrupp_klartext = "*",			 #  NA = tas inte med i uttaget, "*" = alla utbildningsgrupper
    kon_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "män", "kvinnor", "totalt"
    cont_klartext = "*",			 #  Finns: "I arbetskraften", "Inte i arbetskraften", "Totalt antal personer"
    tid_koder = "*",			 # "*" = alla år, "9999" = senaste, finns 2006-2024 (se kommentar nedan)
    long_format = TRUE,			# TRUE = konvertera innehållsvariablerna i datasetet till long-format
    wide_om_en_contvar = TRUE,			# TRUE = behåll wide-format om bara en innehållsvariabel faktiskt hämtades, även om long_format = TRUE
    output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
    excel_filnamn = "arbetskraftsdeltagande.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
    returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
){

  # ====================================================================================================
  #
  # Funktion för att hämta data om arbetskraftsdeltagande från SCB:s API.
  #
  # Skapad av: frkjon den 03 oktober 2024
  # Migrerad till pxweb2r/rdverktyg (bort med p_load(pxweb)/source() mot func_API.R): september 2026.
  #
  # v1-tabellerna denna funktion tidigare kombinerade var:
  #   AM/AM9906/AM9906O/RegionInd19U1b   (äldre år, "O" = "äldre tabeller som inte uppdateras")
  #   AM/AM9906/AM9906B/RegionInd19U1bN1 (2019- och framåt)
  # Den första av dessa (RegionInd19U1b) svarar numera med "Bad Request" (HTTP 400) - v1-url:en är
  # borttagen. Rättelse (upptäckt av användaren 2026-09-14, den första migreringen av den här filen
  # missade detta): datan är INTE borta - den finns kvar som en egen v2-tabell, TAB5433 ("Uppdateras
  # ej. År 2006-2018"), separat från TAB6368 (2019-2024, v2-motsvarigheten till RegionInd19U1bN1).
  # Verifierat att de två tabellerna går att slå ihop rakt av: identiska etiketter för Region/
  # Utbildngrupp/Kon (även samma utbildningskod "00N" för "samtliga utbildningsnivåer"), och
  # ContentsCode har samma tre klartextetiketter i båda även om de underliggande SCB-koderna skiljer
  # (000003NG/NI/NJ i TAB5433 mot 000007ID/IE/IF i TAB6368) - spelar ingen roll här eftersom vi alltid
  # frågar/får klartext, aldrig kod, för ContentsCode. Ingen årsöverlappning (2018/2019-gränsen är
  # skarp) så ingen risk för dubbelräkning likt pendlingstabellernas 2020/2021-överlapp.
  # pxweb2r::pxweb2_get_data() stödjer att skicka med en vektor av tabell-id:n direkt (slås ihop
  # automatiskt, med en extra table_id-kolumn som tas bort igen nedan för att inte ändra det
  # returnerade formatet).
  #
  # ====================================================================================================

  if (!requireNamespace("rdverktyg", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
  }
  if (!requireNamespace("pxweb2r", quietly = TRUE)) remotes::install_github("FaluPeppe/pxweb2r")
  if (!requireNamespace("writexl", quietly = TRUE)) install.packages("writexl")
  # dplyr/purrr/stringr/tidyr följer med som beroenden till rdverktyg.

  scb_tabell_id <- c("TAB5433", "TAB6368")

  # pxweb2_get_values() stödjer bara en tabell åt gången (till skillnad från pxweb2_get_data()) -
  # giltiga år hämtas därför separat per tabell och slås ihop här.
  giltiga_ar <- unique(unlist(purrr::map(scb_tabell_id, ~ pxweb2r::pxweb2_get_values(.x, "Tid", quiet = TRUE)$code)))
  if (all(tid_koder != "*")) {
    tid_sokt <- stringr::str_replace(as.character(tid_koder), "9999", max(giltiga_ar))
    tid_vekt <- unique(tid_sokt[tid_sokt %in% giltiga_ar])
  } else tid_vekt <- giltiga_ar

  # Kön/utbildningsgrupp/innehåll skickas som klartext (eller "*") rakt in i frågan - pxweb2r slår
  # själv upp rätt kod respektive hanterar wildcard. NA betyder (som i v1-versionen) att variabeln
  # utelämnas helt ur frågan, vilket ger en (av SCB) redan summerad totalrad i stället för uppdelning.
  utbildngrupp_vekt <- if (all(is.na(utbildngrupp_klartext))) NULL else utbildngrupp_klartext
  kon_vekt <- if (all(is.na(kon_klartext))) NULL else kon_klartext

  query_list <- purrr::compact(list(
    Region = region_vekt,
    Utbildngrupp = utbildngrupp_vekt,
    Kon = kon_vekt,
    ContentsCode = cont_klartext,
    Tid = tid_vekt
  ))

  px_df <- pxweb2r::pxweb2_get_data(table = scb_tabell_id, query = query_list, quiet = TRUE) |>
    dplyr::select(-dplyr::any_of("table_id")) |>          # tillagd av pxweb2r vid flertabellshämtning, inte del av det tidigare returformatet
    dplyr::rename(regionkod = region_kod) |>
    dplyr::rename(dplyr::any_of(c(utbildngruppkod = "utbildning_kod"))) |>
    dplyr::relocate(regionkod, .before = region) |>
    dplyr::relocate(dplyr::any_of("utbildngruppkod"), .before = dplyr::any_of("utbildning"))

  antal_contvar <- length(unique(px_df$tabellinnehåll))

  # man kan välja bort long-format, då låter vi kolumnerna vara wide om det finns fler innehållsvariabler, annars
  # pivoterar vi om till long-format, dock ej om det bara finns en innehållsvariabel (eller
  # wide_om_en_contvar = TRUE och exakt en innehållsvariabel faktiskt hämtades)
  if (!long_format || (wide_om_en_contvar && antal_contvar == 1)) {
    px_df <- tidyr::pivot_wider(px_df, names_from = tabellinnehåll, values_from = value)
  } else {
    px_df <- dplyr::rename(px_df, variabel = tabellinnehåll, varde = value)
  }

  # Om användaren vill spara data till en Excel-fil
  if (!is.na(output_mapp) & !is.na(excel_filnamn)) {
    writexl::write_xlsx(px_df, paste0(output_mapp, excel_filnamn))
  }

  # Returnera data som en dataframe om användern valt det
  if (returnera_df) return(px_df)

}
