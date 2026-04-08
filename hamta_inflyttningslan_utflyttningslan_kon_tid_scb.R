hamta_inflyttningslan_utflyttningslan_kon_tid_scb <- function(
    inflyttningsl_klartext = "*",			 #  Finns:"Totalt, samtliga inflyttningslän" efter 2024,  " Stockholms län (Inflyttningslän)", " Uppsala län (Inflyttningslän)", " Södermanlands län (Inflyttningslän)", " Östergötlands län (Inflyttningslän)", " Jönköpings län (Inflyttningslän)", " Kronobergs län (Inflyttningslän)", " Kalmar län (Inflyttningslän)", " Gotlands län (Inflyttningslän)", " Blekinge län (Inflyttningslän)", " Skåne län (Inflyttningslän)", " Hallands län (Inflyttningslän)", " Västra Götalands län (Inflyttningslän)", " Värmlands län (Inflyttningslän)", " Örebro län (Inflyttningslän)", " Västmanlands län (Inflyttningslän)", " Dalarnas län (Inflyttningslän)", " Gävleborgs län (Inflyttningslän)", " Västernorrlands län (Inflyttningslän)", " Jämtlands län (Inflyttningslän)", " Västerbottens län (Inflyttningslän)", " Norrbottens län (Inflyttningslän)"
    utflyttningsl_klartext = "*",			 #  Finns:"Samtliga utflyttningslän" efter 2024, " Stockholms län (Utflyttningslän)", " Uppsala län (Utflyttningslän)", " Södermanlands län (Utflyttningslän)", " Östergötlands län (Utflyttningslän)", " Jönköpings län (Utflyttningslän)", " Kronobergs län (Utflyttningslän)", " Kalmar län (Utflyttningslän)", " Gotlands län (Utflyttningslän)", " Blekinge län (Utflyttningslän)", " Skåne län (Utflyttningslän)", " Hallands län (Utflyttningslän)", " Västra Götalands län (Utflyttningslän)", " Värmlands län (Utflyttningslän)", " Örebro län (Utflyttningslän)", " Västmanlands län (Utflyttningslän)", " Dalarnas län (Utflyttningslän)", " Gävleborgs län (Utflyttningslän)", " Västernorrlands län (Utflyttningslän)", " Jämtlands län (Utflyttningslän)", " Västerbottens län (Utflyttningslän)", " Norrbottens län (Utflyttningslän)"
    kon_klartext = "*",              #  NA = tas inte med i uttaget,  Finns: "män", "kvinnor" (och "totalt, samtliga män och kvinnor" efter 2024)
    cont_klartext = "*",
    tid_koder = "*",                 # "*" = alla, "9999" = senaste år. Finns från 2000. Notera att data från och med 2025 hämtas från CKM-tabell
    output_mapp = NA,               # Val av ouput-mapp för sparad data 
    excel_filnamn = "InOmflytt.xlsx",
    returnera_df = TRUE # Returnerar df till global environment
){
  
  # ====================================================================================================
  #
  # Funktion för att hämta data från SCB:s API med hjälp av pxweb-paketet
  #
  # Ändrad av Jon, 2026-04-08. Skript som ersattes finns sparat under G:\skript\jon\Backup
  # Omskriven med hjälp av Chatgp för att hantera att data ligger i två tabeller:
  # - äldre tabell
  # - 2025 och framåt i CKM-tabell
  #
  # Notera att den andra tabellen (CKM) har slumpmässig avrundning/störning för att skydda individer, så data från och med 2025 kommer inte att vara exakt.
  # Notera även att data från och med 2025 innehåller en kategori totalt, alla län, vilket saknas tidigare. Detta beror på CKM, då totalen av grupperna inte nödvändigtvis överenstämmer med totalen. Detta gäller även kategorin kön.
  # ====================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(pxweb, tidyverse, writexl)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  
  skriv_excelfil <- !is.na(output_mapp) && !is.na(excel_filnamn)
  
  url_list <- c(
    "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101J/InOmflytt",
    "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101J/InOmflyttCKM"
  )
  
  # Kontroll om CKM-tabell används
  ckm_anvands <- any(str_detect(tolower(url_list), "ckm"))
  
  if (ckm_anvands) {
    message("OBS: Data från och med 2025 hämtas från CKM-tabell där slumpmässig avrundning/störning har lagts till för att skydda individer. Se mer på SCB:s hemsida. Notera att data från och med 2025 innehåller en kategori Totalt, samtliga inflyttnings-/utflyttningslän, vilket saknas tidigare. Notera att olika val av total i anropet därför enbart finns för CKM-år (>2024)")
  }
  
  if (returnera_df || skriv_excelfil) {
    
    # Hämta alla giltiga år från båda tabellerna
    giltig_tid <- map(url_list, ~ hamta_giltiga_varden_fran_tabell(.x, "tid")) %>%
      unlist() %>%
      unique()
    
    senaste_ar <- max(giltig_tid)
    
    hamta_tid <- if (all(tid_koder == "*")) {
      giltig_tid
    } else {
      tid_koder %>%
        as.character() %>%
        str_replace("9999", senaste_ar) %>%
        .[. %in% giltig_tid] %>%
        unique()
    }
    
    # hamta_data() returnerar en df per tabell och att list_rbind() binder ihop gammal och CKM-tabell.
    
    hamta_data <- function(url_uttag) {
      
      px_meta <- pxweb_get(url_uttag)
      
      # Gör om från klartext till kod per tabell
      inflyttningsl_vekt <- if (all(inflyttningsl_klartext == "*")) {
        hamta_giltiga_varden_fran_tabell(px_meta, "inflyttningsl")
      } else {
        hamta_kod_med_klartext(px_meta, inflyttningsl_klartext, skickad_fran_variabel = "inflyttningsl")
      }
      
      utflyttningsl_vekt <- if (all(utflyttningsl_klartext == "*")) {
        hamta_giltiga_varden_fran_tabell(px_meta, "utflyttningsl")
      } else {
        hamta_kod_med_klartext(px_meta, utflyttningsl_klartext, skickad_fran_variabel = "utflyttningsl")
      }
      
      kon_vekt <- if (!all(is.na(kon_klartext))) {
        if (all(kon_klartext == "*")) {
          hamta_giltiga_varden_fran_tabell(px_meta, "kon")
        } else {
          hamta_kod_med_klartext(px_meta, kon_klartext, skickad_fran_variabel = "kon")
        }
      } else {
        NA
      }
      
      cont_vekt <- if (all(cont_klartext == "*")) {
        hamta_giltiga_varden_fran_tabell(px_meta, "contentscode")
      } else {
        hamta_kod_med_klartext(px_meta, cont_klartext, skickad_fran_variabel = "contentscode")
      }
      
      # Filtrera tid så att just denna tabell bara får sina giltiga år
      giltiga_ar_tabell <- hamta_giltiga_varden_fran_tabell(px_meta, "tid")
      tabell_tid <- hamta_tid[hamta_tid %in% giltiga_ar_tabell]
      
      # Om inga år finns i denna tabell för uttaget: returnera NULL
      if (length(tabell_tid) == 0) return(NULL)
      
      # Bygg query
      varlista <- list(
        InflyttningsL = inflyttningsl_vekt,
        UtflyttningsL = utflyttningsl_vekt,
        Kon = kon_vekt,
        ContentsCode = cont_vekt,
        Tid = tabell_tid
      )
      
      if (all(is.na(kon_klartext))) {
        varlista <- varlista[names(varlista) != "Kon"]
      }
      
      px_uttag <- pxweb_get(url = url_uttag, query = varlista)
      
      retur_df <- as.data.frame(px_uttag)
      
      # Om du senare vill lägga till kodkolumner, gör det här på samma sätt som i andra funktionen
      # Exempel:
      # retur_df <- retur_df %>%
      #   cbind(as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>%
      #           select(InflyttningsL, UtflyttningsL)) %>%
      #   rename(inflyttningsl_kod = InflyttningsL,
      #          utflyttningsl_kod = UtflyttningsL)
      
      return(retur_df)
    }
    
    px_df <- map(url_list, ~ hamta_data(.x)) %>%
      list_rbind()
    
    if (skriv_excelfil) {
      writexl::write_xlsx(px_df, paste0(output_mapp, excel_filnamn))
    }
    
    if (returnera_df) return(px_df)
    
  } else {
    print("Varken 'returnera_df' eller sökväg för excelfil har valts, så inget returneras.")
  }
}
