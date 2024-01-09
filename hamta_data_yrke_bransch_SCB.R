hamta_data_yrken_bransch <- function(region_vekt = "20", # Val av region. Finns såväl regioner som kommuner 
                                        output_mapp = NA, # Här hamnar sparad data. Ändra till en sökväg för att data skall sparas
                                        kon_klartext = c("män","kvinnor"), # Män och kvinnor eller var och en uppdelad (inte totalt).
                                        yrke_klartext = "*", # Se nedan för alternativa val. "*" för alla
                                        bransch_klartext = "*", # Se nedan för alternativa val. "*" för alla
                                        returnera_data = TRUE, # Vill användaren returnera data som en dataframe
                                        tid = "9999", # Sätts till "9999" om man enbart vill ha senaste år,"*" för alla alternativt intervall
                                        filnamn = "yrke_bransch.xlsx"){ # Filnamn
  
  
  # ===========================================================================================================  #
  # Skript för att hämta data för förvärvsarbetande baserat på yrke och bransch. Funkar för regioner, kommuner och Sverige
  # Funkar bara för regioner (eller Sverige)
  # För att välja specifika yrken och/eller branscher, använd namn på yrken/branscher från funktionen nedan
  # pxvardelist("https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0208/AM0208D/YREG56N", "SNI2007")
  # Skapad av Jon Frank
  # Uppdaterad senast 2024-01-09
  # ===========================================================================================================
  # Paket som behövs
  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
         tidyverse,
         openxlsx)
  
  # Funktioner som behövs
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  # Url till SCB:s databas
  url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0208/AM0208D/YREG56N"
  
  # Gör om från klartext till kod som databasen förstår
  if (kon_klartext == "*"){
    
    kon_vekt = "*"
    
  }else kon_vekt <- hamta_kod_med_klartext(url, kon_klartext, skickad_fran_variabel = "kon")
  
  if (yrke_klartext == "*"){
    
    yrke_vekt = "*"
    
  }else yrke_vekt <- hamta_kod_med_klartext(url, yrke_klartext, skickad_fran_variabel = "Yrke2012")
  
  if ( bransch_klartext == "*"){
    
    brancsh_vekt = "*"
    
  }else brancsh_vekt <- hamta_kod_med_klartext(url, bransch_klartext, skickad_fran_variabel = "SNI2007")

  # Om tid har satts till 9999, välj senaste år
  if("9999" %in% tid) tid = max(hamta_giltiga_varden_fran_tabell(url, "tid"))
  
  varlista <- list(
    Region = region_vekt,
    Yrke2012 = yrke_vekt,
    SNI2007 = brancsh_vekt,
    Kon = kon_vekt,
    ContentsCode = c("000003T3"),
    Tid = tid)
  
  px_uttag <- pxweb_get(url = url,
                        query = varlista) 
  
  # Lägg API-uttaget i px_df, lägg på ytterligare ett uttag men med koder istället för klartext
  px_df <- as.data.frame(px_uttag) %>% 
    cbind(Yrke2012_kod = as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>% 
            select(Yrke2012)) %>% 
    rename(yrkeskod = Yrke2012) %>% 
      relocate(yrkeskod, .before = 'Yrke (SSYK 2012)') %>% 
        cbind(SNI2007_kod = as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>% 
            select(SNI2007)) %>% 
          rename(branschkod = SNI2007) %>% 
            relocate(branschkod, .before = 'näringsgren SNI 2007')
  
  # Ändrar namn på en av variablerna
  names(px_df)[ncol(px_df)] <- "Anställda 16-64 år (dagbef)"

  # Om användaren vill spara data
  if (!is.na(output_mapp) & !is.na(filnamn)){
    write.xlsx(px_df,paste0(output_mapp,filnamn))
  }
  
  # Om användaren vill returnera data som en DF.
  if(returnera_data == TRUE) return(px_df)
  
}
