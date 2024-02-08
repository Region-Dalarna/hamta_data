hamta_data_ohalsotal_regso <- function(region_vekt = "20",
                                       Cont_code = "000004X8", # För val, se nedan
                                       bakgrund_klartext = c("samtliga"), # För alternativ, se nedan
                                       kon_klartext = "män och kvinnor", # c("män","kvinnor") ger uppdelat. "män och kvinnor" ger totalt
                                       output_mapp = NA, # Outputmapp. Sätts till en mapp om data skall sparas
                                       filnamn = "ohalsotal_regso.xlsx", # Filnamn. Ändra om man vill köra ut en för regso respektive deso
                                       returnera_data = TRUE, # Om man vill returnera data
                                       tid = "*") # Sätts till "9999" om man enbart vill ha senaste år, alternativt ett intervall som slutar på "9999". "*" ger samtliga år
{
  
  # ===========================================================================================================
  #
  # Skript för att hämta data för ohälsotal kopplat till Regso. Källa:
  # https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AA__AA0003__AA0003I/IntGr10RegSOKon/table/tableViewLayout1/
  # 
  # För att få en djupare förklaring av vad som de olika kategorierna under varje variabel betyder, använd:
  # pxvardelist("https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AA/AA0003/AA0003I/IntGr10RegSOKon", "Bakgrund")
  # För att få en förståelse för alla variabler som finns, använd
  # pxvarlist("https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AA/AA0003/AA0003I/IntGr10RegSOKon")
  # Båda kräver  att source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R") har körts först
  # Förklaringar av huvudvariabler
  # Cont_code:
  # 000004X8                    Ohälsotalet, antal dagar
  # 000004X9 Andel som bidrar till ohälsotalet , procent
  #
  # Skapad av Jon Frank 2024-02-08
  # 
  # ===========================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
         tidyverse,
         openxlsx)
  
  # "Adresser" till SCBs databas
  url_uttag <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AA/AA0003/AA0003I/IntGr10RegSOKon"
  
  region_indelning = "RegSO"
  
  # Data som sourcas från Region Dalarna
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  ########### Hanterar Regso/Deso #######################################################################################
  # vi hämtar alla regionkoder i databasen
  alla_regionkoder <- hamta_giltiga_varden_fran_tabell(url_uttag, "region") 
  # vi tar ut endast RegSO eller DeSO beroende på vad användaren valt
  alla_regionkoder <- if(region_indelning == "RegSO") alla_regionkoder[str_detect(alla_regionkoder, "R")] else alla_regionkoder[str_detect(alla_regionkoder, "A|B|C")]
  
  if (region_vekt != "*") {
    # här delar vi upp de medskickade regionkoderna i RegSO/DeSO, länskoder och kommunkoder 
    fardiga_regionkoder <- region_vekt[str_length(region_vekt) == 9]           # RegSO eller DeSO
    lanskoder <- region_vekt[str_length(region_vekt) == 2]                     # länskoder
    kommunkoder <- region_vekt[str_length(region_vekt) == 4]                   # kommunkoder
    
    # här hämtar vi alla RegSO/DeSO för de länskoder eller kommunkoder som skickats med
    region_lan <- alla_regionkoder[str_sub(alla_regionkoder, 1,2) %in% lanskoder & str_length(alla_regionkoder) > 7]
    region_kommun <- alla_regionkoder[str_sub(alla_regionkoder, 1,4) %in% lanskoder & str_length(alla_regionkoder) > 7]
    
    # vi lägger ihop vektorerna för färdiga RegSO/DeSO, samt RegSO/DeSO för de län och kommuner som skickats med samt tar bort dubletter
    alla_region <- c(fardiga_regionkoder, region_lan, region_kommun) %>% .[!duplicated(.)]
    
  } else alla_region <- alla_regionkoder                     # om användaren vill ha samtliga koder för RegSO eller DeSO
  #########################################################################################################################
  
  # Gör om från klartext
  kon_vekt <- hamta_kod_med_klartext(url_uttag, kon_klartext, skickad_fran_variabel = "kon")
  
  if ( all(bakgrund_klartext == "*")){
    
    bakgrund_vekt = "*"
    
  }else bakgrund_vekt <- hamta_kod_med_klartext(url_uttag, bakgrund_klartext, skickad_fran_variabel = "bakgrund")
  
  giltiga_ar <- hamta_giltiga_varden_fran_tabell(url_uttag, "tid")
  if (all(tid != "*")) tid <- tid %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar] %>% unique()
  
  varlista <- list(Region = alla_region,
                   Bakgrund = bakgrund_vekt,
                   Kon= kon_vekt,
                   ContentsCode = Cont_code,
                   Tid = tid)
  
  px_uttag <- pxweb_get(url = url_uttag,query = varlista)
  
  # Gör uttaget samt diverse justeringar och grupperingar av data.
  ohalsotal <- as.data.frame(px_uttag) %>% 
    cbind(as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>%
            select(regionkod = Region)) %>%
    relocate(regionkod, .before = region) 
  
  
  if (!is.na(output_mapp) & !is.na(filnamn)){
    write.xlsx(ohalsotal,paste0(output_mapp,filnamn))
  }
  
  # Data returneras som en DF om användaren vill det
  if(returnera_data == TRUE) return(ohalsotal) 
  
}
