hamta_data_inkomst_regso <- function(region_vekt = "20",
                                     cont_klartext = "Medianvärde, tkr", # Finns: 
                                     # "Medianvärdet av disponibel inkomst, antal prisbasbelopp"
                                     # "Kvartil 1, andel personer, procent",
                                     # "Kvartil 2, andel personer, procent",
                                     # "Kvartil 3, andel personer, procent",
                                     # "Kvartil 4, andel personer, procent",
                                     # "Medianvärde, tkr",
                                     # "Medelvärde, tkr",
                                     # "Antal personer totalt",
                                     inkomsttyp_klartext = "nettoinkomst", # Finns också: "sammanräknad förvärvsinkomst"
                                     region_indelning = "RegSO",          # Finns: "RegSO", "DeSO"
                                     kon_klartext = "totalt",  # Finns: "totalt", "män", "kvinnor"
                                     output_mapp = NA,                  # Outputmapp. Sätts till en mapp om data skall sparas
                                     filnamn = "inkomst_regso.xlsx",  # Filnamn om man vill spara en excelfil i output_mapp.
                                     returnera_data = TRUE,             # Om man vill returnera data som en dataframe från funktionen
                                     long_format = TRUE,                # om man tar med fler än en innehållsvariabel så görs format om från wide till long
                                     tid = "*")                         # Sätts till "9999" om man enbart vill ha senaste år, alternativt ett intervall som slutar 
# på "9999". "*" ger samtliga år
{
  
  # ===========================================================================================================
  #
  # Skript för att hämta data för inkomst kopplat till Regso.Redovisar personer 20 år och äldre folkbokförda i Sverige 1/1 resp. 31/12 resp. år. 
  # Källa:
  # https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__HE__HE0110__HE0110I/Tab1InkDesoN/
  
  # För att få en djupare förklaring av vad som de olika kategorierna under varje variabel betyder, använd:
  # pxvardelist("https://api.scb.se/OV0104/v1/doris/sv/ssd/START/HE/HE0110/HE0110I/Tab1InkDesoN", "ContentsCode")
  # För att få en förståelse för alla variabler som finns, använd
  # pxvarlist("https://api.scb.se/OV0104/v1/doris/sv/ssd/START/HE/HE0110/HE0110I/Tab1InkDesoN")
  # Båda kräver  att source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R") har körts först
  #
  # Förklaringar inkomst :
  # Sammanräknad förvärvsinkomst
  # Summan av inkomst av tjänst och inkomst av näringsverksamhet. Den sammanräknade förvärvsinkomsten består av de sammanlagda löpande skattepliktiga inkomsterna, 
  # vilket avser inkomster från anställning, företagande, pension, sjukpenning och andra skattepliktiga transfereringar. 
  # I sammanräknad förvärvsinkomst ingår inte inkomst av kapital.sammanräknad förvärvsinkomst
  #
  # Nettoinkomst:
  # Nettoinkomst är summan av en persons alla skattepliktiga och skattefria inkomster minus skatt och övriga negativa transfereringar (exempelvis återbetalt studielån).
  #
  # Skapad av Jon Frank 2024-02-22
  # ===========================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
         tidyverse,
         openxlsx)
  
  # "Adresser" till SCBs databas
  url_uttag <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/HE/HE0110/HE0110I/Tab1InkDesoN"
  px_meta <- pxweb_get(url_uttag)          # vi hämtar metadata till tabellen här och gör inga fler uttag nedan = färre API-anrop (och elegantare lösning)
  
  #region_indelning = "RegSO"
  
  # Data som sourcas från Region Dalarna
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  ########### Hanterar Regso/Deso #######################################################################################
  # vi hämtar alla regionkoder i databasen
  alla_regionkoder <- hamta_giltiga_varden_fran_tabell(px_meta, "region") 
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
  kon_vekt <- hamta_kod_med_klartext(px_meta, kon_klartext, skickad_fran_variabel = "kon")
  cont_vekt <- hamta_kod_med_klartext(px_meta, cont_klartext, skickad_fran_variabel = "contentscode")
  inkomsttyp_vekt <- if (all(inkomsttyp_klartext == "*")) "*" else hamta_kod_med_klartext(px_meta, inkomsttyp_klartext, skickad_fran_variabel = "inkomsttyp")
  
  giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "tid")
  if (all(tid != "*")) tid <- tid %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar] %>% unique()
  
  varlista <- list(Region = alla_region,
                   InkomstTyp = inkomsttyp_vekt,
                   Kon = kon_vekt,
                   ContentsCode = cont_vekt,
                   Tid = tid)
  
  px_uttag <- pxweb_get(url = url_uttag,query = varlista)
  
  # Gör uttaget samt diverse justeringar och grupperingar av data.
  inkomst <- as.data.frame(px_uttag) %>% 
    cbind(as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>%
            select(regionkod = Region)) %>%
    relocate(regionkod, .before = region) 
  
  # man kan välja bort long-format, då låter vi kolumnerna vara wide om det finns fler innehållsvariabler, annars
  # pivoterar vi om till long-format, dock ej om det bara finns en innehållsvariabel
  if (long_format & length(cont_klartext) > 1) {
    inkomst <- inkomst %>% 
      konvertera_till_long_for_contentscode_variabler(api_url = px_meta, content_var = "ohalso_variabel")
  } # slut if-sats som kontrollera om vi vill ha df i long-format
  
  if (!is.na(output_mapp) & !is.na(filnamn)){
    write.xlsx(inkomst,paste0(output_mapp,filnamn))
  }
  
  # Data returneras som en DF om användaren vill det
  if(returnera_data == TRUE) return(inkomst) 
  
}
