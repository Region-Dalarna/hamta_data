hamta_data_valdeltagande_regso <- function(region_vekt = "20",
                                     cont_klartext = "Valdeltagande i val till kommun, procent", # Finns: "Valdeltagande i val till riksdag, procent", "Valdeltagande i val till region, procent"
                                     bakgrund_klartext = c("samtliga"), # Finns: "samtliga 20-64 år", "ålder: 20-24 år", "ålder: 25-34 år", "ålder: 35-44 år", 
                                     #         "ålder: 45-54 år", "ålder: 55-64 år", "samtliga utbildningsnivåer", 
                                     #         "utbildningsnivå: förgymnasial utbildning", "utbildningsnivå: gymnasial utbildning", 
                                     #         "utbildningsnivå: eftergymnasial utbildning", "utbildningsnivå: uppgift saknas", 
                                     #         "samtliga", "födelseregion: Sverige", "födelseregion: Norden exkl. Sverige", 
                                     #         "födelseregion: EU/EFTA exkl. Norden", "födelseregion: övriga världen", 
                                     #         "samtliga utrikes födda invandrare", "samtliga utrikes födda invandrare", 
                                     #         "skäl till invandring: skyddsbehövande och deras anhöriga", 
                                     #         "skäl till invandring: övriga utrikes födda invandrare", "samtliga utrikes födda", 
                                     #         "vistelsetid 0-1 år", "vistelsetid 2-3 år", "vistelsetid 4-9 år", "vistelsetid 10- år"
                                     kon_klartext = "män och kvinnor",  # Finns: "män och kvinnor", "män", "kvinnor"
                                     output_mapp = NA,                  # Outputmapp. Sätts till en mapp om data skall sparas
                                     filnamn = "valdeltagande_regso.xlsx",  # Filnamn om man vill spara en excelfil i output_mapp.
                                     returnera_data = TRUE,             # Om man vill returnera data som en dataframe från funktionen
                                     wide_om_en_contvar = TRUE,			# TRUE = om man vill behålla wide-format om det bara finns en innehållsvariabel, FALSE om man vill konvertera till long-format även om det bara finns en innehållsvariabel
                                     long_format = TRUE,                # om man tar med fler än en innehållsvariabel så görs format om från wide till long
                                     tid = "*")                         # Sätts till "9999" om man enbart vill ha senaste år, alternativt ett intervall som slutar 
# på "9999". "*" ger samtliga år
{
  
  # ===========================================================================================================
  #
  # Skript för att hämta data för valdeltagande (på olika nivåer) kopplat till Regso. Källa:
  # https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AA__AA0003__AA0003J/IntGr11RegSO1/
  # 
  # För att få en djupare förklaring av vad som de olika kategorierna under varje variabel betyder, använd:
  # pxvardelist("https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AA/AA0003/AA0003J/IntGr11RegSO1", "Bakgrund")
  # För att få en förståelse för alla variabler som finns, använd
  # pxvarlist("https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AA/AA0003/AA0003J/IntGr11RegSO1")
  # Båda kräver  att source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R") har körts först
  #
  # Skapad av Jon Frank 2024-02-22
  # Reviderat av Jon Frank 2025-09-04, se hamta_data_ohalsotal_regso för kommentarer om ändringar
  # ===========================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
         tidyverse,
         openxlsx)
  
  # "Adresser" till SCBs databas
  url_uttag <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AA/AA0003/AA0003J/IntGr11RegSO1"
  px_meta <- pxweb_get(url_uttag)          # vi hämtar metadata till tabellen här och gör inga fler uttag nedan = färre API-anrop (och elegantare lösning)
  
  region_indelning = "RegSO"
  
  # Data som sourcas från Region Dalarna
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  ########### Hanterar Regso/Deso #######################################################################################
  # vi hämtar alla regionkoder i databasen
  alla_regionkoder <- hamta_giltiga_varden_fran_tabell(px_meta, "region") 
  # vi tar ut endast RegSO eller DeSO beroende på vad användaren valt
  alla_regionkoder <- if(region_indelning == "RegSO") alla_regionkoder[str_detect(alla_regionkoder, "R")] else alla_regionkoder[str_detect(alla_regionkoder, "A|B|C")]
  
  if (!("*" %in% region_vekt)) {
    # här delar vi upp de medskickade regionkoderna i RegSO/DeSO, länskoder och kommunkoder 
    fardiga_regionkoder <- region_vekt[str_length(region_vekt) > 7]           # RegSO eller DeSO
    lanskoder <- region_vekt[str_length(region_vekt) == 2]                     # länskoder
    kommunkoder <- region_vekt[str_length(region_vekt) == 4]                   # kommunkoder
    
    # här hämtar vi alla RegSO/DeSO för de länskoder eller kommunkoder som skickats med
    region_lan <- alla_regionkoder[str_sub(alla_regionkoder, 1,2) %in% lanskoder & str_length(alla_regionkoder) > 7]
    region_kommun <- alla_regionkoder[str_sub(alla_regionkoder, 1,4) %in% kommunkoder & str_length(alla_regionkoder) > 7]
    
    # vi lägger ihop vektorerna för färdiga RegSO/DeSO, samt RegSO/DeSO för de län och kommuner som skickats med samt tar bort dubletter
    alla_region <- c(fardiga_regionkoder, region_lan, region_kommun) %>% .[!duplicated(.)]
    
  } else alla_region <- alla_regionkoder                     # om användaren vill ha samtliga koder för RegSO eller DeSO
  
  # Tidigare, felaktigt
  # if (region_vekt != "*") {
  #   # här delar vi upp de medskickade regionkoderna i RegSO/DeSO, länskoder och kommunkoder 
  #   fardiga_regionkoder <- region_vekt[str_length(region_vekt) == 9]           # RegSO eller DeSO
  #   lanskoder <- region_vekt[str_length(region_vekt) == 2]                     # länskoder
  #   kommunkoder <- region_vekt[str_length(region_vekt) == 4]                   # kommunkoder
  #   
  #   # här hämtar vi alla RegSO/DeSO för de länskoder eller kommunkoder som skickats med
  #   region_lan <- alla_regionkoder[str_sub(alla_regionkoder, 1,2) %in% lanskoder & str_length(alla_regionkoder) > 7]
  #   region_kommun <- alla_regionkoder[str_sub(alla_regionkoder, 1,4) %in% lanskoder & str_length(alla_regionkoder) > 7]
  #   
  #   # vi lägger ihop vektorerna för färdiga RegSO/DeSO, samt RegSO/DeSO för de län och kommuner som skickats med samt tar bort dubletter
  #   alla_region <- c(fardiga_regionkoder, region_lan, region_kommun) %>% .[!duplicated(.)]
  #   
  # } else alla_region <- alla_regionkoder                     # om användaren vill ha samtliga koder för RegSO eller DeSO
  #########################################################################################################################
  
  # Gör om från klartext
  kon_vekt <- hamta_kod_med_klartext(px_meta, kon_klartext, skickad_fran_variabel = "kon")
  cont_vekt <- hamta_kod_med_klartext(px_meta, cont_klartext, skickad_fran_variabel = "contentscode")
  bakgrund_vekt <- if (all(bakgrund_klartext == "*")) "*" else hamta_kod_med_klartext(px_meta, bakgrund_klartext, skickad_fran_variabel = "bakgrund")
  
  giltiga_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "tid")
  if (all(tid != "*")) tid <- tid %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar] %>% unique()
  
  varlista <- list(Region = alla_region,
                   Bakgrund = bakgrund_vekt,
                   Kon = kon_vekt,
                   ContentsCode = cont_vekt,
                   Tid = tid)
  
  px_uttag <- pxweb_get(url = url_uttag,query = varlista)
  
  # Gör uttaget samt diverse justeringar och grupperingar av data.
  valdeltagande <- as.data.frame(px_uttag) %>% 
    cbind(as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>%
            select(regionkod = Region)) %>%
    relocate(regionkod, .before = region) 
  
  # man kan välja bort long-format, då låter vi kolumnerna vara wide om det finns fler innehållsvariabler, annars
  # pivoterar vi om till long-format, dock ej om det bara finns en innehållsvariabel
  if (long_format & !wide_om_en_contvar) {
    valdeltagande <- valdeltagande %>% 
      konvertera_till_long_for_contentscode_variabler(api_url = px_meta, content_var = "valdeltagande_variabel")
  } # slut if-sats som kontrollera om vi vill ha df i long-format
  
  if (!is.na(output_mapp) & !is.na(filnamn)){
    write.xlsx(valdeltagande,paste0(output_mapp,filnamn))
  }
  
  # Data returneras som en DF om användaren vill det
  if(returnera_data == TRUE) return(valdeltagande) 
  
}
