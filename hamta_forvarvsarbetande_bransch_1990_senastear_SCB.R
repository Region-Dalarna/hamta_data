hamta_data_sysselsatta_1990 <- function(region_vekt = "20", # Val av region. 
                                        output_mapp = NA, # Här hamnar sparad data. Ändra till en sökväg för att data skall sparas
                                        kon_klartext = c("män","kvinnor"), # Män och kvinnor eller var och en uppdelad (inte totalt).
                                        returnera_data = TRUE, # Vill användaren returnera data som en dataframe
                                        filnamn = "forvarvsarbetande_90_senast.xlsx"){ # Filnamn
  
  
  # ===========================================================================================================  #
  # Skript för att hämta data för förvärvsarbetande med arbetsplats i vald region från 1990 och framåt. Använder
  # flera olika databaser. 1990 - 2019 baseras på RAMS, men därefter är det BAS. 16-74 år
  # Funkar bara för regioner (eller Sverige)
  # Branschgrupper baseras på den äldre klassificering som gällde innan SNI2007 (eftersom det fanns färre branschgrupper tidigare)
  # Notera att matchning mellan branschgrupper före och efter 2007 är delvis subjektiv (se case_when satser nedan)
  #
  # Skapad av Jon Frank
  # Uppdaterad senast 2024-01-04
  # ===========================================================================================================
  # Paket som behövs
  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
         tidyverse,
         openxlsx)
  
  # Funktioner som behövs
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  # Förvärvsarbetande under olika tidsperioder kommer från olika källor. Först RAMS fram till 2019. Byter sedan till BAS
  url_1990_2003 <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0207/AM0207C/AMPAK3"
  url_2004_2007 <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0207/AM0207K/DagSNIKonK"
  url_2008_2018 <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0207/AM0207K/DagSNI07KonK"
  url_2019 <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0207/AM0207Z/DagSni07KonKN"
  url_2020_ <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0210/AM0210B/ArbStDoNArNN"
  
  # Gör om från klartext
  kon_vekt <- hamta_kod_med_klartext(url_1990_2003, kon_klartext, skickad_fran_variabel = "kon")
  
  # Variabellistor
  url <- c(url_1990_2003,url_2004_2007,url_2008_2018,url_2019,url_2020_)
  lista = list()
  
  varlista_1990_2003 <- list(
    Region = c(region_vekt),
    SNI92 = '*',
    Kon = kon_vekt,
    ContentsCode = "*",
    Tid = c('*')
  )
  
  varlista_2004_2007 <- list(
    Region = c(region_vekt),
    SNI2002 = '*',
    Kon = kon_vekt,
    ContentsCode = "*",
    Tid = c('*')
  )
  
  varlista_2008_2018 <- list(
    Region = c(region_vekt),
    SNI2007 = '*',
    Kon = kon_vekt,
    ContentsCode = "*",
    Tid = c('*')
  )
  
  varlista_2019 <- list(
    Region = c(region_vekt),
    SNI2007 = '*',
    Kon = kon_vekt,
    ContentsCode = "*",
    Tid = "2019"
  )
  
  varlista_2020_ <- list(
    Region = c(region_vekt),
    SNI2007 = '*',
    Kon = kon_vekt,
    Fodelseregion = "tot",
    ContentsCode = "000005FG" ,
    Tid = "*"
  )
  
  varlista_lista=list(varlista_1990_2003,varlista_2004_2007,varlista_2008_2018,varlista_2019,varlista_2020_)
  
  diagram_capt <- "Källa: RAMS i SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Branschgruppering baserad på SNI2002 och SNI92"
  
  # =============================================== API-uttag ===============================================
  i=1
  
  while(i<(length(url)+1)){
    px_uttag <- pxweb_get(url = url[i],
                          query = varlista_lista[[i]]
    ) 
    
    # Lägg API-uttaget i px_df, lägg på ytterligare ett uttag men med koder istället för klartext,
    # välj ut bara regionkolumnen i det andra uttaget, döp om den till regionkod och lägg den först av kolumnerna
    
    lista[[i]] <- as.data.frame(px_uttag) %>% 
      cbind(regionkod = as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>% 
              select(Region))  
    lista[[i]] <- lista[[i]] %>% rename(regionkod = Region) %>% relocate(regionkod, .before = region)
    names(lista[[i]])[ncol(lista[[i]])] <- "Förvärvsarbetande 16+ år (dagbef)"
    
    i=i+1
  }
  
  # Namnger lista
  names(lista) <- c("1990-2003","2004-2007","2008-2018","2019","2020_")
  
  # Branschgruppering ändras när nya SNI-koder införs 2007. Den mest aggregerade grupperingen (innan 2008) används
  k=3 # Skall enbart göras för mellan 2008 och 2019. Vid byte till BAS ändrar SCB beteckning på branscherna (igen)
  
  while(k <= (length(varlista_lista)-1)){
    lista[[k]]$Branschgrupp <- case_when(
      lista[[k]]$`näringsgren SNI 2007` == "jordbruk, skogsbruk och fiske" ~ "jordbruk, skogsbruk, jakt, fiske",
      lista[[k]]$`näringsgren SNI 2007` == "tillverkning och utvinning" ~ "utvinning av mineral, tillverkningsindustri",
      lista[[k]]$`näringsgren SNI 2007` == "energiförsörjning; miljöverksamhet" ~ "energi- o vattenförsörjning, avfallshantering",
      lista[[k]]$`näringsgren SNI 2007` == "byggverksamhet" ~ "byggindustri",
      lista[[k]]$`näringsgren SNI 2007` == "handel" ~ "handel; transport, magasinering; kommunikation",
      lista[[k]]$`näringsgren SNI 2007` == "transport och magasinering"~ "handel; transport, magasinering; kommunikation",
      lista[[k]]$`näringsgren SNI 2007` == "hotell- och restaurangverksamhet" ~ "personliga och kulturella tjänster",
      lista[[k]]$`näringsgren SNI 2007` == "information och kommunikation" ~ "kreditinstitut, fastighetsförvaltn, företagstjänster",
      lista[[k]]$`näringsgren SNI 2007` == "finans- och försäkringsverksamhet" ~ "kreditinstitut, fastighetsförvaltn, företagstjänster",
      lista[[k]]$`näringsgren SNI 2007` == "fastighetsverksamhet" ~ "kreditinstitut, fastighetsförvaltn, företagstjänster",
      lista[[k]]$`näringsgren SNI 2007` == "företagstjänster" ~ "kreditinstitut, fastighetsförvaltn, företagstjänster",
      lista[[k]]$`näringsgren SNI 2007` == "offentlig förvaltning och försvar"~ "civila myndigheter, försvar; internat. organisationer",
      lista[[k]]$`näringsgren SNI 2007` == "utbildning " ~ "forskning o utveckling; utbildning",
      lista[[k]]$`näringsgren SNI 2007` == "vård och omsorg; sociala tjänster" ~ "enh för hälso- och sjukvård, socialtjänst; veterinärer",
      lista[[k]]$`näringsgren SNI 2007` == "kulturella och personliga tjänster m.m." ~ "personliga och kulturella tjänster",
      lista[[k]]$`näringsgren SNI 2007` == "okänd verksamhet"~ "näringsgren okänd")
    k=k+1
  }
  
  # Vid byte till bas ändras namn på branscher igen (dessutom läggs totalt till). För att matcha mot tidigare år tas totalt bort och sedan ändras namn på branscher
  lista[[k]] <- lista[[k]] %>%filter(`näringsgren SNI 2007` != "Total")
  
  lista[[k]]$Branschgrupp <- case_when(
    lista[[k]]$`näringsgren SNI 2007` == "företag inom jordbruk, skogsbruk och fiske" ~ "jordbruk, skogsbruk, jakt, fiske",
    lista[[k]]$`näringsgren SNI 2007` == "tillverkningsindustri; gruvor och mineralutvinningsindustri" ~ "utvinning av mineral, tillverkningsindustri",
    lista[[k]]$`näringsgren SNI 2007` == "företag inom energi och miljö" ~ "energi- o vattenförsörjning, avfallshantering",
    lista[[k]]$`näringsgren SNI 2007` == "byggindustri" ~ "byggindustri",
    lista[[k]]$`näringsgren SNI 2007` == "handel; serviceverkstäder för motorfordon och motorcyklar" ~ "handel; transport, magasinering; kommunikation",
    lista[[k]]$`näringsgren SNI 2007` == "transport- och magasineringsföretag" ~ "handel; transport, magasinering; kommunikation",
    lista[[k]]$`näringsgren SNI 2007` == "hotell och restauranger" ~ "personliga och kulturella tjänster",
    lista[[k]]$`näringsgren SNI 2007` == "informations- och kommunikationsföretag" ~ "kreditinstitut, fastighetsförvaltn, företagstjänster",
    lista[[k]]$`näringsgren SNI 2007` == "kreditinstitut och försäkringsbolag m.m." ~ "kreditinstitut, fastighetsförvaltn, företagstjänster",
    lista[[k]]$`näringsgren SNI 2007` == "fastighetsbolag och fastighetsförvaltare" ~ "kreditinstitut, fastighetsförvaltn, företagstjänster",
    lista[[k]]$`näringsgren SNI 2007` == "företag inom juridik, ekonomi, vetenskap och teknik; företag inom uthyrning, fastighetsservice, resetjänster och andra stödtjänster" ~ "kreditinstitut, fastighetsförvaltn, företagstjänster",
    lista[[k]]$`näringsgren SNI 2007` == "civila myndigheter och försvaret" ~ "civila myndigheter, försvar; internat. organisationer",
    lista[[k]]$`näringsgren SNI 2007` == "utbildningsväsendet" ~ "forskning o utveckling; utbildning",
    lista[[k]]$`näringsgren SNI 2007` == "enheter för vård och omsorg, socialtjänst" ~ "enh för hälso- och sjukvård, socialtjänst; veterinärer",
    lista[[k]]$`näringsgren SNI 2007` == "enheter för kultur, nöje och fritid; andra serviceföretag m.m." ~ "personliga och kulturella tjänster",
    lista[[k]]$`näringsgren SNI 2007` == "uppgift saknas"~ "näringsgren okänd")
  
  
  # Döper om variabler för att sedan slå ihop de dataset
  # 1990-2003
  lista[[1]] <- lista[[1]] %>% 
    rename("Näringsgren"="näringsgren SNI92")
  # 2004-2007
  lista[[2]] <- lista[[2]] %>% 
    rename("Näringsgren"="näringsgren SNI 2002")
  # 2008-2018
  lista[[3]] <- lista[[3]] %>% 
    select(-c("näringsgren SNI 2007")) %>% 
    rename("Näringsgren"="Branschgrupp") 
  # 2019
  lista[[4]] <- lista[[4]] %>% 
    select(-c("näringsgren SNI 2007")) %>% 
    rename("Näringsgren"="Branschgrupp")  
  # 2020-
  lista[[5]] <- lista[[5]] %>% 
    select(-c("näringsgren SNI 2007","födelseregion")) %>% 
    rename("Näringsgren"="Branschgrupp") 
  
  
  df_utskrift <- rbind(lista[[1]],lista[[2]],lista[[3]],lista[[4]],lista[[5]])
  
  # Grupperar på näringsgren och år
  df_utskrift <- df_utskrift %>% 
    group_by(region,kön,Näringsgren,år) %>% 
    summarize(antal = sum(`Förvärvsarbetande 16+ år (dagbef)`))
  
  if (!is.na(output_mapp) & !is.na(filnamn)){
    write.xlsx(df_utskrift,paste0(output_mapp,filnamn))
  }
  
  if(returnera_data == TRUE) return(df_utskrift)
  
}
