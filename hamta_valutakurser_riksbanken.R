if (!require("pacman")) install.packages("pacman")
p_load(tidyverse,
       httr,
       jsonlite)

hamta_valutakurser <- function(start_datum = "2015-01-02",
                               valutakoder = c("EUR", "GBP", "USD")
) {

  # ===========================================================================================================
  #
  # Skript för att hämta data över valutakurser från Riksbanken via deras API.  
  # 
  # Parametrar som skickas med är:
  # - start_datum                                                 # vilket datum ska data hämtas från. Det är olika för olika valutor när tidsserien börjar, de första startar 1993-01-04, många senare än så 
  # - valutakoder                                                 # vilka valutor ska data hämtas för
  #                                                               # för att se vilka valutor som finns och vilka koder de har,
  #                                                                 kör funktionen lista_valutor() nedan
  #
  # Skapat av: Peter Möller, Region Dalarna
  #            september 2023
  # Senast uppdaterat:  december 2023
  #                     (mindre justeringar)
  # 
  # ===========================================================================================================
  

  
# i nuläget endast för att hämta månadsvis data, men det går att hämta per dag, vecka, månad och år
  
valutalista <- fromJSON(content(GET(paste0("https://api-test.riksbank.se/swea/v1/Series/ExchangeRateSeries")), as = "text", encoding = "utf-8"), flatten = TRUE)

till_datum <- valutalista %>% 
  filter(shortDescription %in% valutakoder) %>% 
  select(observationMaxDate) %>% 
  pull() %>% 
  max()

# hämta id-nummer för de valutor vi valt
valda_valutor_id <- valutalista %>% 
  filter(shortDescription %in% valutakoder) %>% 
  select(seriesId) %>% 
  pull()

# hämta valutakoderna här också, så att vi vet att vi får dem i samma ordning som id-numren ovan
valda_valutor_kod <- valutalista %>% 
  filter(shortDescription %in% valutakoder) %>% 
  select(shortDescription) %>% 
  pull()

# hämta data
tidsserie <- map2_dfr(valda_valutor_id, valda_valutor_kod, ~ fromJSON(content(GET(paste0("https://api.riksbank.se/swea/v1/ObservationAggregates/", .x, "/m/", start_datum, "/", till_datum)), as = "text", encoding = "utf-8"), flatten = TRUE) %>% as.data.frame() %>% mutate(valuta = .y))

# bearbeta datasetet
valutor_df <- tidsserie %>% 
  mutate(manad = format(from %>% as.Date(), "%B"),
         year = year %>% as.character(),
         seqNr = seqNr %>% as.numeric()) %>% 
  select(ar = year, manad, kurs = average, valuta)

return(valutor_df)

} # slut funktion


lista_valutor <- function(){
  
  valutalista <- fromJSON(content(GET(paste0("https://api.riksbank.se/swea/v1/Series/ExchangeRateSeries")), as = "text", encoding = "utf-8"), flatten = TRUE)
  
  return(valutalista)
}
