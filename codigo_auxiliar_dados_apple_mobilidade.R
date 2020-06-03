library(readr)
library(tidyverse)
library(tidyr)
apple_mob <- read_csv("applemobilitytrends.csv")
apple_mob <- apple_mob %>% dplyr::filter(geo_type == "country/region") %>% dplyr::select(-geo_type) %>% 
  tidyr::pivot_longer(-c(region, transportation_type), names_to = "Data", values_to = "mobility") %>% 
  dplyr::select(Data, region, transportation_type, mobility)
