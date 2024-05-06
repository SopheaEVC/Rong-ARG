library(sf)
library(tidyverse)
library(readxl)

# Loading dataset
fci <- st_read("C:/Rstudio/Rong_ARG/Rong-ARG/sfdata_v2/fci_en.gpkg")
fci_tbl <- read_excel("C:/Rstudio/Rong_ARG/Rong-ARG/fci.xlsx")

names(fci)
glimpse(fci)


fci %>% 
     filter(region == "Marine fishery") %>%
     select(province, district, commune, size_ha) %>%
     as_data_frame() %>%
     arrange(desc(size_ha)) %>%
     view()
fci %>% 
     filter(region == "Marine fishery") %>%
     filter(province == "Preah Sihanouk") %>%
     select(province, district, commune, size_ha) %>%
     arrange(desc(size_ha)) %>%
     as_tibble() %>%
     filter(district != "Not found")
