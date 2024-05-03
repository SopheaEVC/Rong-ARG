# Loading packages
library(rjson)
library(sf)
library(tidyverse)

# Loading files 
crs = st_crs("+proj=longlat +datum=WGS84 +no_defs")
n <- st_read("C:/Rstudio/Rong_ARG/Rong-ARG/sf_data/geoBoundaries-KHM-ADM0.geojson") %>%
     st_as_sf() %>% 
     st_transform(crs = crs)
p <- st_read("C:/Rstudio/Rong_ARG/Rong-ARG/sf_data/geoBoundaries-KHM-ADM1.geojson") %>%
     st_as_sf() %>% 
     st_transform(crs = crs)
ps.sf <- p %>%
     filter(shapeName == "Preah Sihanouk")
d <- st_read("C:/Rstudio/Rong_ARG/Rong-ARG/sf_data/geoBoundaries-KHM-ADM2.geojson") %>%
     st_as_sf() %>% 
     st_transform(crs = crs) 
     
c <- st_read("C:/Rstudio/Rong_ARG/Rong-ARG/sf_data/geoBoundaries-KHM-ADM3.geojson") %>%
     st_as_sf() %>% 
     st_transform(crs = crs)

line.color <- "black"
study_col <- "grey"
ps.district 


# Create Insect Map
get_cam_map <- function(){
     cam_map <- ggplot() +
          geom_sf(data = n,
                  fill = 'lightgrey', 
                  color = line.color) +
          geom_sf(data = p, 
                  fill = 'lightgrey') +
          geom_sf(data = ps.sf, 
                  fill = study_col, 
                  col = line.color)+
          theme_void()
     return(cam_map)
}
get_cam_map()


