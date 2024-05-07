# Loading packages
library(cowplot)
rm(list = ls())
libs <- c("rjson", "sf", "tidyverse", "ggspatial")
installed.libs <- libs %in% rownames(installed.packages())
if(any(installed.libs == FALSE)){
        install.packages(libs[!installed.libs])
}
invisible(lapply(libs, library, character.only = TRUE))

# Loading files 
crs = st_crs("+proj=longlat +datum=WGS84 +no_defs")
p <- st_read("C:/Rstudio/Rong_ARG/Rong-ARG/sfdata_v2/province.geojson") %>%
     st_as_sf() %>% 
     st_transform(crs = crs)

d <- st_read("C:/Rstudio/Rong_ARG/Rong-ARG/sfdata_v2/district.geojson") %>%
        st_as_sf() %>% 
        st_transform(crs = crs)
c <- st_read("C:/Rstudio/Rong_ARG/Rong-ARG/sfdata_v2/commune.geojson") %>%
        st_as_sf() %>% 
        st_transform(crs = crs)
# check data
glimpse(p)
glimpse(d)
glimpse(c)

# Preah Sihanouk module
ps.sf <- p %>%
        filter(HRName == "Preah Sihanouk")
ps.procode <- 18
ps.ps.d <- d %>% filter(PRO_CODE == 18) %>% 
        filter(DIS_CODE == 1801)
ps.RongARG <- c %>%
        filter(DIS_CODE == 1801) %>%
        filter(COM_CODE == 180105)
# Koh Kong
kohkong.p <- p %>% filter(HRName == "Koh Kong")
kirisakor.d <- d %>% 
     filter(PRO_CODE == 9) %>%
     filter(DIS_CODE == 902) # Kiri Sakor district
kohsdach.c <- c %>% 
     filter(PRO_CODE == 9) %>%
     filter(DIS_CODE == 902) %>%
     filter(COM_NAME == "Kaoh Sdach")

# define colors
line.col <- "black"
p.col = "grey98"
ps.p.col <- "grey95"
ps.d.col <- "grey90"
ps.c.col <- "grey80"
sea.col <- "#a7cdf2"
df <- data.frame(
     latitude = c(10.667289, 10.581441, 10.940079),
     longitude = c(103.273841, 103.306130, 103.079423),
     Households = c(17, 32, 19),
     location = c("Koh Touch", "Koh Rong Sanloem", "Koh Sdach")
)
# Convert to sf object with specified CRS (replace EPSG code with your desired one)
points_sf <- st_as_sf(df, coords = c("longitude", "latitude"), crs = crs)
points_sf
koh_touch <- st_point(c(10.667289, 103.273841))
koh_rong_sanleom <- 
     st_point(c(10.581441, 103.306130))
Koh_sdach <- st_point(c(10.940079, 103.079423))


ARg <- ggplot() +
        geom_sf(data = p, 
                fill = ps.p.col)+
        geom_sf(data = kohkong.p, 
                fill = ps.p.col)+
        geom_sf(data = kohsdach.c, 
                fill = ps.c.col) +
        geom_sf(data = ps.sf, 
                fill = ps.p.col) +
        geom_sf(data = ps.RongARG, 
                fill = ps.c.col) +
        coord_sf(xlim = c(103.0, 104.1), 
                 ylim = c(10.4, 11.1)) +
        geom_point(data = df,
                   aes(
                           x = longitude, 
                           y = latitude, 
                           # col = Households,
                           # size = Households, 
                           #col = Households
                   ),
                   stat = 'identity',
                   col = "red",
                   size = 3) +
        
        geom_text(aes(y = 10.581441, 
                       x = 103.15 ,
                       label = "Koh Rong Sanloem"),
                   size = 3) +
        geom_text(
                aes(
                        x = 103,
                        y = 10.98,
                        label = "Koh Sdach"
                ), size = 3
        ) + 
        geom_text(
                aes(
                        x = 103.4,
                        y = 10.667289,
                        label = "Koh Touch"
                ), size = 3
        ) +
        annotate(geom = "text", x = 103.25, y = 11,
                 hjust = 0, vjust = 1,
                 label = "Koh Kong", size = 3,
                 color = "black", 
                 fontface="italic")+
        annotate(geom = "text", x = 104.2, y =11.3 ,
                 hjust = 0, vjust = 1,
                 label = "Kompong Speu", size = 3
                 , color = "black", 
                 fontface="italic")+
        annotate(geom = "text", x =104.02 , y = 10.8,
                 hjust = 0, vjust = 0,
                 label = "Kampot", size = 3, 
                 color = "black", 
                 fontface="italic")+
        annotate(geom = "text", x =103.7 , y = 10.7,
                 hjust = 0, vjust = 0,
                 label = "Preah\nSihanouk", size = 3,
                 color = "black", 
                 fontface="italic") +
        annotate("segment",  
                 x =103.2738 + .01, 
                 y = 10.66729, 
                 xend = 103.4 - .045,
                 yend =10.667289,
                 size = 0.2, # Touch
                 colour = "black") +
        annotate('segment',
                 x = 103.30, 
                 y = 10.58144,
                 xend =  103.22, 
                 yend = 10.581441) +
        annotate('segment',
                 x = 103.07, 
                 y = 10.94008,
                 xend =  103 + .04, 
                 yend = 10.98) +
        
        annotation_scale(location = "bl", 
                         bar_cols = c("grey60", "white"), 
                         text_family = "ArcherPro Book") +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.margin = unit(c(0,0,0,0), "cm"),
              plot.margin = unit(c(0,0,0,0), "cm"),
              axis.title = element_blank(),
              axis.ticks.length = unit(-0.15, "cm"),
              panel.background = element_rect(fill = sea.col),#d0cfd4,#a2d2ff
              panel.border = element_rect( color = "grey20", 
                                           fill = NA, size = 1)) 
# Create insect map
insect_map <- ggplot() + 
        geom_sf(data = p,
        fill = p.col, 
        color = line.col)+
        geom_sf(data = ps.sf, 
                fill = ps.d.col) +
        geom_sf(data = ps.RongARG , 
                fill = ps.c.col, 
                col = line.col)+
        geom_sf(data = kohkong.p,
                fill = ps.d.col, 
                col = line.col) +
        geom_sf(data = kirisakor.d, 
                fill = ps.d.col, 
                col = line.col) +
        geom_sf(data = kohsdach.c, 
                fill = ps.c.col) + 
        theme_void()

insect_map
ggdraw() +
        coord_equal(xlim = c(0, 8), ylim = c(0, 8), expand = FALSE) +
        draw_plot(ARg, width = 8, height = 8, x = 0, y = 0)+
        draw_plot(insect_map, width = 2, height = 3, x = 6.08, y = 4.68)+
        theme(panel.background = element_rect(fill = "white"),
              panel.border = element_rect( color = "black", 
                                           fill = NA, size = 1)) +
        theme(
                panel.background = element_blank(),     
                panel.border = element_blank(),        
                plot.background = element_blank())
#===============================================================================



# Zooming province
ggplot() +
        geom_sf(data = p, 
                fill = ps.p.col)+
        geom_sf(data = kohkong.p, 
                fill = ps.p.col)+
        geom_sf(data = kohsdach.c, 
             fill = ps.c.col) +
        geom_sf(data = ps.sf, 
                fill = ps.p.col) +
        geom_sf(data = ps.RongARG, 
                fill = ps.c.col) +
        coord_sf(xlim = c(103.0, 104.2), 
                 ylim = c(10.4, 11.2)) + 
        annotate(geom = "text", x = 103.25, y = 11,
                 hjust = 0, vjust = 1,
                 label = "Koh Kong", size = 3,
                 color = "black", 
                 fontface="italic")+
        annotate(geom = "text", x = 104.2, y =11.3 ,
                 hjust = 0, vjust = 1,
                 label = "Kompong Speu", size = 3
                 , color = "black", 
                 fontface="italic")+
        annotate(geom = "text", x =104.02 , y = 10.8,
                 hjust = 0, vjust = 0,
                 label = "Kampot", size = 3, 
                 color = "black", 
                 fontface="italic")+
        annotate(geom = "text", x =103.7 , y = 10.7,
                 hjust = 0, vjust = 0,
                 label = "Preah\nSihanouk", size = 3,
                 color = "black", 
                 fontface="italic")+
        annotate(geom = "text",
                 x =104.3 , y = 10.5,
                 hjust = 0, vjust = 0,
                 label = "Kep", size = 3, 
                 color = "black", 
                 fontface="italic") +
        geom_label(aes(y = 10.581441, 
                       x = 103.15 ,
                       label = "Koh Rong Sanloem\n 32 (47%)"),
                   size = 3) +
        geom_label(
                aes(
                        x = 103,
                        y = 10.98,
                        label = "Koh Sdach \n19 (28%)"
                ), size = 3
        ) + 
        geom_label(
                aes(
                        x = 103.4,
                        y = 10.667289,
                        label = "Koh Touch \n17 (25%)"
                ), size = 3
        ) +
        geom_point(aes(
                x = 103.079423,
                y = 10.940079
        ), color = "red" # sdach point
        ) +
        geom_point(aes(
                x = 103.273841,
                y = 10.667289 
        ), color = "red") + # Touch point+
        geom_point(aes(
                x = 103.306130, 
                y = 10.581441
        ), col = "red") + 
        annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book") +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.margin = unit(c(0,0,0,0), "cm"),
              plot.margin = unit(c(0,0,0,0), "cm"),
              axis.title = element_blank(),
              axis.ticks.length = unit(-0.15, "cm"),
              legend.position = "none", 
              panel.background = element_rect(fill = sea.col),#d0cfd4,#a2d2ff
              panel.border = element_rect( color = "grey20", 
                                           fill = NA, size = 1))






