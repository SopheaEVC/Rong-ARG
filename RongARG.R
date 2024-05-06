# Loading packages
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
        
# define colors
line.col <- "black"
p.col = "white"
ps.p.col <- "grey95"
ps.d.col <- "grey90"
ps.c.col <- "grey80"
sea.col <- "#a7cdf2"



# Create insect map

ggplot() +
        geom_sf(data = p,
                  fill = p.col, 
                  color = line.col)+
        geom_sf(data = ps.sf, 
                fill = ps.d.col) +
        geom_sf(data = ps.RongARG , 
                  fill = ps.c.col, 
                  col = line.col)+
        theme_void()

# Zooming province
ggplot() +
        geom_sf(data = p, 
                fill = p.col)+
        geom_sf(data = ps.sf, 
                fill = ps.p.col) +
        geom_sf(data = ps.ps.d, 
                fill = ps.d.col, 
                size = 0.02) +
        geom_sf(data = ps.RongARG, 
                fill = ps.c.col) +
        coord_sf(xlim = c(103.0, 104.5), 
                 ylim = c(10.4, 11.5)) + 
        annotate(geom = "text", x = 103.4121, y = 11.5142,
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
        annotate(geom = "text", x =103.8062 , y = 10.86601,
                 hjust = 0, vjust = 0,
                 label = "Preah Sihanouk", size = 3, 
                 angle = 60, color = "black", 
                 fontface="italic")+
        annotate(geom = "text", x =104.3 , y = 10.5,
                 hjust = 0, vjust = 0,
                 label = "Kep", size = 3, 
                 color = "black", 
                 fontface="italic") +
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

# District Map
# 1. plot Preah Sihanouk map 
# 2. district boundaries
# 3. focus on commune

xmin = 103.2
xmax = 104.5
ymin = 10.4
ymax = 11.5

ggplot() +
        geom_sf(ps.ps.d, 
                fill = ps.p.col)+
        geom_sf(data = ps.sf, 
                fill = ps.p.col) +
        geom_sf(data = ps.ps.d, 
                fill = ps.d.col, 
                size = 0.02) +
        geom_sf(data = ps.RongARG, 
                fill = ps.c.col) +
        coord_sf(xlim = c(xmin, xmax), 
                 ylim = c(ymin, ymax)) 
        annotate(geom = "text", x = 103.18, y = 11,
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
                 angle = 60,
                 label = "Kampot", size = 3, 
                 color = "black", 
                 fontface="italic")+
        annotate(geom = "text", x =103.7 , y = 10.6,
                 hjust = 0, vjust = 0,
                 label = "Preah Sihanouk", size = 3, 
                 angle = 60, color = "black", 
                 fontface="italic")+
        annotate(geom = "text", x =104.3 , y = 10.5,
                 hjust = 0, vjust = 0,
                 label = "Kep", size = 3, 
                 color = "black", 
                 fontface="italic") +
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



