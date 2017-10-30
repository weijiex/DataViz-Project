library(extrafont)
library(gpclib)
library(sp)
library(rgeos)
library(rgdal)
library(maptools)
library(ggplot2)
library(readxl)
library(ggmap)
library(tidyverse)
setwd('/Users/admin/Desktop/DV')

the_theme <- theme(plot.title = element_text(face="bold", size=15, family='Verdana'),
                   plot.subtitle = element_text(size=10, margin=margin(b=10)),
                   plot.caption = element_text(size=8, margin=margin(t=10)),
                   
                   axis.title = element_text(size=10),
                   axis.title.x = element_text(margin=margin(t=10)),
                   axis.title.y = element_text(margin=margin(r=10)),
                   axis.text = element_text(size=8),
                   axis.text.x = element_text(margin=margin(t=3)),
                   
                   plot.margin=unit(c(1,1,1,1),"cm"),
                   plot.background = element_rect(fill="#e9e9e9"),
                   
                   panel.background = element_rect(fill = "#e9e9e9"),
                   panel.grid.major.y=element_line(color='white', size=0.25),
                   panel.grid.minor.y=element_blank(),
                   panel.grid.major.x = element_line(color='white', size=0.25),
                   panel.grid.minor.x=element_blank(),
                   
                   legend.background = element_rect(fill="#e9e9e9"),
                   legend.key = element_blank(),
                   legend.title = element_text(family="Verdana", size=9),
                   legend.text = element_text(family="Verdana", size=8),
                   legend.position = "right",
                   
                   axis.ticks=element_blank())
default_color_palette = 'Set2'
default_discrete_color = c('#F08080', '#ADD8E6', '#FFFACD', '#90EE90', '#E6E6FA')
default_continuous_color = c('#DB7093', '#DDA0DD', '#FFC0CB', '#FFE4E1', '#FFF0F5')


## Geospatial Visulization - Plot 1##
#####################################
newyorkcity <- get_map(location = 'New York City', maptype="terrain", source='google')
hospitals <- read_excel("hospitalsnew.xlsx")

min_lat <- 40.57
max_lat <- 40.92
min_long <- -74.18
max_long <- -73.70

ggmap(newyorkcity) +
  geom_point(data = hospitals, aes(x = Longitude, y = Latitude, color=Borough), alpha=0.8, size=2) +
  scale_colour_brewer(palette = "Set1") +
  scale_x_continuous(limits=c(min_long, max_long)) +
  scale_y_continuous(limits=c(min_lat, max_lat)) +
  labs(title="NYC's Public Hospital System",
       subtitle="Examing the distribution of NYC Health and Hospitals Corporation(HHC),\nPublic Health System, which consists of 11 acute care hospitals, \n4 skilled nursing facilities, 6 large diagnostic and treatment centers \nand community-based clinics",
       caption="Source: Health and Hospitals Corporation (HHC) Facilities, NYC Open Data") +
  the_theme +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())


## Geospatial Visulization - Plot 2, with shapefile##
#####################################################
nyc <- read_excel("health_data.xlsx")
nyc_map <- readOGR(dsn="nycd_17c", layer="nycd")
nyc.points <- fortify(nyc_map, region="BoroCD")
nyc.df = merge(nyc.points, nyc, by.x="id", by.y="GEO_ID")

ggplot() + 
  geom_polygon(data=nyc.df, aes(long, lat, group=group, fill=alcho)) +
  geom_path(data = nyc.df, aes(x = long, y = lat, group = group), color = "white", size = 0.1, alpha=0.5) +
  coord_equal() +
  scale_fill_distiller(
    palette = "Spectral", 
    name = "number per 100,000 adults",
    guide = guide_colorbar(
      direction = "horizontal",
      barheight = unit(2, units = "mm"),
      barwidth = unit(50, units = "mm"),
      draw.ulim = F,
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = 0.5
    )) +
  the_theme +
  theme(plot.title = element_text(face="bold", size=15, family='Verdana',hjust=0.5),
        plot.subtitle = element_text(size=10, margin=margin(b=10),hjust=0.5),
        legend.position = "bottom",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.spacing = unit(0,"lines"),
        complete = TRUE) +
  labs(x = NULL, 
       y = NULL, 
       title = "2012 NYC Alcohol-Related Hospitalizations", 
       subtitle = "Examing the number and distribution of alcohol-related hospitalizations", 
       caption = "Source: NYC DOHMH Bureau of Alcohol and Drug Use")





