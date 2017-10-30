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
newyorkcity <- get_map(location = 'New York City', maptype="terrain", source='google')
hospitals <- read_excel("hospitalsnew.xlsx")

min_lat <- 40.5774
max_lat <- 40.9176
min_long <- -74.15
max_long <- -73.7004

ggmap(newyorkcity) +
  geom_point(data = hospitals, aes(x = Longitude, y = Latitude, color=Borough), alpha=0.8, size=2) +
  scale_colour_brewer(palette = "Set1") +
  scale_x_continuous(limits=c(min_long, max_long)) +
  scale_y_continuous(limits=c(min_lat, max_lat)) +
  labs(title="NYC's Public Hospital System",
       subtitle="Examing the distribution of NYC Health and Hospitals Corporation(HHC),\nPublic Health System, which consists of 11 acute care hospitals, \n4 skilled nursing facilities, 6 large diagnostic and treatment centers \nand community-based clinics",
       caption="NYC Open Data") +
  the_theme


## Geospatial Visulization - Plot 2, with shapefile##
nyc <- read_excel("health_data.xlsx")
nyc_map <- readOGR(dsn="nycd_17c", layer="nycd")
nyc.points <- fortify(nyc_map, region="BoroCD")
nyc.df = merge(nyc.points, nyc, by.x="id", by.y="GEO_ID")

ggplot(data=nyc.df, aes(long, lat, group=group, fill=alcho)) + 
  geom_polygon()







