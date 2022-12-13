library(tidyverse)
library(ggnewscale)
library(ggspatial)
library(sf)

#load data

#outline of washington counties with shorelines
#downloaded from https://hub.arcgis.com/datasets/kingcounty::washington-counties-with-natural-shoreline-washsh-area/explore?location=-13.160727%2C-91.266482%2C1.23
wa_shorelines <- here::here("data","WA_Shorelines", "Washington_Counties_with_Natural_Shoreline___washsh_area.shp")
wa_shorelines <- read_sf(wa_shorelines) %>%
  st_transform(wa_counties, crs = 4269) #NAD83

#Shorezone shoreline shapefile
shoreline <- here("data","shorezone_shoreline_only", "shorezone_shoreline_only.shp")
shoreline <- read_sf(shoreline, crs = 2927) %>%  #Washington State Plane South (ft) / NAD83
  st_transform(crs = 4326)

#PSNERP PS basins outline
PSNERPbasins <- here("data","PSNERP_PS_basins", "psnerp_oceanographic_subbasins_geo.shp")
PSNERPbasins <- read_sf(PSNERPbasins) %>% st_transform(crs = 4326) #Washington State Plane South (ft) / NAD83

#NOAA PS basins outline
NOAAbasins <- here("data","NOAA_PS_basins", "noaa_ps_oceanographic_basins_geo.shp")
NOAAbasins <- read_sf(NOAAbasins) %>% st_transform(crs = 4326) #Washington State Plane South (ft) / NAD83

#Beach Strategies armoring shapefile
armor <- here("data","WDFW_ESRP_Shoreline_Armor.gdb")
armor <- read_sf(armor, crs = 2927) %>% #Washington State Plane South (ft) / NAD83
  st_transform(crs = 4326)

#GPS locations for our survey stations with each ipa
SOS_sites <- here("data", "SOS_site_coords.csv")
SOS_sites <- read_csv(SOS_sites) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) #WGS84

#transform site coordinates into the same datum as the shoreline layer
SOS_sites <- SOS_sites %>% st_transform(crs = st_crs(shoreline))

# Super basic map just showing outline of PS shorelines and armoring in red
basic_armor_map <- ggplot() +
  geom_sf(data = shoreline) +
  geom_sf(data = armor, color = "red", lwd = 0.75) +
  coord_sf(xlim = c(-123.42, -122), ylim = c(47, 49)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x  = element_blank(),
        axis.text.y  = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
basic_armor_map

#Zoom in to sites
seahurst <- basic_armor_map +
  geom_sf(data = armor, color = "red", lwd = 1) +
  coord_sf(xlim = c(-122.4, -122.35), ylim = c(47.45, 47.5))

edgewater <- basic_armor_map +
  geom_sf(data = armor, color = "red", lwd = 1) +
  coord_sf(xlim = c(-123, -122.5), ylim = c(47, 47.3))

#check out options for puget sound basins
basins_map <- ggplot() + 
  geom_sf(data = PSNERPbasins, aes(fill = SUBBASIN)) +
  geom_sf(data = shoreline) +
  geom_sf(data = NOAAbasins, fill = "transparent") +
  coord_sf(xlim = c(-123.5, -122), ylim = c(47, 48.75)) +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
basins_map
