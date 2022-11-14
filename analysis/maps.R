library(tidyverse)
library(ggnewscale)
library(ggspatial)
library(sf)

#load data

#outline of washington counties with shorelines
#downloaded from https://hub.arcgis.com/datasets/kingcounty::washington-counties-with-natural-shoreline-washsh-area/explore?location=-13.160727%2C-91.266482%2C1.23
# wa_shorelines <- here::here("data","WA_Shorelines", "Washington_Counties_with_Natural_Shoreline___washsh_area.shp")
# wa_shorelines <- read_sf(wa_shorelines) %>% 
#   st_transform(wa_counties, crs = 4269) #NAD83

#Shorezone shoreline shapefile
shoreline <- here("data","shorezone_shoreline_only", "shorezone_shoreline_only.shp")
shoreline <- read_sf(shoreline, crs = 2927) #Washington State Plane South (ft) / NAD83

#Beach Strategies armoring shapefile
armor <- here("data","WDFW_ESRP_Shoreline_Armor.gdb")
armor <- read_sf(armor, crs = 2927) #Washington State Plane South (ft) / NAD83

#GPS locations for our survey stations with each ipa
SOS_sites <- here("data", "SOS_site_coords.csv")
SOS_sites <- read_csv(SOS_sites) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) #WGS84

#transform site coordinates into the same datum as the shoreline layer
SOS_sites <- SOS_sites %>% st_transform(crs = st_crs(shoreline))

# map it!
map <- ggplot() +
  geom_sf(data = wa_shorelines, fill = "azure3") +
  geom_sf(data = SOW2016, aes(color = factor(ModArmCD)) , lwd = 1) +
  scale_color_manual(values = c("green", "yellow", "red"),
                     name = "Shoreline Condition",
                     labels = c("Not Modified", "Modified: Not Armored",
                                "Armored", "Survey Sites")) +
  geom_sf(data = SOS_sites, color = "black", cex = 2) +
  geom_sf_text(data = SOS_sites, aes(label = Station_code), 
               nudge_x = c(rep(0.1, 7), -0.1, rep(0.1, 4)), 
               nudge_y = c(rep(-0.05, 7), 0.05, rep(-0.05, 3), 0.05)) +
  coord_sf(xlim = c(-123.42, -122), ylim = c(47, 49)) +
  theme_bw() +
  theme(panel.background =   element_rect(fill = "azure"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x  = element_text(angle = 45, vjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  annotation_scale(location = "bl", style = "ticks", width_hint = 0.5) +
  annotation_north_arrow(location = "tl", which_north = "true",
                         pad_x = unit(0, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_minimal)
map