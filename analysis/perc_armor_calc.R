## Percent Armor Calculations

library(here)
library(tidyverse)
library(ggnewscale)
library(ggspatial)
library(sf)
library(lwgeom)
library(units)

## load data and inspect

#Shorezone shoreline shapefile
shoreline <- here("data","shorezone_shoreline_only", "shorezone_shoreline_only.shp") %>% 
  read_sf(crs = 2927) #Washington State Plane South (ft) / NAD83

#Beach Strategies armoring shapefile
armor <- here("data","WDFW_ESRP_Shoreline_Armor.gdb") %>% 
  read_sf(crs = 2927) #Washington State Plane South (ft) / NAD83

#GPS locations for our survey stations with each ipa
SOS_sites <- here("data", "SOS_site_coords.csv") %>% 
  read_csv() %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% #WGS84
  st_transform(crs = 2927) #transform site coordinates into the same datum as the shoreline layer

# map it!
# map <- ggplot() +
#   geom_sf(data = shoreline) +
#   geom_sf(data = armor, color = "red") +
#   geom_sf(data = SOS_sites, color = "blue", cex = 2) +
#   coord_sf(xlim = c(-123.5, -122), ylim = c(47, 48.75), crs = 4326) +
#   theme(plot.background = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())
# map

################################################################################
#create site buffers and crop armor extent to buffer

#set a center point for each site 
SOS_site_cents <- SOS_sites %>%
  group_by(site) %>% 
  summarize(geometry = st_union(geometry)) %>% 
  st_centroid()

#calculate buffers
b_100m <- st_buffer(SOS_site_cents, 328.084) #dorky m to ft conversion
b_500m <- st_buffer(SOS_site_cents, 1640.42)
b_1km <- st_buffer(SOS_site_cents, 3937.01) #3280.84ft in 1km, so this is actually 1.2km to capture the armoring at COR

#look at just one site to make sure everything is working  
Site <- SOS_sites %>% filter(site == "Seahurst") %>% st_buffer(5000)

#map it with buffers!
map.b <- ggplot() +
  geom_sf(data = st_intersection(shoreline, Site)) +
  geom_sf(data = st_intersection(armor, Site), color = "red") +
  geom_sf(data = st_intersection(SOS_site_cents, Site), color = "black") +
  geom_sf(data = st_intersection(b_100m, Site), fill = NA, color = "blue") +
  geom_sf(data = st_intersection(b_500m, Site), fill = NA, color = "yellow") +
  geom_sf(data = st_intersection(b_1km, Site), fill = NA, color = "green")
map.b

#crop shoreline to buffers
s_100m <- st_intersection(shoreline, b_100m) %>% mutate(buffer = "100m")
s_500m <- st_intersection(shoreline, b_500m) %>% mutate(buffer = "500m")
s_1km <- st_intersection(shoreline, b_1km) %>% mutate(buffer = "1.2km")

s_buffered <- rbind(s_100m, s_500m, s_1km) %>% 
  mutate(shore_length = st_length(geometry)) %>% #calculate length of shoreline within the buffer extent
  st_drop_geometry()  #remove geometry so we can just work with the numbers

#crop armor data to buffers
a_100m <- st_intersection(armor, b_100m) %>% mutate(buffer = "100m")
a_500m <- st_intersection(armor, b_500m) %>% mutate(buffer = "500m")
a_1km <- st_intersection(armor, b_1km) %>% mutate(buffer = "1.2km")

a_buffered <- rbind(a_100m, a_500m, a_1km) %>% 
  mutate(armor_length = st_length(SHAPE)) %>% #can't use the SHAPE_length attribute, because we cropped it!
  st_drop_geometry() %>% #remove geometry so we can just work with the numbers
  group_by(site, buffer) %>% 
  summarize(armor_length = sum(armor_length)) #sum feet of armoring per site

#calculate percent armor
site_armor <- inner_join(s_buffered, a_buffered, ID = c("site", "buffer")) %>% 
  mutate(perc.armor = (armor_length/shore_length)*100) %>% 
  dplyr::select(-c(OBJECTID, shore_length, armor_length)) %>% 
  mutate(perc.armor = drop_units(perc.armor)) %>% 
  mutate(perc.armor = round(perc.armor, 2)) 

################################################################################
#calculate % armor by basin

#PSNERP PS basins outline
PSNERPbasins <- here("data","PSNERP_PS_basins", "psnerp_oceanographic_subbasins_geo.shp")
PSNERPbasins <- read_sf(PSNERPbasins) %>% st_transform(crs = 2927) #Washington State Plane South (ft) / NAD83

#assign armor extent to basins
basin_armor <- st_intersection(armor, PSNERPbasins) %>% #this takes a long time
  group_by(SUBBASIN) %>% 
  summarize(armor_length = sum(SHAPE_Length)) %>% 
  st_drop_geometry()
  
#sum shoreline by basin
basin_shoreline <- st_intersection(shoreline, PSNERPbasins) %>% 
  mutate(shore_length = st_length(geometry)) %>% 
  group_by(SUBBASIN) %>% 
  summarize(shore_length = sum(shore_length)) %>% 
  st_drop_geometry()

#convert armor extent to percent armor by basin, assign to sites
a_basin <- inner_join(basin_armor, basin_shoreline, ID = c("SUBBASIN")) %>% 
  mutate(perc.armor = (armor_length/shore_length)*100) %>% 
  mutate(perc.armor = drop_units(perc.armor)) %>% 
  mutate(perc.armor = round(perc.armor, 2))

site_basins <- st_intersection(SOS_site_cents, PSNERPbasins) %>% 
  st_drop_geometry() %>% 
  add_row(site = "Cornet_Bay", SUBBASIN = "WH") #might be able to drop this with new coords, it just doesn't 
#currently overlap with the PSNERP basins layer

site_basin_armor <- inner_join(a_basin, site_basins, ID = c("SUBBASIN")) %>% 
  dplyr::select(-c(shore_length, armor_length)) %>% 
  relocate(site) %>% 
  rename("buffer"= "SUBBASIN")

###############################################################################
#format to integrate with catch data
perc_armor <- rbind(site_armor, site_basin_armor) %>%
  mutate(buffer = replace(buffer, !endsWith(buffer, "m"), "basin")) %>% 
  pivot_wider(names_from = buffer, values_from = perc.armor) %>% 
  replace(is.na(.), 0) %>% 
  transform(site = case_when(site == "Dockton" ~ "DOK",
                             site == "Cornet_Bay" ~ "COR",
                             site == "Edgewater" ~ "EDG",
                             site == "Family_Tides" ~ "FAM",
                             site == "Seahurst" ~ "SHR", 
                             site == "Turn_Island" ~ "TUR",
                             site == "Lost_Lake" ~ "LL",
                             site == "Titlow" ~ "TL",
                             #site == "Penrose_Point" ~ "PR",
                             #site == "Waterman" ~ "WA" ,
                             #site == "Howarth_Park" ~ "HO" ,
                             site == "Maylor_Point" ~ "MA")) %>% 
  add_row(site = c("PR", "WA", "HO")) #remove this when we get coords for these sites

#write to csv
#write_csv(perc_armor, here("data","perc_armor.csv"))

