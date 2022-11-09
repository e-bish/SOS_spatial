## Percent Armor Calculations

library(tidyverse)
library(ggnewscale)
library(ggspatial)
library(sf)
library(lwgeom)

## load data and inspect

#Shorezone shoreline shapefile
shoreline <- here::here("data","shorezone_shoreline_only", "shorezone_shoreline_only.shp")
shoreline <- read_sf(shoreline, crs = 2927) #Washington State Plane South (ft) / NAD83

#Beach Strategies armoring shapefile
armor <- here::here("data","WDFW_ESRP_Shoreline_Armor.gdb")
armor <- read_sf(armor, crs = 2927) #Washington State Plane South (ft) / NAD83

#GPS locations for our survey stations with each ipa
SOS_sites <- here::here("data", "SOS_site_coords.csv")
SOS_sites <- read_csv(SOS_sites) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) #WGS84

#transform site coordinates into the same datum as the shoreline layer
SOS_sites <- SOS_sites %>% st_transform(crs = st_crs(shoreline))

# map it!
map <- ggplot() +
  geom_sf(data = shoreline) +
  geom_sf(data = armor, color = "red") +
  geom_sf(data = SOS_sites, color = "blue", cex = 2) 
map

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
Site <- SOS_sites %>% filter(site == "Dockton") %>% st_buffer(2500)

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
s_1km <- st_intersection(shoreline, b_1km) %>% mutate(buffer = "1km")

s_buffered <- rbind(s_100m, s_500m, s_1km) %>% 
  mutate(shore_length = st_length(geometry)) %>% 
  st_drop_geometry()

#crop armor data to buffers
a_100m <- st_intersection(armor, b_100m) %>% mutate(buffer = "100m")
a_500m <- st_intersection(armor, b_500m) %>% mutate(buffer = "500m")
a_1km <- st_intersection(armor, b_1km) %>% mutate(buffer = "1km")

a_buffered <- rbind(a_100m, a_500m, a_1km) %>% 
  mutate(armor_length.c = st_length(SHAPE)) %>%
  st_drop_geometry()

#feet of armoring per site buffer extent
a_buffered2 <- a_buffered %>% 
  group_by(site, buffer) %>% 
  summarize(armor_length = sum(SHAPE_Length)) 

a_buffered3 <- a_buffered %>% 
  group_by(site, buffer) %>% 
  summarize(armor_length.c = sum(armor_length.c)) 

a_buffered <- inner_join(a_buffered2, a_buffered3, ID = c("site, buffer"))

#calculate percent armor
perc_armor <- inner_join(s_buffered, a_buffered, ID = c("site", "buffer")) %>% 
  mutate(perc_armor = (armor_length/shore_length)*100) %>% 
  mutate(perc_armor.c = (armor_length.c/shore_length)*100) 


ggplot() +
  geom_sf(data = st_intersection(shoreline, Site)) +
  geom_sf(data = st_intersection(armor, Site), color = "red") +
  geom_sf(data = st_intersection(SOS_site_cents, Site), color = "black") +
  geom_sf(data = st_intersection(b_100m, Site), fill = NA, color = "blue") +
  #geom_sf(data = a_buffered[1,], color = "green") + 
  #geom_sf(data = a_buffered[2,], color = "purple") +
  geom_sf(data = s_buffered[1,], color = "orange")  
  

