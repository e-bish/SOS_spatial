## Percent Armor Calculations

library(tidyverse)
library(ggnewscale)
library(ggspatial)
library(sf)
library(lwgeom)
library(units)

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
s_1km <- st_intersection(shoreline, b_1km) %>% mutate(buffer = "1.2km")

s_buffered <- rbind(s_100m, s_500m, s_1km) %>% 
  mutate(shore_length = st_length(geometry)) %>% #calculate length of shoreline within the buffer extent
  st_drop_geometry()  #remove geometry so we can just work with the numbers

#crop armor data to buffers
a_100m <- st_intersection(armor, b_100m) %>% mutate(buffer = "100m")
a_500m <- st_intersection(armor, b_500m) %>% mutate(buffer = "500m")
a_1km <- st_intersection(armor, b_1km) %>% mutate(buffer = "1.2km")

a_buffered <- rbind(a_100m, a_500m, a_1km) %>% 
  mutate(armor_length = st_length(SHAPE)) %>% #can't use the SHAPE_length attribute anymore, because we cropped it!
  st_drop_geometry() %>% #remove geometry so we can just work with the numbers
  group_by(site, buffer) %>% 
  summarize(armor_length = sum(armor_length)) #sum feet of armoring per site

#calculate percent armor
perc_armor <- inner_join(s_buffered, a_buffered, ID = c("site", "buffer")) %>% 
  mutate(perc.armor = (armor_length/shore_length)*100) 

#tidy for export
#create grid so zeros get counted in areas with zero armoring
site<- unique(SOS_sites$site)
buffer <- c("100m", "500m", "1.2km")
sites_buffers<- expand_grid(site, buffer) 

#format to integrate with catch data
perc_armor <- perc_armor %>% 
  merge(sites_buffers, all=TRUE) %>%
  transform(site = case_when(site == "Dockton" ~ "DOK" ,
                             site == "Cornet_Bay" ~ "COR",
                             site == "Edgewater" ~ "EDG",
                             site == "Family_Tides" ~ "FAM",
                             site == "Seahurst" ~ "SHR",
                             site == "Turn_Island" ~ "TUR",
                             site == "Lost_Lake" ~ "LL",
                             site == "Titlow" ~ "TL" ,
                             #site == "Penrose_Point" ~ "PR",
                             #site == "Waterman" ~ "WA" ,
                             #site == "Howarth_Park" ~ "HO" ,
                             site == "Maylor_Point" ~ "MA")) %>%
  mutate(perc.armor = drop_units(perc.armor)) %>% 
  mutate(perc.armor = round(perc.armor, 2)) %>% 
  mutate(perc.armor = replace_na(perc.armor, 0)) %>% 
  select(-c(OBJECTID, shore_length, armor_length))

#write to csv
write_csv(net_2021, here("data","perc_armor.csv"))
