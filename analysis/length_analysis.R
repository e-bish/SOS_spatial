library(tidyverse)
library(here)

raw.18.19 <- here("data", "raw_import", "raw.18.19.csv") %>% 
  read_csv() %>% 
  separate(date, into = c("year","month", "day"), sep = "-") %>%
  mutate(month = str_pad(month, 2, side = c("left"), pad = "0")) %>%
  mutate(day = str_pad(day, 2, side = c("left"), pad = "0")) %>%
  mutate(tax_group = replace(tax_group, species == "Tube Snout", "Aulorhynchus")) %>% 
  mutate(species = replace(species, species == "Gunnel Sp", "UnID Gunnel")) %>% 
  mutate(species = replace(species, species == "Jellyfish Sp", "UnID Jellyfish")) %>% 
  select(-c(tide, observer, water_level, secchi_m)) %>% 
  mutate(hatchery_wild = NA) %>% 
  rename(depth_ft = depth_m) #this seems like its really in ft??

raw.21 <- here("data", "raw_import", "raw.21.csv") %>% read_csv()

raw.22 <- here("data", "raw_import", "raw.22.csv") %>% read_csv()

all_df <- rbind(raw.18.19, raw.21, raw.22) %>% 
  filter(length_mm > 0, na.rm = TRUE) %>% 
  filter(org_type == "Fish")

#how many >100mm?
#all %>% filter(length_mm > 100)
#1442

plot1 <- all_df %>% 
  ggplot(aes(x = depth_ft, y = length_mm, fill = factor(station))) + 
  geom_point() +
  theme_classic() 
plot1


plot2 <- all_df %>% 
  filter(!ipa %in% "Armored_2") %>% 
  ggplot(aes(x = factor(station), y = length_mm, fill = factor(ipa))) +
  geom_boxplot() +
  theme_classic()
plot2


#plot by depth and station --- at armored sites they don't care about shallow water habitat??

#length bins ~ small fish respond differently??

all_df2 <- all_df %>% 
  mutate(depth_bin = case_when(length_mm < 100 ~ "small",
                               length_mm > 100 & length_mm < 200 ~ "med",
                               length_mm > 200 ~ "large",
                               TRUE ~ "NA"))

plot3 <- all_df2 %>% 
  ggplot(aes(x = depth_ft, y = length_mm, fill = factor(depth_bin))) + 
  geom_point() +
  theme_classic() 
plot3
