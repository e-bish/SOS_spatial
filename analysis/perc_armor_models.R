## Percent Armor Models

# Load libraries
library(tidyverse)
library(here)
library(lubridate)
library(MASS)
library(MuMIn)

################################################################################
#Load data
#load data (raw data downloaded/formatted in "import_data.R")
net_2018.19 <- here::here("data","net_18.19.csv")
net_2018.19 <- read_csv(net_2018.19)

net_2021 <- here::here("data","net_2021.csv")
net_2021 <- read_csv(net_2021)

net_2022 <- here::here("data","net_2022.csv")
net_2022 <- read_csv(net_2022)

#create a data frame with all four years of data combined
net_all <- bind_rows(net_2018.19, net_2021, net_2022) %>% 
  mutate(date = paste(day, month, year,sep="-")) %>% 
  mutate(date = dmy(date)) %>% 
  mutate(jday = yday(date)) 

################################################################################
#prepare data for analysis
#expand grid to everywhere we sampled so that joint absences of species are included 
#create grid so zeros get counted for each species 
unique_df<- unique(net_all[c("year", "month", "jday","site","ipa")])
species <- c("Chinook", "Chum", "Surf Smelt", "Herring" )
all<- expand_grid(unique_df, species) %>% mutate(month = as.numeric(month))

net_all2 <- net_all %>% 
  filter(species %in% c("Chinook", "Chum", "Surf Smelt", "Herring"))%>%
  group_by(year, month, jday, site, ipa, species) %>%
  summarise(total= sum(species_count)) %>% 
  ungroup( ) %>%
  mutate(month = as.numeric(month)) %>%
  mutate(total = as.numeric(total)) %>%
  mutate_if(is.character, as.factor) %>%
  merge(all, all=TRUE) %>%
  mutate(total=replace_na(total, 0)) %>% 
  mutate(rest_yr = recode(site, #restoration years from shoremonitoring.org
                          'COR' = 2012, #multiple phases carried out between 2012 and 2015
                          'TUR' = 2015, 
                          'FAM' = 2015, 
                          'DOK' = 2013, 
                          'EDG' = 2016, 
                          'SHR' = 2014, 
                          'HO' = 2016, 
                          'LL' = 2018,
                          'MA' = 2018, 
                          'PP' = 2013,
                          'TL' = 2017,
                          'WA' = 2016)) %>% 
  mutate(rest_age = year - rest_yr) %>% 
  mutate(veg = recode(site, #can we get continuous %eelgrass data?
                    'COR' = "Present",
                    'TUR'="Present",
                    'FAM'="Absent",
                    'DOK'="Absent",
                    'EDG'="Absent",
                    'SHR'="Present",
                    'HO' = "Present", 
                    'LL' = "Absent", 
                    'MA' = "Absent",
                    'PP' = "Present", #need to actually look this up for all jubilee sites
                    'TL' = "Present", #We may not have good data for these ones
                    'WA' = "Absent")) %>% #need to actually look this up for all jubilee sites
  mutate(a_100m = recode(site, #simulate percent armor data at 100m radius
                      'COR' = runif(1, 0, 100), 
                      'TUR'=runif(1, 0, 100),
                      'FAM'=runif(1, 0, 100),
                      'DOK'=runif(1, 0, 100),
                      'EDG'=runif(1, 0, 100),
                      'SHR'=runif(1, 0, 100),
                      'HO' = runif(1, 0, 100), 
                      'LL' = runif(1, 0, 100),
                      'MA' = runif(1, 0, 100), 
                      'PP' = runif(1, 0, 100),
                      'TL' = runif(1, 0, 100),
                      'WA' = runif(1, 0, 100))) %>% 
  mutate(a_500m = recode(site, #simulate percent armor data at 500m radius
                         'COR' = runif(1, 0, 100), 
                         'TUR'=runif(1, 0, 100),
                         'FAM'=runif(1, 0, 100),
                         'DOK'=runif(1, 0, 100),
                         'EDG'=runif(1, 0, 100),
                         'SHR'=runif(1, 0, 100),
                         'HO' = runif(1, 0, 100), 
                         'LL' = runif(1, 0, 100),
                         'MA' = runif(1, 0, 100), 
                         'PP' = runif(1, 0, 100),
                         'TL' = runif(1, 0, 100),
                         'WA' = runif(1, 0, 100))) %>% 
  mutate(a_1km = recode(site, #simulate percent armor data at 1km radius
                        'COR' = runif(1, 0, 100), 
                        'TUR'=runif(1, 0, 100),
                        'FAM'=runif(1, 0, 100),
                        'DOK'=runif(1, 0, 100),
                        'EDG'=runif(1, 0, 100),
                        'SHR'=runif(1, 0, 100),
                        'HO' = runif(1, 0, 100), 
                        'LL' = runif(1, 0, 100),
                        'MA' = runif(1, 0, 100), 
                        'PP' = runif(1, 0, 100),
                        'TL' = runif(1, 0, 100),
                        'WA' = runif(1, 0, 100))) %>% 
  mutate(a_basin = recode(site, #simulate percent armor data at basin scale
                          'COR' = runif(1, 0, 30), 
                          'TUR'=runif(1, 0, 30),
                          'FAM'=runif(1, 0, 30),
                          'DOK'=runif(1, 0, 30),
                          'EDG'=runif(1, 0, 30),
                          'SHR'=runif(1, 0, 30),
                          'HO' = runif(1, 0, 30), 
                          'LL' = runif(1, 0, 30),
                          'MA' = runif(1, 0, 30), 
                          'PP' = runif(1, 0, 30),
                          'TL' = runif(1, 0, 30),
                          'WA' = runif(1, 0, 30)))

#create a list of data for each of the focal species to run through the model
net_list <- net_all2 %>% group_split(species) 

################################################################################
##Chinook 

#Check simple model to get an estimate for theta
base.mod <- glm.nb(total ~ 1, data = net_list[[1]], link = "log")
theta.1 <- base.mod$theta

#create model list
model.list <- list(
  glm.nb(total ~ site + veg + rest_age + jday + I(jday^2), data = net_list[[1]], link = "log", control = glm.control(maxit = 500), init.theta = theta.1),
  glm.nb(total ~ site + veg + rest_age + jday + I(jday^2) + a_100m, data = net_list[[1]], link = "log", control = glm.control(maxit = 500), init.theta = theta.1),
  glm.nb(total ~ site + veg + rest_age + jday + I(jday^2) + a_500m, data = net_list[[1]], link = "log", control = glm.control(maxit = 500), init.theta = theta.1),
  glm.nb(total ~ site + veg + rest_age + jday + I(jday^2) + a_1km, data = net_list[[1]], link = "log", control = glm.control(maxit = 500), init.theta = theta.1),
  glm.nb(total ~ site + veg + rest_age + jday + I(jday^2) + a_basin, data = net_list[[1]], link = "log", control = glm.control(maxit = 500), init.theta = theta.1)
)

## create a vector of predictor variables
mod.terms <- c("null", "100m", "500m", "1km", "basin")

#create a table to store model information and provide column names
mod.tab <- as.data.frame(mod.terms)

# Define columns for storing values
mod.tab$a.weights <- mod.tab$dAICc <- mod.tab$AICc <- NA

for(i in 1:nrow(mod.tab)){
  # Calculate metrics
  mod.tab$AICc[i] <- round(AICc(model.list[[i]]),2)
}

# Calculate delta AIC
mod.tab$dAICc <- mod.tab$AICc - min(mod.tab$AICc)

# Use the delta AIC to calculate the difference
weights_raw <- exp(-0.5*mod.tab$dAICc)

# normalise (i.e. divide by the summed value) for the akaike weights
mod.tab$a.weights <- round(weights_raw/sum(weights_raw),4)

#order table by akaike weights
mod.tab <- mod.tab[order(-mod.tab$a.weights),]

##Extract the best fit mod
# nbinom.mod <- model.list[[]]
# summary(nbinom.mod)
