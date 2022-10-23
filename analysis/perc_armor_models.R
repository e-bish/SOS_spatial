## Percent Armor Models

# Load libraries
library(tidyverse)
library(here)
library(lubridate)

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
  filter(org_type == "Fish") %>%
  group_by(year, month, jday, site, ipa, species) %>%
  summarise(total= sum(species_count)) %>% 
  ungroup( ) %>%
  mutate(month = as.numeric(month)) %>%
  mutate(total = as.numeric(total)) %>%
  mutate_if(is.character, as.factor) %>%
  merge(all, all=TRUE) %>%
  mutate(total=replace_na(total, 0)) 

#code for each site to add a year post- restoration. Here are the 2018 numbers: 

# COR' = 6, 'TUR'= 3,'FAM'= 3,'DOK'= 5,'EDG'= 2,'SHR'= 4



#other additions: vegetation (categorical or can we get %? and %armoring)
  # mutate(veg = recode(site, 'COR' = "Present",
  #                     'TUR'="Present",
  #                     'FAM'="Absent",
  #                     'DOK'="Absent",
  #                     'EDG'="Absent",
  #                     'SHR'="Present"))
#mutate(a_100m = recode(site, 'COR' = runif(1, 0, 30),
                         # 'TUR'=runif(1, 0, 30),
                         # 'FAM'=runif(1, 0, 30),
                         # 'DOK'=runif(1, 0, 30),
                         # 'EDG'=runif(1, 0, 30),
                         # 'SHR'=runif(1, 0, 30))) %>% 
# mutate(a_500m = runif(nrow(net_all), 0, 100)) %>% 
# mutate(a_1km = runif(nrow(net_all), 0, 100)) %>% 
# mutate(a_basin = runif(nrow(net_all), 0, 100))


# create a list of data for each species to run through the model
col.filters <- unique(net_all_core$species) 

lapply(seq_along(col.filters), function(x) {
  filter(net_all_core, species == col.filters[x])
}
) -> df_list

names(df_list) <- col.filters

################################################################################
##Chinook core sites

# Check simple model to get an estimate for theta
base.mod <- glm.nb(total ~ 1, data = net_all2, link = "log")
theta.1 <- base.mod$theta

#create model list
model.list <- list(
  glm.nb(total ~ site + veg + jday + I(jday^2), data = df_list[[10]], link = "log", control = glm.control(maxit = 500), init.theta = theta.1),
  glm.nb(total ~ site + veg + jday + I(jday^2) + a_100m, data = df_list[[10]], link = "log", control = glm.control(maxit = 500), init.theta = theta.1),
  glm.nb(total ~ site + veg + jday + I(jday^2) + a_500m, data = df_list[[10]], link = "log", control = glm.control(maxit = 500), init.theta = theta.1),
  glm.nb(total ~ site + veg + jday + I(jday^2) + a_1km, data = df_list[[10]], link = "log", control = glm.control(maxit = 500), init.theta = theta.1),
  glm.nb(total ~ site + veg + jday + I(jday^2) + a_basin, data = df_list[[10]], link = "log", control = glm.control(maxit = 500), init.theta = theta.1),
)

## create a vector of predictor variables
mod.terms <- c("null", "100m", "500m", "1km", "region")

#create a table to store model information and provide column names
mod.tab <- data.frame(nrow = 5)
names(mod.tab) <- mod.terms

# Define columns for storing values
mod.tab$a.weights <- mod.tab$dAICc <- mod.tab$AICc <- NA

for(i in 1:nrow(mod.tab)){
  # Calculate metrics
  mod.tab$form[i] <- paste0(mod.terms[i])
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
nbinom.mod <- model.list[[]]
summary(nbinom.mod)
