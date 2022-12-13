## Percent Armor Models

# Load libraries
library(tidyverse)
library(here)
library(lubridate)
library(MASS)
library(MuMIn)
library(lme4)

#set seed
set.seed(123)

################################################################################
#Load data
#load data (raw data downloaded/formatted in "import_data.R")
net_2018.19 <- here::here("data","net_18.19.csv")
net_2018.19 <- read_csv(net_2018.19)

net_2021 <- here::here("data","net_2021.csv")
net_2021 <- read_csv(net_2021)

net_2022 <- here::here("data","net_2022.csv")
net_2022 <- read_csv(net_2022)

#create a data frame with all four years of data combined and a term for calendar day of year
net_all <- bind_rows(net_2018.19, net_2021, net_2022) %>% 
  filter(!ipa == "Armored_2") %>% #remove second armored site from Titlow in 2021
  mutate(date = paste(day, month, year, sep="-")) %>% 
  mutate(date = dmy(date)) %>% 
  mutate(yday = yday(date)) 

perc_armor <- here("data", "perc_armor.csv")
perc_armor <- read_csv(perc_armor)

################################################################################
#prepare data for analysis
#expand grid to everywhere we sampled so that joint absences of species are included 
#create grid so zeros get counted for each species 
unique_df<- unique(net_all[c("year", "month", "yday","site","ipa")])
species <- c("Chinook", "Chum", "Surf Smelt", "Herring" )
all<- expand_grid(unique_df, species) %>% mutate(month = as.numeric(month))

net_all2 <- net_all %>% 
  filter(species %in% c("Chinook", "Chum", "Surf Smelt", "Herring"))%>%
  group_by(year, month, yday, site, ipa, species) %>%
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
                          'PR' = 2013,
                          'TL' = 2017,
                          'WA' = 2016)) %>% 
  mutate(rest_age = year - rest_yr) %>% 
  mutate(rest_age = ifelse(ipa != "Restored", 0, rest_age)) %>% 
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
                    'PR' = "Present", #need to actually look this up for all jubilee sites
                    'TL' = "Present", #We may not have good data for these ones
                    'WA' = "Absent")) %>% #need to actually look this up for all jubilee sites
  inner_join(perc_armor, ID = "site") 

#create a list of data for each of the focal species to run through the model
net_list <- net_all2 %>% group_split(species) 

################################################################################
##Chinook 
#chinook <- net_list[[1]]
#write_csv(chinook, here("data", "chinook.csv"))

#Check simple model to get an estimate for theta
base.mod <- glm.nb(total ~ 1, data = net_list[[1]], link = "log")
theta.1 <- base.mod$theta

#evaluate GLMs
null.mod <- glm.nb(total ~ ipa + veg + rest_age + yday + I(yday^2), data = net_list[[1]], link = "log", control = glm.control(maxit = 500))
a100.mod <- glm.nb(total ~ ipa + veg + rest_age + yday + I(yday^2) + scale(X100m), data = net_list[[1]], link = "log", control = glm.control(maxit = 500))
a500.mod <- glm.nb(total ~ ipa + veg + rest_age + yday + I(yday^2) + scale(X500m), data = net_list[[1]], link = "log", control = glm.control(maxit = 500))
a1km.mod <- glm.nb(total ~ ipa + veg + rest_age + yday + I(yday^2) + scale(X1.2km), data = net_list[[1]], link = "log", control = glm.control(maxit = 500))
#areg.mod <- glm.nb(total ~ ipa + veg + rest_age + yday + I(yday^2) + a_basin, data = net_list[[1]], link = "log", control = glm.control(maxit =500))

summary(null.mod)
summary(a100.mod)
summary(a500.mod)
summary(a1km.mod)

################################################################################
#simulate data for GLMMs

#start with values from the null model
glmm.null <- glmer.nb(total ~ (1 | site) + ipa + veg + rest_age + log(yday), data = net_list[[1]], control = glmerControl(optCtrl=list(maxfun=1e5)))
summary(glmm.null)
plot(glmm.null)

modsim <- function(spp, pars, size) {
  #recode site as a random variable drawn from a normal distribution
  site.num <- rnorm(n = length(unique(spp$site)), mean = 0, sd = 1) 
  site.eff <- as.numeric(as.character(factor(spp$site, labels = site.num)))
  
  #set intercept and parameter values
  b0 <- pars[1]
  b1 <- site.eff #random effect for site ~categorical
  b2 <- pars[2] #ipaNatural ~categorical
  b3 <- pars[3] #ipaRestored ~categorical
  b4 <- pars[4] #vegAbsent ~categorical
  b5 <- pars[5] #rest_age ~continuous
  b6 <- pars[6] #log(yday) ~continuous
  b7 <- pars[7] #%armor in 100m ~continuous
  ndat <- nrow(spp)
  
  #define the model equation
  logmu <- b0 + b1 + b2*ifelse(spp$ipa == "Natural", 1, 0) + b3*ifelse(spp$ipa == "Restored", 1, 0) + b4*ifelse(spp$veg == "Absent", 1, 0) + b5*spp$rest_age + b6*log(spp$yday) + b7*spp$X100m
  
  #generate dataset
  y <- rnbinom(n = ndat, mu = exp(logmu), size = size) #negative binomial distribution
  return(y)
}

#set "true" model parameters
parstest <- c(7.31, -0.07, -0.54, -1.72, 0.02, -1.14, -0.5) #added 0.1 to each from the null model, made up a parameter for %armor

#simulate fish counts and add to a dataframe with real data values
simY<-modsim(spp = net_list[[1]], pars = parstest, size = 10)
simdat<-data.frame(SimY = simY, net_list[[1]])

#run the model with simulated fish counts
sim.mod <- glmer.nb(SimY ~ (1 | site) + ipa + veg + rest_age + log(yday) + scale(X100m), data = simdat, control = glmerControl(optCtrl=list(maxfun=2e9)))
summary(sim.mod)
plot(sim.mod)

ggplot(simdat) + 
  geom_histogram(aes(x = SimY, color = "SimY")) + 
  #geom_histogram(aes(x = total, color = "total")) + 
  labs(x = "")

################################################################################

## GLMMs

null.glmm <- glmer.nb(total ~ (1 | site) + ipa + veg + rest_age + log(yday), data = net_list[[1]], control = glmerControl(optCtrl=list(maxfun=1e5)))
a100.glmm <- glmer.nb(total ~ (1 | site) + ipa + veg + rest_age + log(yday) + scale(X100m), data = net_list[[1]], control = glmerControl(optCtrl=list(maxfun=1e5)))
a500.glmm <- glmer.nb(total ~ (1 | site) + ipa + veg + rest_age + log(yday) + scale(X500m), data = net_list[[1]], control = glmerControl(optCtrl=list(maxfun=1e5)))
a1km.glmm <- glmer.nb(total ~ (1 | site) + ipa + veg + rest_age + log(yday) + scale(X1.2km), data = net_list[[1]], control = glmerControl(optCtrl=list(maxfun=1e5)))
#areg.glmm <- glmer.nb(total ~ (1 | site) + ipa + veg + rest_age + yday + scale(Xbasin), data = net_list[[1]], control = glmerControl(optCtrl=list(maxfun=1e5))))

summary(null.glmm)
summary(a100.glmm)
summary(a500.glmm)
summary(a1km.glmm)
