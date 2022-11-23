library(here)
library(tidyverse)
require(TMB)

#load data
#chinook comes from perc_armor_models net_list[[1]]
chinook<- here::here("data","chinook.csv")
chinook <- read_csv(chinook) %>% 
  mutate(logyday = log(yday)) %>% 
  dplyr::select(-c(year, month, yday, species, rest_yr, X500m, X1.2km))

### start with only two continuous variables and no random effects ####
chinook.mm <- model.matrix(~ -1 + total + logyday + X100m, chinook) %>% 
  as_tibble() %>% 
  transform(total = c(total), #strip attributes (otherwise TMB gets mad)
            logyday = c(logyday),
            X100m = c(X100m))

data <- list(y = chinook.mm$total,
             x = chinook.mm$logyday,
             z = chinook.mm$X100m)

parameters <- list(b0=5, b1=1, b2=1, log_var=log(30)) #variance must be >mu

compile("tmb/amod.cpp") #zero means it worked
dyn.load(dynlib("tmb/amod"))

model <- MakeADFun(data, parameters, DLL="amod",hessian=T)

#Fit model
fit <- nlminb(model$par, model$fn, model$gr)

#Extract parameter estimates
best <- model$env$last.par.best
print(best)
rep <- sdreport(model)
print(summary(rep))

