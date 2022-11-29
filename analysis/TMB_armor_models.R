library(here)
library(tidyverse)
require(TMB)

#load data
#chinook comes from perc_armor_models net_list[[1]]
chinook<- here::here("data","chinook.csv")
chinook <- read_csv(chinook) %>% 
  mutate(logyday = log(yday)) %>% 
  na.omit(chinook)

b0 <- 0.4
b1 <- 1
b2 <- -0.25
X <- model.matrix(~ scale(logyday) + scale(X100m), data = chinook)

logmu <- X %*% c(b0, b1, b2)

#simulate fish counts
fake.y <- rnbinom(nrow(X), mu = exp(logmu), size = 4)

################################################################################
######## start with only two continuous variables and no random effects ########
data <- list(y = fake.y,
             X = X)

parameters <- list(beta = rep(0,3), 
                   log_var = 0) 

compile("tmb/amod.cpp") #zero means it worked
dyn.load(dynlib("tmb/amod"))

model <- MakeADFun(data, parameters, DLL="amod", hessian=T)

#Fit model
fit <- nlminb(model$par, model$fn, model$gr)

#Extract parameter estimates
best <- model$env$last.par.best
print(best)
rep <- sdreport(model)
print(summary(rep))

################################################################################
######################### add random effect ####################################

# create model matrix for the random effect (site)
Z <- model.matrix( ~ -1 + site, data = chinook)

# mean of site intercepts
gamma <- rnorm(n = ncol(Z), mean = 0, sd = 1)

#beta has to be a matrix
beta <- matrix(c(b0, b1, b2), nrow = 3, ncol =1)

#matrix algebra!
logmu <- X %*% beta + Z %*% gamma

#simulate fish counts
fake.y <- rnbinom(nrow(X), mu = exp(logmu), size = 4)

data <- list(y = fake.y,
             X = X,
             Z = Z)

parameters <- list(beta = rep(0,3), 
                   gamma = rep(0, ncol(Z)),
                   log_var = 0) 

compile("tmb/amod_ran.cpp") 
dyn.load(dynlib("tmb/amod_ran"))

model_ran <- MakeADFun(data, parameters, random = "gamma", DLL="amod_ran", hessian=T)

#Fit model
fit_ran <- nlminb(model_ran$par, model_ran$fn, model_ran$gr)

#Extract parameter estimates
best_ran <- model_ran$env$last.par.best
print(best_ran)
rep_ran <- sdreport(model_ran)
print(summary(rep))




