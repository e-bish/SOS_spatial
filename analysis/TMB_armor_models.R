library(here)
library(tidyverse)
require(TMB)

#load data
#chinook comes from perc_armor_models net_list[[1]]
chinook<- here::here("data","chinook.csv")
chinook <- read_csv(chinook) %>% 
  mutate(logyday = log(yday)) 

chinook <- na.omit(chinook)
b0 <- 0.4
b1 <- 1
b2 <- -0.25
X <- model.matrix(~ scale(logyday) + scale(X100m), data = chinook)

logmu <- X %*% c(b0, b1, b2)

fake.y <- rnbinom(nrow(X), mu = exp(logmu), size = 4)

### start with only two continuous variables and no random effects ####
chinook1.mm <- model.matrix(~ -1 + total + logyday + X100m, chinook) %>% 
  as_tibble() %>% 
  mutate(across(everything(), as.vector)) #strip attributes (otherwise TMB gets mad)


data <- list(y = fake.y,
             X = X)

#parameters <- list(b0 = 0, b1=0, b2=0, log_var=0) #variance must be >mu
parameters <- list(beta=  rep(0, 3), 
                   log_var=0) #variance must be >mu
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
#add parameters and random effect
chinook2.mm <- model.matrix(~ -1 + total + site + ipa + veg + rest_age + logyday + X100m, chinook) %>% 
  as_tibble() %>% 
  mutate(across(everything(), as.vector))

Z <- model.matrix( ~ -1 + site, data = chinook)
gamma <- rnorm(n = ncol(Z), mean = 0, sd = 1)
beta <- matrix(c(b0, b1, b2), nrow = 3, ncol =1)
logmu <- X %*% beta + Z %*% gamma
