library(here)
library(tidyverse)
require(TMB)

#load data
#chinook comes from perc_armor_models net_list[[1]]
chinook<- here::here("data","chinook.csv")
chinook <- read_csv(chinook) %>% 
  mutate(logyday = log(yday)) %>% 
  na.omit(chinook)

#set parameters to simulate data and create data matrix
set.seed(1234)
b0 <- 0.4 #intercept
b1 <- 1 #yday
b2 <- -0.25 #% armor
beta <- matrix(c(b0, b1, b2), nrow = 3, ncol = 1)
X <- model.matrix(~ scale(logyday) + scale(X100m), data = chinook)

################################################################################
######## start with only two continuous variables and no random effects ########

# #matrix algebra!
# logmu <- X %*% beta
# 
# #simulate fish counts
# fake.y <- rnbinom(nrow(X), mu = exp(logmu), size = 4)
# 
# #gather data
# data <- list(y = fake.y,
#              X = X)
# 
# #set initial values for parameters
# parameters <- list(beta = rep(0,3),
#                    log_var = 0)
# 
# #call the model
# compile("tmb/amod.cpp") #zero means it worked
# dyn.load(dynlib("tmb/amod"))
# 
# model <- MakeADFun(data, parameters, DLL="amod", hessian=T)
# 
# #Fit model
# fit <- nlminb(model$par, model$fn, model$gr)
# 
# #Extract parameter estimates
# best <- model$env$last.par.best
# print(best)
# rep <- sdreport(model)
# print(summary(rep))
# 
# ################################################################################
# ######################### add random effect ####################################
# 
# #create model matrix for the random effect (site)
# Z <- model.matrix( ~ -1 + site, data = chinook)
# 
# #means of site intercepts
# gamma <- rnorm(n = ncol(Z), mean = 0, sd = 1)
# 
# #matrix algebra!
# logmu <- X %*% beta + Z %*% gamma #fixed effects + random effect
# 
# #simulate fish counts with a random site effect
# fake.y <- rnbinom(nrow(X), mu = exp(logmu), size = 4)
# 
# #gather data
# data <- list(y = fake.y,
#              X = X,
#              Z = Z)
# 
# #set initial values for parameters
# parameters <- list(beta = rep(0, times = nrow(beta)), 
#                    gamma = rep(0, times = ncol(Z)),
#                    log_var = 0) 
# 
# #call the model
# compile("tmb/amod_ran.cpp") 
# dyn.load(dynlib("tmb/amod_ran"))
# 
# model_ran <- MakeADFun(data, parameters, random = "gamma", DLL="amod_ran", hessian=T)
# 
# #Fit model
# fit_ran <- nlminb(model_ran$par, model_ran$fn, model_ran$gr)
# 
# #Extract parameter estimates
# best_ran <- model_ran$env$last.par.best
# print(best_ran)
# rep_ran <- sdreport(model_ran)
# print(summary(rep_ran))
# 
# ################################################################################
# ###################### add other fixed effects #################################
# b3 <- 0.25 #ipaNatural
# b4 <- 0.1 #ipaRestored
# b5 <- 0.3 #VegPresent
# beta <- matrix(c(b0, b1, b2, b3, b4, b5), nrow = 6, ncol = 1)
# X2 <- model.matrix(~ scale(logyday) + scale(X100m) + ipa + veg, data = chinook)
# 
# #create model matrix for the random effect (site)
# Z <- model.matrix( ~ -1 + site, data = chinook)
# 
# #means of site intercepts
# gamma <- rnorm(n = ncol(Z), mean = 0, sd = 1)
# 
# #matrix algebra!
# logmu <- X2 %*% beta + Z %*% gamma #fixed effects + random effect
# 
# #simulate fish counts with a random site effect
# fake.y <- rnbinom(nrow(X2), mu = exp(logmu), size = 4)
# 
# #gather data
# data <- list(y = fake.y,
#              X = X2,
#              Z = Z)
# 
# #set initial values for parameters
# parameters <- list(beta = rep(0, times = nrow(beta)),
#                    gamma = rep(0, times = ncol(Z)),
#                    log_var = 0)
# 
# #call the model
# compile("tmb/amod_ran.cpp")
# dyn.load(dynlib("tmb/amod_ran"))
# 
# model_fullran <- MakeADFun(data, parameters, random = "gamma", DLL="amod_ran", hessian=T)
# 
# #Fit model
# fit_fullran <- nlminb(model_fullran$par, model_fullran$fn, model_fullran$gr)
# 
# #Extract parameter estimates
# best_fullran <- model_fullran$env$last.par.best
# print(best_fullran)
# rep_fullran <- sdreport(model_fullran)
# print(summary(rep_fullran))

################################################################################
##################### add fancy code for restoration age #######################
b3 <- 0.25 #vegPresent
beta <-c(b0, b1, b2, b3) 

#create model matrix for regular fixed effects
X2 <- model.matrix(~ scale(logyday) + scale(X100m) + veg, data = chinook)

#create model matrix for the random effect (site)
Z <- model.matrix( ~ -1 + site, data = chinook)

#means of site intercepts
gamma <- rnorm(n = ncol(Z), mean = 0, sd = 1)

#create model matrix for the ipa
L <- model.matrix(~ -1 + ipa, data = chinook)

#simulate effect of restoration age
lambda_nat <- 0 #natural
lambda_arm <- -1 #armored
logb <- 0.5
a <- chinook$rest_age
lambda_rest <- lambda_arm*exp(-logb*a) #restored

lambda <- c(lambda_nat, lambda_arm)

# plot <- data.frame(a, l_rest)
# ggplot(plot, aes(x = a, y = l_rest)) +
#   geom_point()

#matrix algebra!
logmu <- X2 %*% beta + Z %*% gamma + L[,1:2] %*% lambda + L[,3] * lambda_rest #fixed effects + random effect + fancy code

#simulate fish counts with a random site effect and effect of time since restoration
fake.y <- rnbinom(nrow(X2), mu = exp(logmu), size = 4)

#gather data
data <- list(y = fake.y,
             X = X2,
             Z = Z,
             L = L,
             a = a)

#set initial values for parameters
parameters <- list(beta = rep(0, times = length(beta)), 
                   gamma = rep(0, times = ncol(Z)),
                   lambda_nat = 0,
                   lambda_arm = 0,
                   log_b = 0,
                   log_var = 0) 

#call the model
compile("tmb/amod_ran_age.cpp") 
dyn.load(dynlib("tmb/amod_ran_age"))

model_age <- MakeADFun(data, parameters, random = "gamma", DLL="amod_ran_age", hessian=T)

#Fit model
fit_age <- nlminb(model_age$par, model_age$fn, model_age$gr)

#Extract parameter estimates
best_age <- model_age$env$last.par.best
print(best_age)
rep_age <- sdreport(model_age)
print(summary(rep_age))