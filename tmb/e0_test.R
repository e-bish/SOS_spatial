library(TMB)
library(mvtnorm)
library(mgcv)
library(dplyr)
library(parallel)
library(devtools)


##Simulating data from sablefish trawl data
#Data
setwd("~/Dropbox/choke species/code/thresholds_mi_distribution")
dat <- readRDS("data/data_sablefish2.rds")

#Get variance covariances of depth, temperature and po2
env.dat <- dplyr::select(dat, "po2", "depth", "temp")
var.covar <- var(log(env.dat))
env.bar <- colMeans(log(env.dat))

#Simulate environmental data
ndata <- 100
log.env <- rmvnorm(n = ndata, env.bar - diag(var.covar) / 2, var.covar)
env <- data.frame(exp(log.env))

#MI constants
Eo <-  0.4525966 
Ao <- 1E-7
n <- -0.303949 
B = 1000 # size in grams, roughly average
avgbn <-0.1124206  # this works for the adult class (0.5 - 6 kg).  for the large adult, adjust
kelvin = 273.15
boltz = 0.000086173324

#Calculate MI
env$mi = avgbn*Ao*env$po2*exp(Eo/(boltz*(env$temp+kelvin)))

#Set up parameter values for simulating
beta_3 <- 1 #Slope of MI effect
mi_crit <- 3 #Threshold of MI effect

theta <- 0
tweedie_p <- 1 + exp(theta) / ( 1 + exp (theta))
tweedie_disp <- 1
b3 <- as.array(c(beta_3,mi_crit)) #Make MI parameter array for TMB

#Simulate catch
logmu <- beta_3 * apply(cbind(env$mi, mi_crit), FUN = min, MAR = 1)
mu <- exp(logmu)

catch <- rTweedie(mu, phi = tweedie_disp, p = tweedie_p )

#################################################################
##Model fitting
#Create list of data for TMB

sim.data <- list(po2=env$po2, 
                 invtemp = 1/ (boltz * (env$temp + 273.15)), 
                 y=catch,
                 Ao = Ao,
                 avgbn = avgbn,
                 n = length(catch),
                 s_cut = 5)

#Create list of parameters for TMB
parameters <- list(Eo=0.5, 
                   b3= 0.5, 
                   logphi=log(tweedie_disp), 
                   theta=0)

#Compile tmb code

compile("TMB_stuff/eo_test.cpp")
dyn.load(dynlib("TMB_stuff/eo_test"))

#MakeADFun
model <- MakeADFun(data=sim.data, parameters=parameters, DLL="eo_test",silent=T, hessian=T)

#Fit model
fit <- nlminb(model$par, model$fn, model$gr)

#Extract parameter estimates
best <- model$env$last.par.best
print(best)
rep <- sdreport(model)
print(summary(rep))

#Diagnostics
model$he()
solve(model$he()) #Variance-covariance
cov2cor(solve(model$he())) #Correlation matrix
sqrt(diag(solve(model$he()))) #Standard errors
fit$objective

