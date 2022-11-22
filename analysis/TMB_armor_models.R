require(TMB)

chinook <- net_list[[1]] %>% 
  mutate(logyday = log(yday)) %>% 
  dplyr::select(-c(year, month, yday, species, rest_yr, X500m, X1.2km))


### First run - only continuous variables and no random effects at all ####
chinook.mm <- model.matrix(~ -1 + total + logyday + X100m, chinook) %>% 
  as_tibble() |> 
  transform(total = c(total), #strip attributes (otherwise TMB gets mad)
            logyday = c(logyday),
            X100m = c(X100m))

data <- list(y = chinook.mm$total,
             logyday = chinook.mm$logyday,
             X100m = chinook.mm$X100m)

parameters <- list(b0=0, b1=0, b2=0, log_var=log(1))

compile("tmb/amod.cpp") #zero means it worked
dyn.load(dynlib("tmb/amod"))

model <- MakeADFun(data, parameters, DLL="amod")

#Fit model
fit <- nlminb(model$par, model$fn, model$gr)

#Extract parameter estimates
best <- model$env$last.par.best
print(best)
rep <- sdreport(model)
print(summary(rep))

