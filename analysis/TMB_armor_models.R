require(TMB)

chinook <- net_list[[1]] %>% 
  mutate(logyday = log(yday)) %>% 
  dplyr::select(-c(year, month, yday, species, rest_yr, X500m, X1.2km))


### First run - only continuous variables and no random effects at all ####
data <- model.matrix(~ -1 + total + logyday + X100m, chinook) %>% as_tibble() %>% as.list()
parameters <- list(b0=0, b1=0, b2=0, logSigma=0)

obj <- MakeADFun(data, parameters, DLL="tmb/amod")
obj$hessian <- TRUE

opt <- do.call("optim", obj)
opt

opt$hessian ## <-- FD hessian from optim
obj$he()    ## <-- Analytical hessian

sdreport(obj)
