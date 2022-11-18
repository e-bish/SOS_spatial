require(TMB)

chinook <- net_list[[1]] %>% dplyr::select(-rest_yr)

chinook.mm <- chinook %>% model.matrix(total)

### First run - no random effects at all ####

DATA_VECTOR(y);
DATA_VECTOR(x);


data <- list(n = ndata,
             nsites = nsites,
             X = as.matrix(X),
             y = thedata$y,
             Z = as.matrix(Z)
)
params <- list(B = matrix(0, nrow = nsites, ncol = 2),
               bbar = 0,
               ubar = 0,
               logsigma =0,
               logsigma_re = c(0,0)
)


compile("tmb/linreg.cpp")
dyn.load(dynlib("tmb/linreg"))

model <-
  MakeADFun(
    data = data,
    parameters = parameters,
    DLL = "linreg")
#,
#   random = "B"
#  )
fit <- nlminb(model$par, model$fn, model$gr)


library(TMB)
runExample(all=TRUE)
