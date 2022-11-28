# make data

require(TMB)
library(mvtnorm)

# Create random effects
set.seed(103)
sigma_re <- c(1,2)
nsites <- 6
rho <- 0.25

sigma <- 1
bbar <- 3
ubar <- 2


ndata <- 6 * 10
Sigma <- diag(sigma_re)
Sigma[1,2] <-Sigma[2,1] <- rho * prod(sigma_re)

devs <- rmvnorm(nsites, mean = c(bbar, ubar), sigma = Sigma,
        method="chol")
b <- bbar + devs[,1]
u <- ubar + devs[,2]

thedata <- data.frame(x = runif(ndata),
                      y = 1,
                      site = rep(1:6, times= 10)
)

thedata$site <- as.factor(thedata$site)

# Create model matrix
X <- model.matrix( ~ -1 + site, data = thedata) 
Z <- model.matrix( ~ -1 + site, data = thedata) * thedata$x
epsilon <- rnorm(ndata, 0, sigma)

thedata$y <- X %*% b + Z %*% u + epsilon

### First run - no random effects at all ####


data <- list(n = ndata,
             nsites =nsites,
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


compile("tmb/Timlinreg.cpp")
dyn.load(dynlib("tmb/Timlinreg"))

model <-
  MakeADFun(
    data = data,
    parameters = parameters,
    DLL = "linreg")
#,
#   random = "B"
#  )
fit <- nlminb(model$par, model$fn, model$gr)

data = list(y = thedata$y,
            x = thedata$x)

parameters = list(b0 = 0,
                  b1 = 0,
                  logsigma = 0)

data <- list(n = ndata,
             nsites =nsites,
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


compile("linregmixed_slopes.cpp")
dyn.load(dynlib("linregmixed_slopes"))

model <-
  MakeADFun(
    data = data,
    parameters = parameters,
    DLL = "linreg",
    silent = T)
#,
 #   random = "B"
#  )
fit <- nlminb(model$par, model$fn, model$gr)
rep <- sdreport(model)


