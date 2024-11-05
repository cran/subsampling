## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(subsampling)

## -----------------------------------------------------------------------------
set.seed(2)
N <- 2 * 1e4
beta0 <- c(-6, -rep(0.5, 6))
d <- length(beta0) - 1
X <- matrix(0, N, d)
corr <- 0.5
sigmax <- corr ^ abs(outer(1:d, 1:d, "-"))
X <- MASS::mvrnorm(n = N, mu = rep(0, d), Sigma = sigmax)
Y <- rbinom(N, 1, 1 - 1 / (1 + exp(beta0[1] + X %*% beta0[-1])))
print(paste('N: ', N))
print(paste('sum(Y): ', sum(Y)))
n.plt <- 200
n.ssp <- 1000
data <- as.data.frame(cbind(Y, X))
colnames(data) <- c("Y", paste("V", 1:ncol(X), sep=""))
formula <- Y ~ .

## ---- eval = FALSE------------------------------------------------------------
#  ssp.relogit(
#    formula,
#    data,
#    subset = NULL,
#    n.plt,
#    n.ssp,
#    criterion = "optL",
#    likelihood = "logOddsCorrection",
#    control = list(...),
#    contrasts = NULL,
#    ...
#  )

## -----------------------------------------------------------------------------
n.plt <- 200
n.ssp <- 600
ssp.results <- ssp.relogit(formula = formula,
                           data = data,
                           n.plt = n.plt,
                           n.ssp = n.ssp,
                           criterion = 'optA',
                           likelihood = 'logOddsCorrection'
                           )

## -----------------------------------------------------------------------------
names(ssp.results)

## -----------------------------------------------------------------------------
summary(ssp.results)

