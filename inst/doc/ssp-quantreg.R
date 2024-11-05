## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(subsampling)

## -----------------------------------------------------------------------------
set.seed(1)
N <- 1e4
tau <- 0.75
beta.true <- rep(1, 7)
d <- length(beta.true) - 1
corr  <- 0.5
sigmax  <- matrix(0, d, d)
for (i in 1:d) for (j in 1:d) sigmax[i, j] <- corr^(abs(i-j))
X <- MASS::mvrnorm(N, rep(0, d), sigmax)
err <- rnorm(N, 0, 1) - qnorm(tau)
Y <- beta.true[1] + X %*% beta.true[-1] + err * rowMeans(abs(X))
data <- as.data.frame(cbind(Y, X))
colnames(data) <- c("Y", paste("V", 1:ncol(X), sep=""))
formula <- Y ~ .
head(data)

## ---- eval = FALSE------------------------------------------------------------
#  ssp.quantreg(
#    formula,
#    data,
#    subset = NULL,
#    tau = 0.5,
#    n.plt,
#    n.ssp,
#    B = 5,
#    boot = TRUE,
#    criterion = "optL",
#    sampling.method = "withReplacement",
#    likelihood = c("weighted"),
#    control = list(...),
#    contrasts = NULL,
#    ...
#  )

## -----------------------------------------------------------------------------
B <- 5
n.plt <- 200
n.ssp <- 200
ssp.results1 <- ssp.quantreg(formula, 
                             data, 
                             tau = tau, 
                             n.plt = n.plt,
                             n.ssp = n.ssp,
                             B = B, 
                             boot = TRUE, 
                             criterion = 'optL',
                             sampling.method = 'withReplacement', 
                             likelihood = 'weighted'
                             )

ssp.results2 <- ssp.quantreg(formula, 
                             data, 
                             tau = tau, 
                             n.plt = n.plt,
                             n.ssp = n.ssp,
                             B = B, 
                             boot = FALSE, 
                             criterion = 'optL',
                             sampling.method = 'withReplacement', 
                             likelihood = 'weighted'
                             )

## -----------------------------------------------------------------------------
names(ssp.results1)

## -----------------------------------------------------------------------------
summary(ssp.results1)

## -----------------------------------------------------------------------------
summary(ssp.results2)

