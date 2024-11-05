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
beta0 <- rep(-0.5, 7)
d <- length(beta0) - 1
corr <- 0.5
sigmax  <- matrix(corr, d, d) + diag(1-corr, d)
X <- MASS::mvrnorm(N, rep(0, d), sigmax)
colnames(X) <- paste("V", 1:ncol(X), sep = "")
P <- 1 - 1 / (1 + exp(beta0[1] + X %*% beta0[-1]))
Y <- rbinom(N, 1, P)
data <- as.data.frame(cbind(Y, X))
formula <- Y ~ .
head(data)

## ---- eval = FALSE------------------------------------------------------------
#  ssp.glm(
#    formula,
#    data,
#    subset = NULL,
#    n.plt,
#    n.ssp,
#    family = "quasibinomial",
#    criterion = "optL",
#    sampling.method = "poisson",
#    likelihood = "weighted",
#    control = list(...),
#    contrasts = NULL,
#    ...
#    )

## -----------------------------------------------------------------------------
n.plt <- 200
n.ssp <- 600
ssp.results <- ssp.glm(formula = formula,
                       data = data,
                       n.plt = n.plt,
                       n.ssp = n.ssp,
                       family = "quasibinomial",
                       criterion = "optL",
                       sampling.method = "withReplacement",
                       likelihood = "weighted"
                       )
summary(ssp.results)

## -----------------------------------------------------------------------------
ssp.results <- ssp.glm(formula = formula,
                       data = data,
                       n.plt = n.plt,
                       n.ssp = n.ssp,
                       family = "quasibinomial",
                       criterion = "optA",
                       sampling.method = "poisson",
                       likelihood = "logOddsCorrection"
                       )
summary(ssp.results)

## -----------------------------------------------------------------------------
names(ssp.results)

