## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(subsampling)

## -----------------------------------------------------------------------------
set.seed(2)
N <- 1e4
d_rare <- 3
d_cont <- 2
p_rare <- c(0.01, 0.02, 0.05)
beta0 <- c(0.5, rep(0.5, d_rare), rep(0.5, d_cont)) 
corr <- 0.5
sigmax  <- matrix(corr, d_cont, d_cont) + diag(1-corr, d_cont)
X <- MASS::mvrnorm(N, rep(0, d_cont), sigmax)
Z <- do.call(cbind, lapply(seq_along(p_rare), function(i) {
rbinom(N, 1, p_rare[i])
}))
X <- cbind(Z, X)
P <- 1 / (1 + exp(-(beta0[1] + X %*% beta0[-1])))
Y <- as.integer(rbinom(N, 1, P))
colnames(X) <- paste0("X", 1:(d_rare + d_cont))
rareFeature.index <- c(1:d_rare)
data <- data.frame(Y = Y, X)
formula <- Y ~ .
head(data)
summary(data)

## ----eval = FALSE-------------------------------------------------------------
#  ssp.glm.rF(formula,
#             data,
#             subset = NULL,
#             n.plt,
#             n.ssp,
#             family = 'binomial',
#             criterion = 'BL-Uni',
#             sampling.method = 'poisson',
#             likelihood = 'weighted',
#             balance.plt = TRUE,
#             balance.Y = FALSE,
#             rareFeature.index = NULL,
#             control = list(...),
#             contrasts = NULL,
#             ...
#             )

## -----------------------------------------------------------------------------
n.plt <- 300
n.ssp <- 2000
BL.Uni.results <- ssp.glm.rF(formula = formula, 
data = data, 
n.plt = n.plt,
n.ssp = n.ssp,
family = 'quasibinomial',
criterion = 'BL-Uni',
sampling.method = 'poisson',
likelihood = 'weighted',
balance.plt = TRUE,
balance.Y = FALSE,
rareFeature.index = rareFeature.index
)
summary(BL.Uni.results)

## -----------------------------------------------------------------------------
R.Lopt.results <- ssp.glm.rF(formula = formula, 
data = data, 
n.plt = n.plt,
n.ssp = n.ssp,
family = 'quasibinomial',
criterion = 'R-Lopt',
sampling.method = 'poisson',
likelihood = 'weighted',
balance.plt = TRUE,
balance.Y = FALSE,
rareFeature.index = rareFeature.index
)
summary(R.Lopt.results)

