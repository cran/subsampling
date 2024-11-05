## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(subsampling)

## -----------------------------------------------------------------------------
set.seed(1)
d <- 3
K <- 2
G <- rbind(rep(-1/(K+1), K), diag(K) - 1/(K+1)) %x% diag(d)
N <- 1e4
beta.true.baseline <- cbind(rep(0, d), matrix(-1.5, d, K))
beta.true.summation <- cbind(rep(1, d), 0.5 * matrix(-1, d, K))
mu <- rep(0, d)
sigma <- matrix(0.5, nrow = d, ncol = d)
diag(sigma) <- rep(1, d)
X <- MASS::mvrnorm(N, mu, sigma)
prob <- exp(X %*% beta.true.summation)
prob <- prob / rowSums(prob)
Y <- apply(prob, 1, function(row) sample(0:K, size = 1, prob = row))
data <- as.data.frame(cbind(Y, X))
colnames(data) <- c("Y", paste("V", 1:ncol(X), sep=""))
head(data)

## ---- eval = FALSE------------------------------------------------------------
#  ssp.softmax(
#    formula,
#    data,
#    subset,
#    n.plt,
#    n.ssp,
#    criterion = "MSPE",
#    sampling.method = "poisson",
#    likelihood = "MSCLE",
#    constraint = "summation",
#    control = list(...),
#    contrasts = NULL,
#    ...
#  )

## -----------------------------------------------------------------------------
n.plt <- 200
n.ssp <- 600
formula <- Y ~ . -1
ssp.results1 <- ssp.softmax(formula = formula,
                            data = data,
                            n.plt = n.plt,
                            n.ssp = n.ssp,
                            criterion = 'MSPE',
                            sampling.method = 'withReplacement',
                            likelihood = 'weighted',
                            constraint = 'baseline'
                            )
summary(ssp.results1)

## -----------------------------------------------------------------------------
ssp.results2 <- ssp.softmax(formula = formula,
                            data = data,
                            n.plt = n.plt,
                            n.ssp = n.ssp,
                            criterion = 'MSPE',
                            sampling.method = 'poisson',
                            likelihood = 'MSCLE',
                            constraint = 'baseline'
                            )
summary(ssp.results2)

## -----------------------------------------------------------------------------
names(ssp.results1)

