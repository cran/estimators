## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(distr)

## ----setup--------------------------------------------------------------------
library(estimators)

## -----------------------------------------------------------------------------
shape1 <- 1
shape2 <- 2

dbeta(0.5, shape1, shape2)
pbeta(0.5, shape1, shape2)
qbeta(0.75, shape1, shape2)
rbeta(2, shape1, shape2)

## ----eval = FALSE-------------------------------------------------------------
#  library(distr)

## -----------------------------------------------------------------------------
D <- Beta(shape1 = shape1, shape2 = shape2)

d(D)(0.5)
p(D)(0.5)
q(D)(0.75)
r(D)(2)

## -----------------------------------------------------------------------------
shape <- 1:3
scale <- 2

set.seed(1)
x1 <- rdirich(100, shape)
x2 <- rmgamma(100, shape, scale)

ddirich(x1[, 1], shape)
dmgamma(x2[, 1], shape, scale)

## -----------------------------------------------------------------------------
D1 <- Dirichlet(shape)
D2 <- MGamma(shape, scale)

set.seed(1)
x1 <- r(D1)(100)
x2 <- r(D2)(100)

d(D1)(x1[, 1])
d(D2)(x2[, 1])

## -----------------------------------------------------------------------------
set.seed(1)
x <- rbeta(100, shape1, shape2)
D <- Beta(shape1, shape2)

## -----------------------------------------------------------------------------
llbeta(x, shape1, shape2)
ll(x, c(shape1, shape2), D)

## -----------------------------------------------------------------------------
ll(x, c(shape1, shape2), "beta")

## -----------------------------------------------------------------------------
ebeta(x, type = "mle")
ebeta(x, type = "me")
ebeta(x, type = "same")

mle(x, D)
me(x, D)
same(x, D)

## -----------------------------------------------------------------------------
estim(x, D, type = "mle")

## -----------------------------------------------------------------------------
mle(x, "beta")
estim(x, "Beta", type = "mle")

## -----------------------------------------------------------------------------
vbeta(shape1, shape2, type = "mle")
vbeta(shape1, shape2, type = "me")
vbeta(shape1, shape2, type = "same")

avar_mle(D)
avar_me(D)
avar_same(D)

## -----------------------------------------------------------------------------
avar(D, type = "mle")

## ----fig.width=15, fig.height=8, out.width="100%"-----------------------------
prm <- list(name = "shape",
            pos = 1,
            val = seq(1, 5, by = 0.5))

x <- small_metrics(D1, prm,
             obs = c(20, 50),
             est = c("mle", "same", "me"),
             sam = 5e3,
             seed = 1)

head(x)

plot_small_metrics(x)

## ----fig.width=15, fig.height=8, out.width="100%"-----------------------------
prm <- list(name = "shape1",
            pos = NULL,
            val = seq(1, 5, by = 0.1))

x <- large_metrics(D, prm,
                   est = c("mle", "same", "me"))

head(x)

plot_large_metrics(x)

