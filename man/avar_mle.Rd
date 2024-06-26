% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/01_generics.R
\name{avar_mle}
\alias{avar_mle}
\title{MLE Asymptotic Variance}
\usage{
avar_mle(distr, ...)
}
\arguments{
\item{distr}{A subclass of \code{Distribution}. The distribution family assumed.}

\item{...}{extra arguments.}
}
\value{
A named matrix. The asymptotic covariance matrix of the estimator.
}
\description{
Calculates the asymptotic variance (or variance - covariance matrix in the
multidimensional case) of the MLE, given a specified family of
distributions and the true parameter values.
}
\examples{
# -----------------------------------------------------
# Beta Distribution Example
# -----------------------------------------------------

# Simulation
set.seed(1)
shape1 <- 1
shape2 <- 2
D <- Beta(shape1, shape2)
x <- r(D)(100)

# Likelihood - The ll Functions

llbeta(x, shape1, shape2)
ll(x, c(shape1, shape2), D)
ll(x, c(shape1, shape2), "beta")

# Point Estimation - The e Functions

ebeta(x, type = "mle")
ebeta(x, type = "me")
ebeta(x, type = "same")

mle(x, D)
me(x, D)
same(x, D)

estim(x, D, type = "mle")

# Asymptotic Variance - The v Functions

vbeta(shape1, shape2, type = "mle")
vbeta(shape1, shape2, type = "me")
vbeta(shape1, shape2, type = "same")

avar_mle(D)
avar_me(D)
avar_same(D)

avar(D, type = "mle")
}
\seealso{
\link{avar}, \link{avar_me}, \link{avar_same}
}
