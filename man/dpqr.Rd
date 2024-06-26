% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/01_generics.R
\name{dpqr}
\alias{dpqr}
\alias{d}
\alias{p}
\alias{q}
\alias{r}
\alias{qn}
\title{The d p q r Functions}
\usage{
d(x, ...)

p(x, ...)

qn(x, ...)

r(x, ...)
}
\arguments{
\item{x}{an object of subclass \code{Distribution}.}

\item{...}{extra arguments.}
}
\value{
The d p q r functions return the density, cumulative probability,
quantile, and random generator functions, respectively.
}
\description{
Four generic functions that take a distribution object (e.g. \code{Bern}) and
return the density, cumulative probability, quantile, and random generator
functions, respectively.
}
\examples{
# -----------------------------------------------------
# Beta Distribution Example
# -----------------------------------------------------

library(estimators)

# Create the distribution
x <- Beta(3, 5)

# Density function
df <- d(x)
df(c(0.3, 0.8, 0.5))

# Probability function
pf <- p(x)
pf(c(0.3, 0.8, 0.5))

# Density function
qf <- qn(x)
qf(c(0.3, 0.8, 0.5))

# Random Generator function
rf <- r(x)
rf(5)
}
