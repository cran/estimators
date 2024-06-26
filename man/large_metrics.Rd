% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/03_metrics.R
\name{large_metrics}
\alias{large_metrics}
\title{Large Sample Metrics}
\usage{
large_metrics(D, prm, est = c("same", "me", "mle"), ...)
}
\arguments{
\item{D}{A subclass of \code{Distribution}. The distribution family of interest.}

\item{prm}{A list containing three elements (name, pos, val). See Details.}

\item{est}{character. The estimator of interest. Can be a vector.}

\item{...}{extra arguments.}
}
\value{
A data.frame with columns "Row", "Col", "Parameter", "Estimator",
and "Value".
}
\description{
This function performs Monte Carlo simulations to estimate the asymptotic
variance - covariance matrix, characterizing the large sample behavior of an
estimator. The function evaluates the metrics as a function of a single
parameter, keeping the other ones constant. See Details.
}
\details{
The distribution \code{D} is used to specify an initial distribution. The list
\code{prm} contains details concerning a single parameter that is allowed to
change values. The quantity of interest is evaluated as a function of this
parameter.

Specifically, \code{prm} includes three elements named "name", "pos", and "val".
The first two elements determine the exact parameter that changes, while the
third one is a numeric vector holding the values it takes. For example,
in the case of the Multivariate Gamma distribution,
\code{D <- MGamma(shape = c(1, 2), scale = 3)} and
\code{prm <- list(name = "shape", pos = 2, val = seq(1, 1.5, by = 0.1))}
means that the evaluation will be performed for the MGamma distributions with
shape parameters \verb{(1, 1)}, \verb{(1, 1.1)}, ..., \verb{(1, 1.5)} and scale \code{3}. Notice
that the initial shape parameter \code{2} in \code{D} is not utilized in the function.
}
\examples{
\donttest{
D <- Beta(shape1 = 1, shape2 = 2)

prm <- list(name = "shape1",
            pos = NULL,
            val = seq(0.5, 2, by = 0.5))

x <- large_metrics(D, prm,
                   est = c("mle", "me", "same"))

plot_large_metrics(x)
}
}
\seealso{
\link{small_metrics}, \link{plot_small_metrics}, \link{plot_large_metrics}
}
