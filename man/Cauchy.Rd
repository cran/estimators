% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/02_Cauchy.R
\name{Cauchy}
\alias{Cauchy}
\alias{d,Cauchy-method}
\alias{p,Cauchy-method}
\alias{qn,Cauchy-method}
\alias{r,Cauchy-method}
\alias{median,Cauchy-method}
\alias{mode,Cauchy-method}
\alias{entro,Cauchy-method}
\alias{finf,Cauchy-method}
\alias{ll,numeric,numeric,Cauchy-method}
\title{Cauchy Distribution}
\usage{
Cauchy(location = 1, scale = 1)

\S4method{d}{Cauchy}(x)

\S4method{p}{Cauchy}(x)

\S4method{qn}{Cauchy}(x)

\S4method{r}{Cauchy}(x)

\S4method{median}{Cauchy}(x)

\S4method{mode}{Cauchy}(x)

\S4method{entro}{Cauchy}(x)

\S4method{finf}{Cauchy}(x)

\S4method{ll}{numeric,numeric,Cauchy}(x, prm, distr)
}
\arguments{
\item{location, scale}{numeric. The distribution parameters.}

\item{x}{an object of class \code{Cauchy}. If the function also has a \code{distr}
argument, \code{x} is a numeric vector, a sample of observations.}

\item{prm}{numeric. A vector including the distribution parameters.}

\item{distr}{an object of class \code{Cauchy}.}
}
\value{
The dpqr family of functions return the evaluated density, cumulative
probability, quantile, and random sample, respectively.
The moments family of functions return the appropriate theoretical moment,
as calculated by the distribution true parameters.
The ll function returns the evaluated log-likelihood, given a sample and the
theoretical parameters.
The estim family of functions return the estimated parameters of the
distribution, given a sample.
The avar family of functions return the asymptotic variance or variance -
covariance matrix (if there are two or more parameters) of the corresponding
estimation method.
Calculus performed on Distribution objects returns a Distribution object of
the appropriate class and with the appropriate parameters.
}
\description{
Cauchy Distribution
}
