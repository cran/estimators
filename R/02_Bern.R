# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Bern Distribution                                                         ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Distribution           ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~

setClass("Bern",
         contains = "Distribution",
         slots = c(prob = "numeric"),
         prototype = list(prob = 0.5))

#' @title Bernoulli Distribution
#' @name Bern
#'
#' @param x an object of class `Bern`. If the function also has a `distr`
#' argument, `x` is a numeric vector, a sample of observations.
#' @param distr an object of class `Bern`.
#' @param prm,prob numeric. The distribution parameter.
#'
#' @inherit Distributions return
#'
#' @importFrom extraDistr dbern pbern qbern rbern
#' @export
#'
#' @seealso [dpqr], [moments]
Bern <- function(prob = 0.5) {
  new("Bern", prob = prob)
}

setValidity("Bern", function(object) {
  if(length(object@prob) != 1) {
    stop("prob has to be a numeric of length 1")
  }
  if(object@prob <= 0 || object@prob >= 1) {
    stop("prob has to be between 0 and 1")
  }
  TRUE
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~
## d, p, q, r             ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname Bern
setMethod("d", signature = c(x = "Bern"),
          function(x) {
            function(y, log = FALSE) {
              extraDistr::dbern(y, prob = x@prob, log = log)
            }
          })

#' @rdname Bern
setMethod("p", signature = c(x = "Bern"),
          function(x) {
            function(q, lower.tail = TRUE, log.p = FALSE) {
              extraDistr::pbern(q, prob = x@prob,
                     lower.tail = lower.tail, log.p = log.p)
            }
          })

#' @rdname Bern
setMethod("qn", signature = c(x = "Bern"),
          function(x) {
            function(p, lower.tail = TRUE, log.p = FALSE) {
              extraDistr::qbern(p, prob = x@prob,
                     lower.tail = lower.tail, log.p = log.p)
            }
          })

#' @rdname Bern
setMethod("r", signature = c(x = "Bern"),
          function(x) {
            function(n) {
              extraDistr::rbern(n, prob = x@prob)
            }
          })

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Moments                ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname Bern
setMethod("mean",
          signature  = c(x = "Bern"),
          definition = function(x) {

  x@prob

})

#' @rdname Bern
setMethod("median",
          signature  = c(x = "Bern"),
          definition = function(x) {

  if (x@prob < 0.5) {
    return(0)
  } else if (x@prob > 0.5) {
    return(1)
  } else {
    warning("Bernoulli prob is equal to 0.5, therefore the median is any element
            in the [0, 1] interval. 0.5 is returned by default.")
    return(0.5)
  }

})

#' @rdname Bern
setMethod("mode",
          signature  = c(x = "Bern"),
          definition = function(x) {

  if (x@prob < 0.5) {
    return(0)
  } else if (x@prob > 0.5) {
    return(1)
  } else {
    warning("Bernoulli prob is equal to 0.5, therefore the mode is both 0 and 1.
            1 is returned by default.")
    return(1)
  }

})

#' @rdname Bern
setMethod("var",
          signature  = c(x = "Bern"),
          definition = function(x) {

  x@prob * (1 - x@prob)

})

#' @rdname Bern
setMethod("sd",
          signature  = c(x = "Bern"),
          definition = function(x) {

  sqrt(var(x))

})

#' @rdname Bern
setMethod("skew",
          signature  = c(x = "Bern"),
          definition = function(x) {

  p <- x@prob
  (1 - 2 * p) / sqrt(p * (1 - p))

})

#' @rdname Bern
setMethod("kurt",
          signature  = c(x = "Bern"),
          definition = function(x) {

  p <- x@prob
  q <- 1 - p
  (1 - 6 * p * q) / (p * q)

})

#' @rdname Bern
setMethod("entro",
          signature  = c(x = "Bern"),
          definition = function(x) {

  p <- x@prob
  q <- 1 - p
  - (q * log(q) + p * log(p))

})

#' @rdname Bern
setMethod("finf",
          signature  = c(x = "Bern"),
          definition = function(x) {

  1 / (x@prob * (1 - x@prob))

})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Likelihood             ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname ll
#' @export
llbern <- function(x, prob) {
  ll(x, prm = prob, distr = Bern())
}

#' @rdname Bern
setMethod("ll",
          signature  = c(x = "numeric", prm = "numeric", distr = "Bern"),
          definition = function(x, prm, distr) {

  n <- length(x)
  s <- sum(x)

  log(prm) * s + log(1 - prm) * (n - s)

})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Estimation             ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname estim
#' @export
ebern <- function(x, type = "mle", ...) {

  estim(x, Bern(), type, ...)

}

#' @rdname Bern
setMethod("mle",
          signature  = c(x = "numeric", distr = "Bern"),
          definition = function(x, distr) {

  c(prob = mean(x))

})

#' @rdname Bern
setMethod("me",
          signature  = c(x = "numeric", distr = "Bern"),
          definition = function(x, distr) {

  mle(x, distr)

})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Avar                   ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname avar
#' @export
vbern <- function(prob, type = "mle") {

  avar(Bern(prob = prob), type = type)

}

#' @rdname Bern
setMethod("avar_mle",
          signature  = c(distr = "Bern"),
          definition = function(distr) {

  prob <- distr@prob
  c(prob = prob * (1 - prob))

})

#' @rdname Bern
setMethod("avar_me",
          signature  = c(distr = "Bern"),
          definition = function(distr) {

  avar_mle(distr)

})
