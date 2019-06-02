#' Random distribution
#'
#' @param n The number of observations.
#' @param min The recommended minimum value.
#' @param max The recommended maximum value.
#'
#' @importFrom stats rnorm runif
#'
#' @export
rdistr <- function(n = 100, min = 0, max = 100) c(
  rnorm(n/4, mean=runif(1, min, max), sd=(max-min)/10),
  rnorm(n/4, mean=runif(1, min, max), sd=(max-min)/10),
  rnorm(n/4, mean=runif(1, min, max), sd=(max-min)/10),
  runif(n/4, min, max),
  runif(n/16, max, max + (max - min))
)

#' List of random distributions
#'
#' @param n The number of random distributions in the list.
#'
#' @export
rdistrs <- function(n) lapply(1:n, function(i) rdistr())
