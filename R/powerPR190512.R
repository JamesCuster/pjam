# Things to do.
#   - Rename this script

#' Design Constructor (breif description goes here)
#'
#' You can add a slightly longer description here which is displayed underneath
#' the brief description
#'
#' @param x a list of "factors" used in simulation design
#'
#' @return A longer description of what the function does can go here.
#'
#' @export
#'
sim.design.fn <- function(x) {
  for (i in (length(x) - 1):1) {
    x[[i]] <- rep(x[[i]], each = length(x[[i + 1]]))
  }
  return(data.frame(x))
}

# Possible alternatives...not sure because I'm not sure what the ultimate goal
# is
foo <- list(
  apple = c("hello", "world"),
  bannana = c("green", "yellow", "brown"),
  orange = c("blood", "small"))
# This function may be doing what expand grid does
expand.grid(foo)

alternative <- function(x) {
  i <- which.max(lapply(x, length))
  x[[i]] <- rep(x[[i]], each = length(x[[i]]))
  return(data.frame(x))
}


#' Compute power in linear model
#'
#' Slightly longer description here
#'
#' @param n sample size
#' @param alpha significance level (type 1 error rate)
#' @param df.test number of degrees of freedom for test
#' @param df.adj adjusted degrees of freedom
#' @param r2.full r squared full
#' @param r2.red r squared reduced
#'
#' @return Compute power in linear model (more description)
#'
#' @examples
#' power.lm.fn(n = 180, alpha = 0.05, df.test = 2, df.adj = 4 + 1, r2.full = 0.1, r2.red = 0)
#'
#' @export
#'
power.lm.fn <- function(n, alpha, df.test, df.adj, r2.full, r2.red) {
  r2.part <- (r2.full - r2.red) / (1 - r2.red)  # does this need to be in here?
  lambda <- n * (r2.full - r2.red) / (1 - r2.full)
  power <-
    stats::pf(
      q = stats::qf(1 - alpha, df1 = df.test, df2 = n - df.test - df.adj),
      df1 = df.test,
      df2 = n - df.test - df.adj,
      ncp = lambda,
      lower.tail = FALSE
    )
  return(power)
}


#' Get Rsquare from Cohen's d
#'
#' Slightly longer description here
#'
#' @param d not sure
#' @param p not sure
#'
#' Detailed explanation of what the function does and how
#'
d2rsq <- function(d, p) {
  A <- 1 / (p * (1 - p))
  rsq <- d ^ 2 / (A + d ^ 2)
  return(rsq)
}
