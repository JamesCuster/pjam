# Things to do.
#   - Rename this script

#' Design Constructor (breif description goes here)
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

# Possible alternatives...not sure because I'm not sure what the ultimate goal is
foo <- list(apple = c("hello", "world"), bannana = c("green", "yellow", "brown"), orange = c("blood", "small"))
# This function may be doing what expand grid does
expand.grid(foo)

alternative <- function(x) {
  i <- which.max(lapply(x, length))
  x[[i]] <- rep(x[[i]], each = length(x[[i]]))
  return(data.frame(x))
}


#' Compute power in linear model
#'
#' @param n sample size
#' @param alpha significance level (type 1 error rate)
#' @param dfTest number of degrees of freedom for test
#' @param dfAdj adjusted degrees of freedom
#' @param R2full r squared full
#' @param R2red r squared reduced
#'
#' @return Compute power in linear model (more description)
#'
#' @examples
#' power.lm.fn(n = 180, alpha = 0.05, dfTest = 2, dfAdj = 4 + 1, R2full = 0.1, R2red = 0)
#'
#' @export
#'
power.lm.fn <- function(n, alpha, dfTest, dfAdj, R2full, R2red) {
  R2part <- (R2full - R2red) / (1 - R2red)
  lambda <- n * (R2full - R2red) / (1 - R2full)
  power <-
    stats::pf(
      q = stats::qf(1 - alpha, df1 = dfTest, df2 = n - dfTest - dfAdj),
      df1 = dfTest,
      df2 = n - dfTest - dfAdj,
      ncp = lambda,
      lower.tail = FALSE
    )
  return(power)
}

