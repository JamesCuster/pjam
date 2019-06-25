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
  a <- 1 / (p * (1 - p))
  rsq <- d ^ 2 / (a + d ^ 2)
  return(rsq)
}



#' New implmentation of \code{d2rsq}
#'
#' Adds parameter checks and expands features
#'
#' @param d Either a \code{data.frame} which contains columns corresponding to
#'   Cohen's d and the proportion, or a numeric vector of Cohen's d values.
#' @param p Numeric vector of proportions. The defualt value, \code{NULL},
#'   should be used if \code{d} is a \code{data.frame}.
#' @param var.names Ordered character vector with column names of columns
#'   corresponding to Cohen's d and the proportion. The name of the column
#'   corresponding to Cohen's d must come first \code{var.names} only needs to
#'   be specified if the columne names in the \code{data.frame} differ from "d"
#'   and "p".
#' @param expand logical. If \code{TRUE} \code{expand.grid} is used to create
#'   all combinations of the provided Cohen's d values and proportions.
#' @param return.df logical. If \code{FALSE} the function returns a vector of R
#'   squared values. If \code{TRUE} the function returns a \code{data.frame}
#'   containing all Cohen's d values and proportions with corresponding R
#'   squared values.
#'
d2rsq.2 <- function(d, p = NULL, var.names = NULL, expand = FALSE, return.df = FALSE) {
  # checks on data.frame (retrieves d and p as numeric vectors)
  if (is.data.frame(d)){
    if (!is.null(p)){
      stop("`d` and `p` should both be contatined in the data.frame d or given as numeric vectors")
    }
    if (!is.null(var.names)) {
      if (length(var.names) != 2) {
        stop("Incorrect dimensions given in var.names. ",
             "Should be a length 2 vector with column names of d then p")
      } else {
        p <- d[[var.names[2]]]
        d <- d[[var.names[1]]]
      }
    } else if (!all(c("d", "p") %in% names(d))) {
      stop("Can't find `d` or `p` in data.frame:\n",
           "  If the column names differ from ", dQuote("d"), " and ", dQuote("p"),
           " use var.names to specify their names.")
    } else {
      p <- d[["p"]]
      d <- d[["d"]]
    }
  }
  # checks on vectors
  if (any(is.na(d)) || any(is.na(p))){
    stop("`d` or `p` contain missing values")
  }
  if (!(is.numeric(d) && is.numeric(p))) {
    stop("d and p must be numeric")
  }
  if (any(p < 0) | any(p > 1)) {
    stop("p must a numeric in [0, 1]")
  }
  if (expand) {
    gd <- expand.grid(d, p)
    d <- gd[1]
    p <- gd[2]
  }
  if (length(d) != length(p)) {
    stop("`d` and `p` are not the same length")
  }
  a <- 1 / (p * (1 - p))
  rsq <- d ^ 2 / (a + d ^ 2)
  if (return.df) {
    rsq <- data.frame(d, p, rsq)
    rsq <- stats::setNames(rsq, c("d", "p", "rsq"))
  }
  return(rsq)
}
