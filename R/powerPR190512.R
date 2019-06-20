# Things to do.
#   - Rename this script

#' Design Constructor (breif description goes here)
#'
#' @param x a list of "factors" used in simulation design
#'
#' @return A longer description of what the function does can go here.
#'
sim.design.fn <- function(x) {
  for (i in (length(x) - 1):1) {
    x[[i]] <- rep(x[[i]], each = length(x[[i + 1]]))
  }
  return(data.frame(x))
}

foo <- list(apple = c("hello", "world"), bannana = c("green", "yellow", "brown"), orange = c("blood", "small"))
# This function may be doing what expand grid does
expand.grid(foo)

# Possible alternative...not sure because I'm not sure what the ultimate goal is
alternative <- function(x) {
  i <- which.max(lapply(foo, length))
  foo[[i]] <- rep(foo[[i]], each = length(foo[[i]]))
  return(data.frame(foo))
}

