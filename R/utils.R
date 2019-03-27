globalVariables(".")

#' Check the splitting variable is a factor, and return number of levels
#' @noRd
check_factor <- function(x) {
  if (inherits(x, "factor")) {
    nlevels(x)
  } else {
    stop("Splitting variable `by` does not inherit from class factor")
  }
}
