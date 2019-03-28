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

#' Percent formatter with rounding
#' @noRd
round_percent <- function(x, digits) {
  scales::percent(x = x, accuracy = 10 ^ -(digits), prefix = "(", suffix = "%)")
}

#' PValue formatter with rounding
#' @noRd
round_pvalue <- function(x, p.digits) {
  scales::pvalue(x = x, accuracy = 10 ^ (-p.digits))
}
