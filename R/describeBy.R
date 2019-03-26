#' Descriptive statistics
#'
#' Descriptive statistics and univariable association tests
#'
#' Takes variables from `data` and returns descriptive statistics split on
#' factor `by1`.
#'
#' @param data data.frame to produce descriptive statistics
#' @param var.names variable names of interest in `data`
#' @param var.labels variable descriptions. Uses `var.names` by default.
#' @param by1 factor to split other variables by in `data`
#' @param dispersion measure of variability, either "sd" (default) or "se".
#' @param ShowTotal logical; if `TRUE`, it shows the total number of each level
#'   w/ `by1`.
#' @param by2 optional second factor to split other variables by
#' @param per print column ("col") or row ("row") percentages
#' @param digits number of digits to round descriptive statistics
#' @param p.digits number of digits to round univariable test p-value
#' @param Missing logical; if `TRUE`, shows missing value counts, if they exist
#' @param stats either "parametric" (default) or "non-parametric" univariable
#'   tests are performed
#' @param simulate.p.value passed to `chisq.test`. Only relevant for categorical
#'   variables.
#' @param B passed to `chisq.test`. Only relevant for categorical variables.
#' @return A table with descriptive statistics for continuous and categorical
#'   variables and relevant univariable association tests
#' @author Aline Talhouk
#' @export
#' @examples
#' mtcars$cyl <- as.factor(mtcars$cyl)
#' mtcars$vs <- as.character(mtcars$vs)
#' Amisc::describeBy(data = mtcars, var.names = c("vs", "hp"), by1 = "cyl",
#' dispersion = "sd", Missing = TRUE, stats = "parametric")
describeBy <- function(data, var.names, var.labels = var.names, by1,
                       dispersion = c("sd", "se"), ShowTotal = TRUE, by2 = NULL,
                       per = "col", digits = 0, p.digits = 3, Missing = TRUE,
                       stats = c("parametric", "non-parametric"),
                       simulate.p.value = FALSE, B = 2000) {
  # Extract variables of interest
  var.dat <- data[, var.names, drop = FALSE]
  facets <- data[, c(by1, by2), drop = FALSE]

  # Store variable types, throw error for unsupported type
  types <- vapply(var.dat, class, character(1))
  num.ind <- which(types %in% c("numeric", "integer"))
  fac.ind <- which(types %in% c("factor", "character"))
  if (length(c(num.ind, fac.ind)) == 0) {
    stop("Variable(s) must be of type numeric, integer, factor, or character.")
  }

  # Separate selected variables into continuous and categorical
  num.var <- num.dat <- fac.var <- fac.dat <- NULL
  if (length(num.ind) > 0) {
    # Continuous: numeric and integer types
    num.var <- names(types)[num.ind]
    num.label <- var.labels[num.ind]
    num.dat <- cbind(var.dat[, num.var, drop = FALSE], facets)
  }
  if (length(fac.ind) > 0) {
    # Categorical: character and factor types
    fac.var <- names(types)[fac.ind]
    fac.label <- var.labels[fac.ind]
    fac.dat <- cbind(var.dat[, fac.var, drop = FALSE], facets)
  }

  # Use uni_test_cont and/or uni_test_cat to obtain summary statistics
  if (!(is.null(fac.dat) | is.null(num.dat))) {
    # Data is a mix of continuous and categorical variables, apply uni_test_cont and uni_test_cat respectively
    num.formatted <- uni_test_cont(num.dat, num.var, num.label, by1, dispersion = dispersion, digits = digits, p.digits = p.digits, showMissing = Missing, stats = stats)$formatted
    cat.formatted <- uni_test_cat(fac.dat, fac.var, fac.label, by1, per = per, digits = digits, p.digits = p.digits, simulate.p.value = simulate.p.value, B = B, showMissing = Missing)$formatted
    final <- rbind(num.formatted, cat.formatted)
  } else if (is.null(fac.dat)) {
    # Data is only continuous, only apply uni_test_cont
    final <- uni_test_cont(num.dat, num.var, num.label, by1, dispersion = dispersion, digits = digits, p.digits = p.digits, showMissing = Missing, stats = stats)$formatted
  } else if (is.null(num.dat)) {
    # Data is only categorical, only apply uni_test_cat
    final <- uni_test_cat(fac.dat, fac.var, fac.label, by1, per = per, digits = digits, p.digits = p.digits, simulate.p.value = simulate.p.value, B = B, showMissing = Missing)$formatted
  }

  # Add facet total counts and percentages to row header
  if (ShowTotal) {
    counts <- c(table(facets), nrow(facets))
    percents <- scales::percent(
      x = counts / nrow(facets),
      accuracy = 10 ^ -(digits),
      prefix = "(",
      suffix = "%)"
    )
    row_header <- c("N (%)", paste(counts, percents))
    final[1, 2:(length(final) - 1)] <- row_header
  }
  final
}
