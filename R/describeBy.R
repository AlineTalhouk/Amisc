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
#' @param by2 optional second factor to split other variables by
#' @param total add a row showing the total counts of each `by1` level at the
#'   `top` or `bottom` of the table. Setting `none` hides the total row.
#' @param Missing logical; if `TRUE`, shows missing value counts, if they exist
#' @param digits number of digits to round descriptive statistics
#' @param p.digits number of digits to round univariable test p-value
#' @param dispersion measure of variability, either "sd" (default) or "se".
#' @param stats either "parametric" (default) or "non-parametric" univariable
#'   tests are performed
#' @param per print column ("col") or row ("row") percentages
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
#' Missing = TRUE, dispersion = "sd", stats = "parametric")
describeBy <- function(data, var.names, var.labels = var.names, by1, by2 = NULL,
                       total = c("top", "bottom", "none"), Missing = TRUE,
                       digits = 0, p.digits = 3, dispersion = c("sd", "se"),
                       stats = c("parametric", "non-parametric"),
                       per = "col", simulate.p.value = FALSE, B = 2000) {
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
  if (length(num.ind) > 0) {
    # Continuous: numeric and integer types
    num.var <- names(types)[num.ind]
    num.label <- var.labels[num.ind]
    num.dat <- cbind(var.dat[, num.var, drop = FALSE], facets)
    num.table <- uni_test_cont(num.dat, num.var, num.label, by1, Missing = Missing, digits = digits, p.digits = p.digits, dispersion = dispersion, stats = stats)
  }
  if (length(fac.ind) > 0) {
    # Categorical: character and factor types
    fac.var <- names(types)[fac.ind]
    fac.label <- var.labels[fac.ind]
    fac.dat <- cbind(var.dat[, fac.var, drop = FALSE], facets)
    fac.table <- uni_test_cat(fac.dat, fac.var, fac.label, by1, Missing = Missing, digits = digits, p.digits = p.digits, per = per, simulate.p.value = simulate.p.value, B = B)
  }

  # Combine summary statistics
  if (!exists("fac.table")) {
    final <- num.table  # Data has only continuous
  } else if (!exists("num.table")) {
    final <- fac.table  # Data has only categorical
  } else {
    final <- rbind(num.table, fac.table)  # Data has both continuous/categorical
  }

  # Add row for total counts and percentages
  total <- match.arg(total)
  if (total == "none") {
    return(final)
  } else {
    counts <- c(table(facets), nrow(facets))
    percents <- round_percent(counts / nrow(facets), digits)
    tr <- c("**Total**", "N (%)", paste(counts, percents), "")
    if (total == "top") {
      final <- rbind(tr, final)
    } else if (total == "bottom") {
      final <- rbind(final, tr)
    }
  }
  final
}
