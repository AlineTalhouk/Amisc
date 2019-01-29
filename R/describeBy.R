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
#' @param dispersion measure of variability, either "se" (default) or "sd".
#' @param ShowTotal logical; if `TRUE`, it shows the total number of each level
#'   w/ `by1`.
#' @param by2 optional second factor to split other variables by
#' @param per print column ("col") or row ("row") percentages
#' @param digits number of digits to round descriptive statistics
#' @param p.digits number of digits to round univariable test p-value
#' @param Missing logical; if `TRUE`, shows missing value counts, if they exist
#' @param stats either "parametric" or "non-parametric" univariable tests are
#'   performed
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
                       dispersion = "se", ShowTotal = TRUE, by2 = NULL,
                       per = "col", digits = 0, p.digits = 3, Missing = TRUE,
                       stats = "parametric", simulate.p.value = FALSE,
                       B = 2000) {
  # Take out variables that we are interested in
  var.dat <- data[, var.names]
  facets <- data[, c(by1, by2)]

  # Apply class() function to all selected variable.names
  types <- vapply(var.dat, class, character(1))
  num.ind <- types %in% c("numeric", "integer")
  fac.ind <- types %in% c("factor", "character")

  # Separate selected variables into `numeric` and `factor`
  if (length(var.names) < 2) { # Single input
    if (all(num.ind)) {
      # If the Single variable is Numeric/Integer
      # Obtain a data.frame of numerical variables: num.dat
      num.var <- var.names
      num.label <- num.var
      num.dat <- data.frame(var.dat, facets) %>%
        magrittr::set_colnames(c(num.var, by1, by2))
      fac.var <- fac.dat <- NULL
    } else if (all(fac.ind)) {
      # If the Single variable is Factor/Character
      # Obtain a data.frame of factor variables: fac.dat
      fac.var <- var.names
      fac.label <- fac.var
      fac.dat <- data.frame(var.dat, facets) %>%
        magrittr::set_colnames(c(fac.var, by1, by2))
      num.var <- num.dat <- NULL
    } else {
      stop("Variable must be numeric, integer, factor, or character.")
    }
  } else {
    # Multiple inputs
    if (length(which(num.ind)) > 0) {
      # Numeric cases
      num.var <- names(types)[which(num.ind)]
      num.label <- var.labels[which(num.ind)]
      num.dat <- data.frame(var.dat[, num.var], facets) %>%
        magrittr::set_colnames(c(num.var, by1, by2))
    } else {
      num.var <- num.dat <- NULL
    }
    if (length(which(fac.ind)) > 0) {
      # Factor cases
      fac.var <- names(types)[which(fac.ind)]
      fac.label <- var.labels[which(fac.ind)]
      fac.dat <- data.frame(var.dat[, fac.var], facets) %>%
        magrittr::set_colnames(c(fac.var, by1, by2))
    } else {
      fac.var <- fac.dat <- NULL
    }
  }
  # We use unitestsCont and/or unitestsCat to obtain statistic summaries

  if (!(is.null(fac.dat) | is.null(num.dat))) {
    # Data is a mix of categorical and numerical, then we apply unitestsCont and unitestsCat to numerical and categorical respectively
    num.formatted <- unitestsCont(num.dat, num.var, num.label, by1, dispersion = dispersion, digits = digits, p.digits = p.digits, ShowTotal = ShowTotal, showMissing = Missing, test.type = stats)$formatted
    cat.formatted <- unitestsCat(fac.dat, fac.var, fac.label, by1, per = per, digits = digits, p.digits = p.digits, simulate.p.value = simulate.p.value, B = B, showMissing = Missing)$formatted
    row <- c(rep("", ncol(cat.formatted) - 1), "PearsonChi_square")
    cat.formatted <- rbind(row, cat.formatted)

    final <- rbind(num.formatted, cat.formatted)
  } else if (is.null(fac.dat)) {
    # Data is only numerical, then we only apply unitestsCont
    final <- unitestsCont(num.dat, num.var, num.label, by1, dispersion = dispersion, digits = digits, p.digits = p.digits, ShowTotal = ShowTotal, showMissing = Missing, test.type = stats)$formatted
  } else if (is.null(num.dat)) {
    # Data is only categorical, then we only apply unitestsCat
    final <- unitestsCat(fac.dat, fac.var, fac.label, by1, per = per, digits = digits, p.digits = p.digits, simulate.p.value = simulate.p.value, B = B, showMissing = Missing)$formatted
    row <- c(rep("", ncol(final) - 1), "PearsonChi_square")
    final <- rbind(row, final)
  }
  return(final)
}
