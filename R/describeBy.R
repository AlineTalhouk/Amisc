#' univariable association
#'
#' univariable association
#'
#' univariable association
#'
#' @param some parameters
#' @return possibly something
#' @author Aline Talhouk
#' @export
#' @import
#' @importFrom (tidyr, extract), (reshape, expand)
#' @examples TODO

describeBy <- function (data, var.names, var.labels = var.names, by1, dispersion="se", ShowTotal = TRUE,
                        by2 = NULL, digits = 1, p.digits = 3, Missing = TRUE, stats = "parametric",
                        simulate.p.value = FALSE, # Only for unitestCat (Ignored by unitestCont)
                        B = 2000 # Only for unitestCat (Ignored by unitestCont)
) {
  # This function takes up variables from a data.frame(df) and returns a descriptive statistic based on a selected factor `by1` in df.
  #
  # Args:
  #   data: Input data.frame(df)
  #   var.names: Variables from df that we are interested in
  #   var.labels: Same as var.names
  #   by1 : A selected factor in df
  #   ShowTotal: When set to be TRUE, it shows the total number of each level w/ by1.
  #   Missing: It's always set to be TRUE, and the missing ones will be shown only if there is one
  #
  # Returns: A summary data.frame summarise descriptives statistics for the input variables

  # Take out variables that we are interested in
  var.dat <- data[, var.names]
  facets <- data[, c(by1, by2)]

  # Apply class() function to all selected variable.names
  types <- sapply(var.dat, class)
  num.ind <- types %in% c("numeric", "integer")
  fac.ind <- types %in% c("factor", "character")

  # Separate selected variables into `numeric` and `factor`
  if (length(var.names) < 2) {  # Single input
    if (all(num.ind)) {
      # If the Single variable is Numeric/Integer
      # Obtain a data.frame of numerical variables: num.dat
      num.var <- var.names
      num.label <- num.var
      num.dat <- data.frame(var.dat, facets) %>%
        set_colnames(c(num.var, by1, by2))
      fac.var <- fac.dat <- NULL
    } else if (all(fac.ind)) {
      # If the Single variable is Factor/Character
      # Obtain a data.frame of factor variables: fac.dat
      fac.var <- var.names
      fac.label <- fac.var
      fac.dat <- data.frame(var.dat, facets) %>%
        set_colnames(c(fac.var, by1, by2))
      num.var <- num.dat <- NULL
    } else {
      stop('Variable must be numeric, integer, factor, or character.')
    }
  } else {
    # Multiple inputs
    if (length(which(num.ind)) > 0) {
      # Numeric cases
      num.var <- names(types)[which(num.ind)]
      num.label <- var.labels[which(num.ind)]
      num.dat <- data.frame(var.dat[, num.var], facets) %>%
        set_colnames(c(num.var, by1, by2))
    } else {
      num.var <- num.dat <- NULL
    }
    if (length(which(fac.ind)) > 0) {
      # Factor cases
      fac.var <- names(types)[which(fac.ind)]
      fac.label <- var.labels[which(fac.ind)]
      fac.dat <- data.frame(var.dat[, fac.var], facets) %>%
        set_colnames(c(fac.var, by1, by2))
    } else {
      fac.var <- fac.dat <- NULL
    }
  }
  # We use unitestsCont and/or unitestsCat to obtain statistic summaries

  if(!(is.null(fac.dat)|is.null(num.dat))) {
    # Data is a mix of categorical and numerical, then we apply unitestsCont and unitestsCat to numerical and categorical respectively
    num.formatted <- unitestsCont(num.dat, num.var,num.label, by1, dispersion = dispersion, digits = digits, p.digits = p.digits, ShowTotal = ShowTotal, showMissing = Missing)$formatted
    cat.formatted <- unitestsCat(fac.dat, fac.var, fac.label, by1, digits = digits, p.digits = p.digits, simulate.p.value = simulate.p.value, B=B, showMissing = Missing)$formatted
    final <- rbind(num.formatted, cat.formatted)
  } else if(is.null(fac.dat)){
    # Data is only numerical, then we only apply unitestsCont
    final <- unitestsCont(num.dat, num.var,num.label, by1, dispersion = dispersion, digits = digits, p.digits = p.digits, ShowTotal = ShowTotal, showMissing = Missing, test.type = stats)$formatted
  } else if(is.null(num.dat)){
    # Data is only categorical, then we only apply unitestsCat
    final <- unitestsCat(fac.dat, fac.var, fac.label, by1, digits = digits, p.digits = p.digits, simulate.p.value = simulate.p.value, B=B, showMissing = Missing)$formatted
  }
  return(final)
}

test <- function(a,b,c) {
  rm(a,envir=environment())
  print(as.list(environment()))
}
