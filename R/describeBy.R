describeBy <- function(data,  var.names, by1, by2=NULL, digits = 3) {
  var.dat <- data[, var.names]
  facets <- data[, c(by1, by2)]
  types <- sapply(var.dat, class)
  num.ind <- types %in% c("numeric", "integer")
  fac.ind <- types %in% c("factor", "character")
  # Separate numeric and factor components
  if (length(var.names) < 2) {  # Single response
    if (all(num.ind)) {  # Numeric case
      num.var <- var.names
      num.dat <- cbind(var.dat, facets) %>%
        set_colnames(c(num.var, by1, by2))
      fac.var <- fac.dat <- NULL
    } else if (all(fac.ind)) {  # Factor case
      fac.var <- var.names
      fac.dat <- cbind(var.dat, facets) %>%
        set_colnames(c(fac.var, by1, by2))
      num.var <- num.dat <- NULL
    } else {
      stop('Variable must be numeric, integer, factor, or character.')
    }
  } else {  # Multiple responses
    if (length(which(num.ind)) > 0) {  # Numeric cases
      num.var <- names(types)[which(num.ind)]
      num.dat <- cbind(var.dat[, num.var], facets) %>%
        set_colnames(c(num.var, by1, by2))
    } else {
      num.var <- num.dat <- NULL
    }
    if (length(which(fac.ind)) > 0) {  # Factor cases
      fac.var <- names(types)[which(fac.ind)]
      fac.dat <- cbind(var.dat[, fac.var], facets) %>%
        set_colnames(c(fac.var, by1, by2))
    } else {
      fac.var <- fac.dat <- NULL
    }
  }

 num.raw <- unitestsCont(num.dat, num.var, by1)$raw
 num.formatted <- unitestsCont(num.dat, num.var, by1)$formatted
 cat.formatted <- unitestsCat(fac.dat, fac.var, by1,digits = 1)$formatted
 #final <- rbind(num.formatted, cat.formatted)

 final <- rbind(num.formatted, cat.formatted)
 return(final)
}

