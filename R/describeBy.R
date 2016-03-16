describeBy <- function (data, var.names, var.labels=var.names, by1,
                        by2=NULL, digits = 3, Missing=FALSE) {
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
      num.label <- var.labels[which(num.ind)]
      num.dat <- data.frame(var.dat[, num.var], facets) %>%
        set_colnames(c(num.var, by1, by2))
    } else {
      num.var <- num.dat <- NULL
    }
    if (length(which(fac.ind)) > 0) {  # Factor cases
      fac.var <- names(types)[which(fac.ind)]
      fac.label <- var.labels[which(fac.ind)]
      fac.dat <- data.frame(var.dat[, fac.var], facets) %>%
        set_colnames(c(fac.var, by1, by2))
    } else {
      fac.var <- fac.dat <- NULL
    }
  }

if(!(is.null(fac.dat)|is.null(num.dat))){  # Data is a mix of categorical and continuous
 num.formatted <- unitestsCont(num.dat, num.var,num.label, by1, showMissing=Missing)$formatted
 cat.formatted <- unitestsCat(fac.dat, fac.var,fac.label, by1, digits = 1, showMissing=Missing)$formatted
 final <- rbind(num.formatted, cat.formatted)
} else if(is.null(fac.dat)){ # Data is only continuous
 final <- unitestsCont(num.dat, num.var,num.label, by1,digits = 1, showMissing=Missing)$formatted
} else if(is.null(num.dat)){ #Data is only categorical
 final <- unitestsCat(fac.dat, fac.var,fac.label, by1, digits = 1, showMissing=Missing)$formatted
}

 return(final)
}

