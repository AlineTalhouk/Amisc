#' @inheritParams describeBy
#' @param fac.dat data of categorical variables and one factor variable
#' @param fac.var categorical variables
#' @param fac.label categorical variable descriptions
#' @param by factor variable passed as `by1` from `describeBy`
#' @return a formatted summary of categorical variables
#' @noRd
unitestsCat <- function(fac.dat, fac.var, fac.label, by, per = "col",
                        digits = 0, p.digits = 3, showMissing,
                        simulate.p.value = FALSE, B = 2000) {
  # Verify `by` is a factor and store number of distinct levels
  if (is.factor(fac.dat[, by])) {
    level_num <- nlevels(fac.dat[, by])
  } else {
    stop("Argument 'by' must be of type factor")
  }

  # Main functions used to obtain the marginal totals, which are the total counts of the cases over the categories of interest
  sumStatsCat <- function(x, var, var.lab, ind, digits, per) {
    x <- droplevels(x) # drop unused levels from a factor
    ind <- droplevels(ind)
    count <- table(x, ind, dnn = list(var, by))
    na.r <- stats::addmargins(table(x, ind, useNA = "always"))[nlevels(x) + 1, -level_num - 1]
    if (per == "col") {
      per.val <- round(prop.table(stats::addmargins(count, margin = 2), margin = 2) * 100, digits)
    } else if (per == "row") {
      per.val <- round(stats::addmargins(prop.table(count, margin = 1), margin = 2) * 100, digits)
    }
    tots <- matrix(paste0(stats::addmargins(count, 2), " (", per.val, "%)"), byrow = FALSE, nrow = dim(count)[1]) %>%
      magrittr::set_rownames(levels(x))

    # Missing cases will only be shown if showMissing == TRUE and there are indeed missing ones
    if (sum(na.r) != 0 && showMissing == TRUE) {
      tots <- rbind(tots, Missing = na.r)
    }
    # re-arrange the matrix so that it will be able to rbind with continuous part
    tot.rowname <- rownames(tots)
    tots <- tots[, c(ncol(tots), 1:ncol(tots) - 1)]

    tots <- as.data.frame(matrix(tots, ncol = nlevels(ind) + 1), row.names = tot.rowname) %>%
      cbind(Variable = c(paste0("**", var.lab, "**"), rep("", nrow(.) - 1)), Levels = rownames(.), .) %>%
      cbind(., AssociationTest = c(format(round(stats::chisq.test(count, simulate.p.value = simulate.p.value, B = B)$p.value, p.digits), nsmall = p.digits), rep("", nrow(.) - 1))) %>%
      magrittr::set_rownames(NULL) %>%
      magrittr::set_colnames(c("Variable", "Levels", "Total", levels(ind), "PValue")) %>%
      dplyr::mutate_all(as.character)

    tots <- tots[, c(1, 2, 4:(ncol(tots) - 1), 3, ncol(tots))] # re-arrange columns for the sake of output layout

    return(list(count = count, tots = tots))
  }

  # Obtain Summary Data
  ind <- fac.dat[, by]
  res <- NULL
  for (i in seq_along(fac.var)) {
    res <- rbind(res, sumStatsCat(factor(fac.dat[, fac.var[i]]), fac.var[i], fac.label[i], ind, digits, per)$tots)
  }
  Row.Insert <- c(rep("", ncol(res) - 1), "PearsonChi_square")
  res <- rbind(Row.Insert, res)
  return(list(formatted = res))
}

