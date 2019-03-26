#' @inheritParams describeBy
#' @param fac.dat data of categorical variables and one factor variable
#' @param fac.var categorical variables
#' @param fac.label categorical variable descriptions
#' @param by factor variable passed as `by1` from `describeBy`
#' @return a formatted summary of categorical variables
#' @noRd
uni_test_cat <- function(fac.dat, fac.var, fac.label, by, per = "col",
                         digits = 0, p.digits = 3, showMissing,
                         simulate.p.value = FALSE, B = 2000) {
  # Verify `by` is a factor and store number of distinct levels
  if (is.factor(fac.dat[, by])) {
    level_num <- nlevels(fac.dat[, by])
  } else {
    stop("Argument 'by' must be of type factor")
  }

  # Obtain Summary Data
  ind <- fac.dat[, by]
  res <- NULL
  for (i in seq_along(fac.var)) {
    res <- rbind(res, sum_stats_cat(factor(fac.dat[, fac.var[i]]), fac.var[i], fac.label[i], ind, level_num, digits, per, p.digits, showMissing, simulate.p.value, B)$tots)
  }
  Row.Insert <- c(rep("", ncol(res) - 1), "PearsonChi_square")
  res <- rbind(Row.Insert, res)
  return(list(formatted = res))
}

# Main functions used to obtain the marginal totals, which are the total counts of the cases over the categories of interest
sum_stats_cat <- function(x, var, var.lab, ind, level_num, digits, per,
                          p.digits, showMissing, simulate.p.value, B) {
  x <- droplevels(x) # drop unused levels from a factor
  ind <- droplevels(ind)
  count <- table(x, ind, dnn = list(var, by))
  if (per == "col") {
    per.val <- count %>%
      stats::addmargins(margin = 2) %>%
      prop.table(margin = 2)
  } else if (per == "row") {
    per.val <- count %>%
      prop.table(margin = 1) %>%
      stats::addmargins(margin = 2)
  }
  tots <- per.val %>%
    magrittr::multiply_by(100) %>%
    round(digits) %>%
    paste0(stats::addmargins(count, 2), " (", ., "%)") %>%
    matrix(nrow = nrow(count),
           dimnames = list(levels(x), c(levels(ind), "Total")))

  # Missing cases will only be shown if showMissing == TRUE and there are indeed missing ones
  na.r <- stats::addmargins(table(x, ind, useNA = "always"))[nlevels(x) + 1, -level_num - 1]
  if (sum(na.r) != 0 && showMissing) {
    tots <- rbind(tots, Missing = na.r)
  }
  # re-arrange the matrix so that it will be able to rbind with continuous part
  test <- stats::chisq.test(count, simulate.p.value = simulate.p.value, B = B)$p.value
  tots <- tots %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    dplyr::mutate(
      Variable = c(paste0("**", var.lab, "**"), rep("", nrow(.) - 1)),
      Levels = rownames(.),
      PValue = c(format(round(test, p.digits), nsmall = p.digits), rep("", nrow(.) - 1))
    ) %>%
    dplyr::select(c("Variable", "Levels", levels(ind), "Total", "PValue"))
  tibble::lst(count, tots)
}
