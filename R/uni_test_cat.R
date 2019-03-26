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
  ind <- fac.dat[, by]
  if (is.factor(ind)) {
    level_num <- nlevels(ind)
  } else {
    stop("Argument 'by' must be of type factor")
  }

  # Obtain Summary Data
  stats_args <- tibble::lst(ind, level_num, digits, per, p.digits, showMissing,
                            simulate.p.value, B)
  row_header <- c(rep("", level_num + 3), "PearsonChi_square")
  formatted <- fac.dat %>%
    dplyr::transmute_at(fac.var, factor) %>%
    purrr::splice(fac.var, fac.label) %>%
    purrr::pmap_dfr(~ purrr::invoke(
      sum_stats_cat, x = ..1, var = ..2, var.lab = ..3, stats_args
    )) %>%
    rbind(row_header, .)
  tibble::lst(formatted)
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
    as.numeric() %>%
    scales::percent(accuracy = 10 ^ -(digits),
                    prefix = "(",
                    suffix = "%)") %>%
    paste(stats::addmargins(count, 2), .) %>%
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
  tots
}
