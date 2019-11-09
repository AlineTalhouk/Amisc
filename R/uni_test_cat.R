#' @inheritParams describeBy
#' @param fac.dat data of categorical variables and one factor variable
#' @param fac.var categorical variables
#' @param fac.label categorical variable descriptions
#' @param by factor variable passed as `by1` from `describeBy`
#' @return a formatted summary of categorical variables
#' @noRd
uni_test_cat <- function(fac.dat, fac.var, fac.label, by, Missing,
                         digits = 0, p.digits = 3, per = "col",
                         simulate.p.value = FALSE, B = 2000) {
  # Verify `by` is a factor and return number of levels
  ind <- fac.dat[, by]
  level_num <- check_factor(ind)

  # Obtain Summary Data
  stats_args <- tibble::lst(ind, level_num, digits, per, p.digits, Missing,
                            simulate.p.value, B)
  formatted <- fac.dat %>%
    dplyr::transmute_at(fac.var, factor) %>%
    purrr::splice(fac.var, fac.label) %>%
    purrr::pmap_dfr(~ purrr::invoke(
      sum_stats_cat, x = ..1, var = ..2, var.lab = ..3, stats_args
    ))
  formatted
}

# Main functions used to obtain the marginal totals, which are the total counts of the cases over the categories of interest
sum_stats_cat <- function(x, var, var.lab, ind, level_num, digits, per,
                          p.digits, Missing, simulate.p.value, B) {
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
    round_percent(digits) %>%
    paste(stats::addmargins(count, 2), .) %>%
    matrix(nrow = nrow(count),
           dimnames = list(levels(x), c(levels(ind), "Total")))

  # Missing cases will only be shown if Missing == TRUE and there are indeed missing ones
  na.r <- stats::addmargins(table(x, ind, useNA = "always"))[nlevels(x) + 1, -level_num - 1]
  if (sum(na.r) != 0 && Missing) {
    tots <- rbind(tots, Missing = na.r)
  }
  # re-arrange the matrix so that it will be able to rbind with continuous part
  pval <- stats::chisq.test(count, simulate.p.value = simulate.p.value, B = B)$p.value
  tots <- tots %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    dplyr::mutate(
      Variable = c(var.lab, rep("", nrow(.) - 1)),
      Levels = rownames(.),
      PValue = c(round_pvalue(pval, p.digits), rep("", nrow(.) - 1))
    ) %>%
    dplyr::select(c("Variable", "Levels", levels(ind), "Total", "PValue"))
  tots
}
