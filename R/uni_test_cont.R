#' @inheritParams describeBy
#' @param num.dat data of numerical variables and one factor variable
#' @param num.var numerical variables
#' @param num.label numerical variable descriptions
#' @param by factor variable passed as `by1` from `describeBy`
#' @return a formatted summary of continuous variables
#' @noRd
uni_test_cont <- function(num.dat, num.var, num.label, by, Missing, test,
                          digits = 0, p.digits = 3, bold_pval = FALSE,
                          sig.level = 0.05, dispersion = c("sd", "se"),
                          stats = c("parametric", "non-parametric")) {
  # Verify `by` is a factor
  check_factor(num.dat[, by])

  # Group and total continuous stats
  # Combine group/total stats and reorder Variable by num.label
  # Place total stats per variable after group stats
  df <- num.dat %>%
    dplyr::rename_at(num.var, ~ num.label) %>%
    dplyr::filter(!is.na(!!rlang::sym(by)))
  groups <- df %>% dplyr::mutate(Levels = !!rlang::sym(by))
  totals <- df %>% dplyr::mutate(Levels = "Total")

  raw <- rbind(groups, totals) %>%
    dplyr::group_by(Levels) %>%
    dplyr::summarize(dplyr::across(dplyr::where(is.numeric),
                                   ~ list(sum_stats_cont(.)))) %>%
    tidyr::pivot_longer(
      -"Levels",
      names_to = "Variable",
      names_ptypes = list(Variable = factor(levels = num.label)),
      values_to = "stats"
    ) %>%
    tidyr::unnest(stats) %>%
    dplyr::arrange(.data$Variable) %>%
    dplyr::select("Variable", dplyr::everything())

  # If we cannot detect any missing element or we do not require the missing
  # parts, the "Missing" variable will be removed
  if (sum(raw[["Missing"]]) == 0 | !Missing) {
    raw <- dplyr::select(raw, -"Missing")
  }

  # Choose dispersion parameter
  switch(match.arg(dispersion),
         sd = {
           disp_name <- "Mean (sd)"
           disp_var <- "SD"
         },
         se = {
           disp_name <- "Mean (se)"
           disp_var <- "SEM"
         })

  # Repeat digits for each variable if multiple values supplied
  if (length(digits) > 1) {
    digits <- unlist(purrr::map2(digits, table(raw[["Variable"]]), rep_len))
  }

  # Formatted table with selected dispersion variable
  formatted <- raw %>%
    dplyr::mutate_if(is.double, round, digits = digits) %>%
    dplyr::mutate(
      !!disp_name := paste0(.data$Mean, " (", .data[[disp_var]], ")"),
      `Median (IQR)` = paste0(.data$Median, " (", .data$IQR_25, " - ", .data$IQR_75, ")")
    ) %>%
    dplyr::select_if(~ !is.double(.))
  if ("Missing" %in% names(formatted)) {
    formatted <- formatted %>%
      dplyr::select(-"Missing", "Missing") %>%
      dplyr::mutate_at("Missing", as.character)
  }

  # Pivot table
  formatted <- formatted %>%
    tidyr::gather(key = "Stats", , -1:-2) %>%
    tidyr::spread("Levels", "value") %>%
    dplyr::mutate(
      first = !duplicated(.data$Variable),
      Variable = ifelse(.data$first, as.character(.data$Variable), "")
    )

  # Choose parametric/non-parametric statistical test, format p-values
  if (test) {
    f <- switch(
      match.arg(stats),
      parametric = stats::oneway.test,
      `non-parametric` = stats::kruskal.test
    )
    pvals <- df %>%
      dplyr::summarize_if(is.numeric, ~ f(. ~ !!rlang::sym(by))$p.value)
    pvals_f <- pvals %>%
      purrr::map_chr(round_pvalue, p.digits = p.digits) %>%
      purrr::map2_chr(pvals, ~ ifelse(bold_pval &
                                        .y < sig.level, paste0("**", .x, "**"), .x))
    formatted <- formatted %>%
      dplyr::mutate(PValue = ifelse(.data$first, pvals_f[match(.data$Variable, names(pvals_f))], ""))
  }

  # Remove "first" column and rename stats to Levels
  formatted <- formatted %>%
    dplyr::select(-"first") %>%
    dplyr::rename(!!"Levels" := "Stats")

  # Indicate missing inputs if applicable
  n_missing <- nrow(num.dat) - sum(table(df[, by]))
  if (n_missing > 0) {
    message(n_missing, " missing in 'by' argument ", sQuote(by), ".")
  }
  formatted
}

# Main function used to calculate Mean, SD, SEM, Median, IQR and Missing
sum_stats_cont <- function(x) {
  data.frame(
    Mean = mean(x, na.rm = TRUE),
    SD = stats::sd(x, na.rm = TRUE),
    SEM = stats::sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x))),
    Median = stats::median(x, na.rm = TRUE),
    IQR_25 = stats::quantile(x, 0.25, na.rm = TRUE, names = FALSE),
    IQR_75 = stats::quantile(x, 0.75, na.rm = TRUE, names = FALSE),
    Missing = sum(is.na(x))
  )
}
