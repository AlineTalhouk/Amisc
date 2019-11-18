#' @inheritParams describeBy
#' @param num.dat data of numerical variables and one factor variable
#' @param num.var numerical variables
#' @param num.label numerical variable descriptions
#' @param by factor variable passed as `by1` from `describeBy`
#' @return raw and formatted summaries of numerical variables
#' @importFrom rlang .data :=
#' @noRd
uni_test_cont <- function(num.dat, num.var, num.label, by, Missing,
                          digits = 0, p.digits = 3, dispersion = c("sd", "se"),
                          stats = c("parametric", "non-parametric")) {
  # Verify `by` is a factor and return number of levels
  level_num <- check_factor(num.dat[, by])

  # Group and total continuous stats
  df <- num.dat %>%
    dplyr::rename_at(num.var, ~ num.label) %>%
    dplyr::filter(!is.na(!!rlang::sym(by)))
  group_stats <- df %>%
    dplyr::group_by(Levels = !!rlang::sym(by)) %>%
    dplyr::summarize_if(is.numeric, ~ list(sum_stats_cont(.)))
  total_stats <- df %>%
    dplyr::group_by(Levels = "Total") %>%
    dplyr::summarize_if(is.numeric, ~ list(sum_stats_cont(.)))

  # Combine group/total stats and reorder Variable by num.label
  # Place total stats per variable after group stats
  raw <- rbind(group_stats, total_stats) %>%
    tidyr::pivot_longer(
      -"Levels",
      names_to = "Variable",
      names_ptypes = list(Variable = factor(levels = num.label)),
      values_to = "stats"
    ) %>%
    tidyr::unnest(stats) %>%
    dplyr::arrange(.data$Variable) %>%
    dplyr::select(.data$Variable, dplyr::everything())

  # If we cannot detect any missing element or we do not require the missing
  # parts, the "Missing" variable will be removed
  if (sum(raw[["Missing"]]) == 0 | !Missing) {
    raw <- dplyr::select(raw, -.data$Missing)
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

  # Choose parametric/non-parametric statistical test, format p-values
  f <- switch(
    match.arg(stats),
    parametric = stats::oneway.test,
    `non-parametric` = stats::kruskal.test
  )
  pvals <- df %>%
    dplyr::summarize_if(is.numeric, ~ f(. ~ !!rlang::sym(by))$p.value) %>%
    purrr::map_chr(round_pvalue, p.digits = p.digits)

  # Pivot table and add p-values
  formatted <- formatted %>%
    tidyr::gather(key = "Stats", , -1:-2) %>%
    tidyr::spread("Levels", "value") %>%
    dplyr::mutate(
      PValue = pvals[match(.data$Variable, names(pvals))],
      first = !duplicated(.data$Variable),
      Variable = ifelse(.data$first, as.character(.data$Variable), ""),
      PValue = ifelse(.data$first, .data$PValue, "")
    ) %>%
    dplyr::select(-"first") %>%
    dplyr::rename(!!"Levels" := .data$Stats)

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
