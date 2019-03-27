#' @inheritParams describeBy
#' @param num.dat data of numerical variables and one factor variable
#' @param num.var numerical variables
#' @param num.label numerical variable descriptions
#' @param by factor variable passed as `by1` from `describeBy`
#' @return raw and formatted summaries of numerical variables
#' @importFrom rlang .data :=
#' @noRd
uni_test_cont <- function(num.dat, num.var, num.label, by,
                          dispersion = c("sd", "se"), digits = 0, p.digits = 3,
                          showMissing,
                          stats = c("parametric", "non-parametric")) {
  # Verify `by` is a factor and return number of levels
  ind <- num.dat[, by]
  level_num <- check_factor(ind)

  # Group and total continuous stats
  df <- num.dat[, num.var, drop = FALSE]
  group_stats <- df %>%
    purrr::map(base::by, INDICES = ind, FUN = sum_stats_cont) %>%
    purrr::imap_dfr(~ data.frame(
      Variable = .y,
      Levels = names(.x),
      purrr::invoke(rbind, .x),
      stringsAsFactors = FALSE
    ))
  total_stats <- df %>%
    purrr::map(sum_stats_cont) %>%
    purrr::invoke(rbind, .) %>%
    as.data.frame() %>%
    tibble::add_column(Levels = "Total", .before = 1) %>%
    tibble::rownames_to_column("Variable")

  # Choose parametric/non-parametric statistical test, format p-values
  switch(match.arg(stats),
         parametric = {
           f <- stats::oneway.test
           test_name <- "OneWay_Test"
         },
         `non-parametric` = {
           f <- stats::kruskal.test
           test_name <- "Kruskal_Wallis"
         })
  pvals <- purrr::map_dbl(df, ~ f(. ~ ind)$p.value)

  # Combine group/total stats and reorder Variable by num.label
  # Place total stats per variable after group stats
  raw <- rbind(group_stats, total_stats) %>%
    dplyr::mutate(
      Variable = factor(.data$Variable, labels = num.label),
      Levels = factor(.data$Levels, levels = c(levels(ind), "Total"))
    ) %>%
    dplyr::arrange(.data$Variable)

  # If we cannot detect any missing element or we do not require the missing
  # parts, the "Missing" variable will be removed
  if (sum(raw[["Missing"]]) == 0 | !showMissing) {
    raw <- dplyr::select(raw, -.data$Missing)
    showMissing <- FALSE # set FALSE so that missing elements will not show up
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
    dplyr::mutate_if(is.numeric, round, digits = digits) %>%
    dplyr::mutate(
      !!disp_name := paste0(.data$Mean, " (", .data[[disp_var]], ")"),
      `Median (IQR)` = paste0(.data$Median, " (", .data$IQR_25, " - ", .data$IQR_75, ")")
    ) %>%
    dplyr::select(-c("Mean", "SD", "SEM", "Median", "IQR_25", "IQR_75"))
  if ("Missing" %in% names(formatted)) {
    formatted <- formatted %>%
      dplyr::select(-"Missing", "Missing") %>%
      dplyr::mutate_at("Missing", as.character)
  }
  # Pivot table, bold variable names, add p-values and row header
  row_header <- c(rep("", level_num + 3), test_name)
  formatted <- formatted %>%
    tidyr::gather(key = "Stats", , -1:-2) %>%
    tidyr::spread("Levels", "value") %>%
    dplyr::mutate(
      PValue = pvals[match(.data$Variable, names(pvals))],
      first = !duplicated(.data$Variable),
      Variable = ifelse(.data$first, paste0("**", .data$Variable, "**"), ""),
      PValue = ifelse(.data$first, scales::pvalue(.data$PValue, accuracy = 10 ^ (-p.digits)), "")
    ) %>%
    dplyr::select(-"first") %>%
    dplyr::rename(!!"Levels" := .data$Stats) %>%
    rbind(row_header, .)

  # Indicate missing inputs if applicable
  total_count <- sum(table(ind))
  if (length(ind) > total_count) {
    n_missing <- length(ind) - total_count
    message(n_missing, " missing in 'by' argument ", sQuote(by), ".")
  }
  tibble::lst(raw, formatted)
}

# Main function used to calculate Mean, SD, SEM, Median, IQR and Missing
sum_stats_cont <- function(x) {
  c(
    Mean = mean(x, na.rm = TRUE),
    SD = stats::sd(x, na.rm = TRUE),
    SEM = stats::sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x))),
    Median = stats::median(x, na.rm = TRUE),
    IQR_25 = stats::quantile(x, 0.25, na.rm = TRUE, names = FALSE),
    IQR_75 = stats::quantile(x, 0.75, na.rm = TRUE, names = FALSE),
    Missing = sum(is.na(x))
  )
}
