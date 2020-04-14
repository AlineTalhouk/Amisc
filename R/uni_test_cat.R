#' @inheritParams describeBy
#' @param fac.dat data of categorical variables and one factor variable
#' @param fac.var categorical variables
#' @param fac.label categorical variable descriptions
#' @param by factor variable passed as `by1` from `describeBy`
#' @return a formatted summary of categorical variables
#' @noRd
uni_test_cat <- function(fac.dat, fac.var, fac.label, by, Missing, test,
                         digits = 0, p.digits = 3, per = "col",
                         simulate.p.value = FALSE, B = 2000) {
  # Verify `by` is a factor
  check_factor(fac.dat[, by])

  # Group and total categorical counts
  df <- fac.dat %>%
    dplyr::rename_at(fac.var, ~ fac.label) %>%
    dplyr::filter(!is.na(!!rlang::sym(by))) %>%
    dplyr::mutate_all(as.factor) %>%
    tidyr::pivot_longer(
      names_to = "Variable",
      names_ptypes = list(Variable = factor()),
      values_to = "Value",
      -!!rlang::sym(by)
    )
  group_counts <- df %>%
    dplyr::mutate(Value = forcats::fct_explicit_na(.data$Value, "Missing")) %>%
    dplyr::count(Levels = !!rlang::sym(by), .data$Variable, .data$Value) %>%
    tidyr::complete(.data$Levels,
                    tidyr::nesting(.data$Value, .data$Variable),
                    fill = list(n = 0)) %>%
    dplyr::mutate(Levels = as.character(.data$Levels))
  total_counts <- df %>%
    dplyr::mutate(Value = forcats::fct_explicit_na(.data$Value, "Missing")) %>%
    dplyr::count(Levels = "Total", .data$Variable, .data$Value)
  all_counts <- dplyr::bind_rows(group_counts, total_counts) %>%
    dplyr::select("Levels", "Variable", "Value", "n")

  # Missing cases will only be shown they exist and if isTRUE(Missing)
  if (!("Missing" %in% levels(all_counts[["Value"]]) && Missing)) {
    all_counts <- dplyr::filter(all_counts, .data$Value != "Missing")
  }

  # Percent by column/row
  if (per == "col") {
    val_per <- all_counts %>%
      dplyr::group_by(.data$Levels, .data$Variable) %>%
      dplyr::mutate(prop = (.data$n / sum(.data$n[.data$Value != "Missing"])))
  } else if (per == "row") {
    val_per <- all_counts %>%
      dplyr::group_by(.data$Value, .data$Variable) %>%
      dplyr::mutate(prop = (.data$n / sum(.data$n[.data$Levels != "Total"])))
  }

  # Pivot table and merged counts with proportions
  formatted <- val_per %>%
    dplyr::ungroup() %>%
    dplyr::mutate(stat = ifelse(
      .data$Value == "Missing",
      .data$n,
      paste(.data$n, round_percent(.data$prop, digits), sep = " ")
    )) %>%
    dplyr::select(-c(.data$n, .data$prop)) %>%
    dplyr::arrange(.data$Variable) %>%
    tidyr::pivot_wider(names_from = "Levels", values_from = "stat")

  # Chi-squared test
  if (test) {
    pval_df <- df %>%
      dplyr::group_by(.data$Variable) %>%
      dplyr::summarize(
        PValue = ifelse(
          nlevels(droplevels(.data$Value)) == 1,
          NA_character_,
          stats::chisq.test(
            x = !!rlang::sym(by),
            y = .data$Value,
            simulate.p.value = simulate.p.value,
            B = B
          ) %>%
            purrr::pluck("p.value") %>%
            round_pvalue(p.digits)
        )
      )
    formatted <- dplyr::inner_join(formatted, pval_df, by = "Variable")
  }

  # Remove duplicates, set Value to character, rename Value to Levels
  formatted <- formatted %>%
    dplyr::mutate_at("Variable",
                     ~ ifelse(!duplicated(.), as.character(.), "")) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::matches("PValue")),
                     ~ ifelse(.data$Variable == "", "", .data$PValue)) %>%
    dplyr::mutate(Value = as.character(.data$Value)) %>%
    dplyr::rename(!!"Levels" := .data$Value)
  formatted
}
