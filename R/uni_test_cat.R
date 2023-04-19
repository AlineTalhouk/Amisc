#' @inheritParams describeBy
#' @param fac.dat data of categorical variables and one factor variable
#' @param fac.var categorical variables
#' @param fac.label categorical variable descriptions
#' @param by factor variable passed as `by1` from `describeBy`
#' @return a formatted summary of categorical variables
#' @noRd
uni_test_cat <- function(fac.dat, fac.var, fac.label, by, Missing, test,
                         digits = 0, p.digits = 3, bold_pval = FALSE,
                         sig.level = 0.05, per = "col",
                         simulate.p.value = FALSE, B = 2000) {
  # Verify `by` is a factor
  check_factor(fac.dat[, by])

  # Rename labels, remove missing from by variable, make all factors
  fac.dat <- fac.dat %>%
    dplyr::rename_at(fac.var, ~ fac.label) %>%
    dplyr::filter(!is.na(!!rlang::sym(by))) %>%
    dplyr::mutate_all(as.factor)

  # Formatted table with counts and percentages
  formatted <- purrr::map_dfr(rlang::syms(fac.label), ~ {
    # Group and total categorical counts
    grp <- fac.dat %>%
      dplyr::mutate(!!.x := forcats::fct_na_value_to_level(!!.x, "Missing")) %>%
      dplyr::count(Levels = !!rlang::sym(by), !!.x, .drop = FALSE)
    tot <- fac.dat %>%
      dplyr::mutate(!!.x := forcats::fct_na_value_to_level(!!.x, "Missing")) %>%
      dplyr::count(Levels = "Total", !!.x, .drop = FALSE)
    all <- dplyr::bind_rows(grp, tot)
    # Missing cases will only be shown they exist and if isTRUE(Missing)
    if (!("Missing" %in% levels(all[[rlang::as_name(.x)]]) && Missing)) {
      all <- dplyr::filter(all, !!.x != "Missing")
    }
    # Percent by column/row
    if (per == "col") {
      val_per <- all %>%
        dplyr::group_by(.data$Levels) %>%
        dplyr::mutate(prop = (.data$n / sum(.data$n[!!.x != "Missing"])))
    } else if (per == "row") {
      val_per <- all %>%
        dplyr::group_by(!!.x) %>%
        dplyr::mutate(prop = (.data$n / sum(.data$n[.data$Levels != "Total"])))
    }
    # Pivot table and merged counts with proportions
    if (per != "none") {
      val_per %>%
        dplyr::ungroup() %>%
        dplyr::transmute(Levels, !!.x, stat = ifelse(
          !!.x == "Missing",
          .data$n,
          paste(.data$n, round_percent(.data$prop, digits), sep = " ")
        )) %>%
        tidyr::pivot_longer(!!.x, names_to = "Variable", values_to = "Value") %>%
        tidyr::pivot_wider(names_from = "Levels", values_from = "stat")
    } else {
      all %>%
        tidyr::pivot_longer(vs, names_to = "Variable", values_to = "Value")%>%
        tidyr::pivot_wider(names_from = "Levels", values_from = "n")
    }
  })

  # Chi-squared test
  if (test) {
    pval_df <- fac.dat %>%
      tidyr::pivot_longer(
        names_to = "Variable",
        names_ptypes = list(Variable = factor()),
        values_to = "Value",
        values_ptypes = list(Value = character()),
        -!!rlang::sym(by)
      ) %>%
      dplyr::group_by(.data$Variable) %>%
      dplyr::summarize(
        PValue = ifelse(
          nlevels(factor(.data$Value)) == 1,
          NA_character_,
          stats::chisq.test(
            x = !!rlang::sym(by),
            y = .data$Value,
            simulate.p.value = simulate.p.value,
            B = B
          ) %>%
            purrr::pluck("p.value")
        )
      )
    pval_df <- pval_df %>%
      dplyr::mutate(
        !!"PValue" :=
          round_pvalue(.data$PValue, p.digits = p.digits) %>%
          purrr::map2_chr(.data$PValue, ~ ifelse(
            bold_pval & .y < sig.level, paste0("**", .x, "**"), .x
          ))
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
