#' @inheritParams describeBy
#' @param num.dat data of numerical variables and one factor variable
#' @param num.var numerical variables
#' @param num.label numerical variable descriptions
#' @param by factor variable passed as `by1` from `describeBy`
#' @return raw and formatted summaries of numerical variables
#' @importFrom rlang .data
#' @noRd
uni_test_cont <- function(num.dat, num.var, num.label, by, dispersion = "sd",
                          digits = 0, p.digits = 3, ShowTotal,
                          showMissing, test.type = "parametric") {
  # Verify `by` is a factor and store number of distinct levels
  if (is.factor(num.dat[, by])) {
    level_num <- nlevels(num.dat[, by])
  } else {
    stop("Argument 'by' must be of type factor")
  }

  # Group and total continuous stats
  ind <- num.dat[, by]
  df <- data.frame(num.dat[, num.var, drop = FALSE])
  group_stats <- df %>%
    purrr::map(base::by, INDICES = ind, FUN = sum_stats_cont) %>%
    purrr::imap_dfr(~ data.frame(num.var = .y, by = names(.x), purrr::invoke(rbind, .x), stringsAsFactors = FALSE))
  total_stats <- df %>%
    purrr::map(sum_stats_cont) %>%
    purrr::invoke(rbind, .) %>%
    as.data.frame() %>%
    tibble::add_column(by = "Total", .before = 1) %>%
    tibble::rownames_to_column("num.var")
  TotCount <- table(ind) # Count total number of each level in the factor column `by`
  ind_names <- attributes(TotCount)$dimnames$ind # a vector all level names

  # Choose parametric/non-parametric statistical test
  switch(test.type,
         parametric = {
           f <- stats::oneway.test
           test_name <- "OneWay_Test"
         },
         `non-parametric` = {
           f <- stats::kruskal.test
           test_name <- "Kruskal_Wallis"
         })
  test <- apply(df, 2, function(x) round(f(x ~ ind)$p.value, p.digits))

  # Order num.var by num.label, we need to change num.label as a factor and
  # manually set the level so that the order of num.label is preserved
  raw <- rbind(group_stats, total_stats) %>%
    dplyr::mutate(
      num.var = factor(num.var, levels = num.label),
      by = forcats::fct_relevel(by, "Total", after = Inf)
    ) %>%
    dplyr::arrange(num.var)

  # If we cannot detect any missing element or we do not require the missing
  # parts, the "Missing" variable will be removed
  if (sum(raw[["Missing"]]) == 0 | !showMissing) {
    raw <- dplyr::select(raw, -.data$Missing)
    showMissing <- FALSE # set FALSE so that missing elements will not show up
  }

  # Choose dispersion parameter
  if (dispersion == "se") {
    disp_name <- "Mean (se)"
    disp_var <- "SEM"
  } else if (dispersion == "sd") {
    disp_name <- "Mean (sd)"
    disp_var <- "SD"
  } else {
    stop("dispersion should be either sd or se")
  }

  # Formatted table with selected dispersion variable
  formatted <- raw %>%
    dplyr::mutate_if(is.numeric, round, digits = digits) %>%
    dplyr::transmute(
      Variable = num.var,
      by,
      !!disp_name := paste0(.data$Mean, " (", .data[[disp_var]], ")"),
      `Median (IQR)` = paste0(.data$Median, " (", .data$IQR_25, " - ", .data$IQR_75, ")")
    )
  if ("Missing" %in% names(formatted)) {
    formatted <- formatted %>% dplyr::mutate_at("Missing", as.character)
  }
  formatted <- formatted %>%
    tidyr::gather(key = Levels, , -1:-2, factor_key = TRUE) %>%
    tidyr::spread(by, value)

  # Total counts
  total_count <- c()
  for (i in 3:ncol(formatted)) {
    total_count <- c(total_count, TotCount[which(colnames(formatted)[i] == ind_names)])
  }

  # Add bold formatting to variable names
  formatted$Variable <- ifelse(pracma::mod(1:nrow(formatted), ifelse(showMissing, 3, 2)) == 1, paste0("**", formatted$Variable, "**"), "")

  if (ShowTotal) {
    formatted$PValue <- as.vector(rbind(format(as.character(round(test, digits = p.digits)), nsmall = p.digits), matrix(rep("", ifelse(showMissing, 2, 1) * length(test)), ncol = length(test))))
    # Add the total number
    formatted <- formatted %>% dplyr::mutate_if(is.factor, as.character) # Change the factor column into character to prepare for row inserting
    if (length(num.dat[, by]) > sum(total_count)) {
      MissingNumber <- as.character(length(num.dat[, by]) - sum(total_count))
      Row.Insert <- c("", "N", total_count, c(length(num.dat[, by]), test_name))
      print(paste(as.character(MissingNumber), "missing in the Input Argument", as.character(by), ". "))
    } else {
      Row.Insert <- c("", "N", c(total_count, length(num.dat[, by])), test_name)
    }
  } else {
    formatted$PValue <- as.vector(rbind(format(round(test, digits = p.digits), nsmall = p.digits), matrix(rep("", ifelse(showMissing, 2, 1) * length(test)), ncol = length(test))))
    formatted <- formatted %>% dplyr::mutate_if(is.factor, as.character) # Change the factor column into character to prepare for row inserting
    Row.Insert <- c(rep("", level_num + 3), test_name)
  }
  formatted <- DataCombine::InsertRow(formatted, NewRow = Row.Insert, RowNum = 1)
  tibble::lst(raw, formatted)
}

# Main function used to calculate Mean, SD, SEM, Median, IQR and Number of Missings
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
