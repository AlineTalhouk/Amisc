#' @inheritParams describeBy
#' @param num.dat data of numerical variables and one factor variable
#' @param num.var numerical variables
#' @param num.label numerical variable descriptions
#' @param by factor variable passed as `by1` from `describeBy`
#' @return raw and formatted summaries of numerical variables
#' @importFrom rlang .data
#' @noRd
uni_test_cont <- function(num.dat, num.var, num.label, by, dispersion = "sd",
                          digits = 0, p.digits = 3, ShowTotal = ShowTotal,
                          showMissing, test.type = "parametric") {
  # Verify `by` is a factor and store number of distinct levels
  if (is.factor(num.dat[, by])) {
    level_num <- nlevels(num.dat[, by])
  } else {
    stop("Argument 'by' must be of type factor")
  }

  # Obtain Summary Result
  ind <- num.dat[, by]
  selected_df <- data.frame(num.dat[, num.var, drop = FALSE]) # Select all num.var in num.dat as a data.frame
  resCont <- apply(selected_df, 2, function(x) by(x, ind, sum_stats_cont))
  TotCount <- table(ind) # Count total number of each level in the factor column `by`
  ind_names <- attributes(TotCount)$dimnames$ind # a vector all level names

  # Run statistical test and obtain p-value
  f <- switch(
    test.type,
    parametric = stats::oneway.test,
    `non-parametric` = stats::kruskal.test
  )
  test <- apply(selected_df, 2, function(x) round(f(x ~ ind)$p.value, p.digits))

  # Since the codes below e.g. arrange, melt, dcast will order the
  # results by num.label, we need to change num.label as a factor
  # and manually set the level so that the order of num.label may
  # be preserved

  num.label <- factor(num.label, levels = num.label)
  tot_num <- length(num.var) # Total number of numerical variables

  raw <- resCont %>%
    purrr::map(~ purrr::invoke(rbind, .)) %>%
    purrr::invoke(rbind, .) %>%
    rbind(purrr::invoke(rbind, purrr::map(selected_df, sum_stats_cont))) %>%
    data.frame(num.var = c(rep(num.label, each = level_num), num.label),
               by = c(rep(levels(ind), tot_num), rep("", tot_num)),
               .) %>%
    dplyr::arrange(num.var)

  # If we can not detect any missing element or we do not require the missing parts, the "Missing" category will be removed
  if (sum(raw[, "Missing"]) == 0 | showMissing == FALSE) {
    raw <- raw[, !names(raw) %in% c("Missing")]
    showMissing <- FALSE # re-set showMissing == FALSE so that missing elements will not show up
  }

  if (dispersion == "se") {
    formatted <- raw %>%
      dplyr::mutate("Mean (se)" = paste(round(.data$Mean, digits), " (", round(.data$SEM, digits), ")", sep = "")) %>%
      dplyr::mutate("Median (IQR)" = paste(round(.data$Median, digits), " (", round(.data$IQR_25, digits), " - ", round(.data$IQR_75, digits), ")", sep = "")) %>%
      dplyr::select(-c("Mean", "SEM", "SD", "Median", "IQR_25", "IQR_75"))
    if (showMissing == TRUE) {
      formatted <- formatted %>% .[, c("num.var", "by", "Mean (se)", "Median (IQR)", "Missing")]
      formatted[, "Missing"] <- as.character(formatted[, "Missing"])
    }
    formatted <- formatted %>% reshape2::melt(., id = c("num.var", "by")) %>% reshape2::dcast(., num.var + relevel(variable, ref = "Mean (se)") ~ by)

    # set colnames
    colnames(formatted)[1:2] <- c("Variable", "Levels")
    formatted <- formatted[, c(1, 2, 4:ncol(formatted), 3)] # rearrange colnames for the sake of output layout
    colnames(formatted)[ncol(formatted)] <- "Total"

    total_count <- c()
    for (i in 3:ncol(formatted)) {
      total_count <- c(total_count, TotCount[which(colnames(formatted)[i] == ind_names)])
    }
  } else if (dispersion == "sd") {
    formatted <- raw %>%
      dplyr::mutate("Median (IQR)" = paste(round(.data$Median, digits), " (", round(.data$IQR_25, digits), " - ", round(.data$IQR_75, digits), ")", sep = "")) %>%
      dplyr::mutate("Mean (sd)" = paste(round(.data$Mean, digits), " (", round(.data$SD, digits), ")", sep = "")) %>%
      dplyr::select(-c("Mean", "SEM", "SD", "Median", "IQR_25", "IQR_75"))
    if (showMissing == TRUE) {
      formatted <- formatted %>% .[, c("num.var", "by", "Mean (sd)", "Median (IQR)", "Missing")]
      formatted[, "Missing"] <- as.character(formatted[, "Missing"])
    }
    formatted <- formatted %>% reshape2::melt(., id = c("num.var", "by")) %>% reshape2::dcast(., num.var + relevel(variable, ref = "Mean (sd)") ~ by)

    # set colnames
    colnames(formatted)[1:2] <- c("Variable", "Levels")
    formatted <- formatted[, c(1, 2, 4:ncol(formatted), 3)] # rearrange colnames for the sake of output layout
    colnames(formatted)[ncol(formatted)] <- "Total"

    total_count <- c()
    for (i in 3:ncol(formatted)) {
      total_count <- c(total_count, TotCount[which(colnames(formatted)[i] == ind_names)])
    }
  } else {
    stop("dispersion should be either sd or se")
  }

  # Since num.label is a factor, we put back the actual character name
  raw$num.var <- num.label[raw$num.var]
  formatted$Variable <- num.label[formatted$Variable]
  formatted$Variable <- ifelse(pracma::mod(1:nrow(formatted), ifelse(showMissing, 3, 2)) == 1, paste0("**", formatted$Variable, "**"), "")

  if (ShowTotal == TRUE) {
    # If we would like to see the total numbers
    if (test.type == "non-parametric") {
      formatted$PValue <- as.vector(rbind(format(as.character(round(test, digits = p.digits)), nsmall = p.digits), matrix(rep("", ifelse(showMissing, 2, 1) * length(test)), ncol = length(test))))
      # Add the total number
      formatted <- formatted %>% dplyr::mutate_if(is.factor, as.character) # Change the factor column into character to prepare for row inserting
      if (length(num.dat[, by]) > sum(total_count)) {
        MissingNumber <- as.character(length(num.dat[, by]) - sum(total_count))
        Row.Insert <- c("", "N", total_count, c(length(num.dat[, by]), "Kruskal_Wallis"))
        print(paste(as.character(MissingNumber), "missing in the Input Argument", as.character(by), ". "))
      } else {
        Row.Insert <- c("", "N", c(total_count, length(num.dat[, by])), "Kruskal_Wallis")
      }
    } else if (test.type == "parametric") {
      formatted$PValue <- as.vector(rbind(format(as.character(round(test, digits = p.digits)), nsmall = p.digits), matrix(rep("", ifelse(showMissing, 2, 1) * length(test)), ncol = length(test))))
      # Add the total number
      formatted <- formatted %>% dplyr::mutate_if(is.factor, as.character) # Change the factor column into character to prepare for row inserting
      if (length(num.dat[, by]) > sum(total_count)) {
        MissingNumber <- as.character(length(num.dat[, by]) - sum(total_count))
        Row.Insert <- c("", "N", total_count, c(length(num.dat[, by]), "OneWay_Test"))
        print(paste(as.character(MissingNumber), "missing in the Input Argument", as.character(by), ". "))
      } else {
        Row.Insert <- c("", "N", c(total_count, length(num.dat[, by])), "OneWay_Test")
      }
    } else {
      stop("test.type should be either non-parametric or parametric")
    }
  } else {
    if (test.type == "non-parametric") {
      formatted$PValue <- as.vector(rbind(format(round(test, digits = p.digits), nsmall = p.digits), matrix(rep("", ifelse(showMissing, 2, 1) * length(test)), ncol = length(test))))
      formatted <- formatted %>% dplyr::mutate_if(is.factor, as.character) # Change the factor column into character to prepare for row inserting
      Row.Insert <- c(rep("", level_num + 3), "Kruskal_Wallis")
    } else if (test.type == "parametric") {
      formatted$PValue <- as.vector(rbind(format(round(test, digits = p.digits), nsmall = p.digits), matrix(rep("", ifelse(showMissing, 2, 1) * length(test)), ncol = length(test))))
      formatted <- formatted %>% dplyr::mutate_if(is.factor, as.character) # Change the factor column into character to prepare for row inserting
      Row.Insert <- c(rep("", level_num + 3), "OneWay_Test")
    } else {
      stop("test.type should be either non-parametric or parametric")
    }
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
