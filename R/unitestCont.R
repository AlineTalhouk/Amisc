unitestsCont <- function(num.dat, num.var, num.label, by, dispersion = "sd",
                         digits = 0, p.digits = 3, ShowTotal = ShowTotal, showMissing, test.type = "parametric") {

  # This function takes a data.frame(num.dat) of numerical columns as well as a factor column, and returns a statistical summary of df based on the factor column in num.dat
  #
  # Args:
  #   num.dat: A data.frame of all numerical variables and one factor variable
  #   num.var: Names of selected numerical variables
  #   num.label: Same as num.var
  #   by: Same factor column as by1 in describeBy
  #   ShowTotal: When set to be TRUE, it show total number of each factor
  #   showMissing: It's always set to be TRUE, but the missing ones will be shown only if there is one
  #
  #
  # Returns: A list of two summary data.frames(raw, formatted) summarise descriptives statistics for the input data.frame(num.dat)


  # Verify `by` in num.dat is indeed a factor
  if (is.factor(num.dat[, by])) {
    # Obtain number of distinct elements in the factor
    level_num <- nlevels(num.dat[, by])
  } else {
    stop("Argument By must be factor")
  }

  # Main function used to calculate Mean, SD, SEM, Median, IQR and Number of Missings
  sumStatsCont <- function(x) {
    c(
      Mean = mean(x, na.rm = T), SD = sd(x, na.rm = T), SEM = sd(x, na.rm = T) / sqrt(sum(!is.na(x))),
      Median = round(median(x, na.rm = T), digits), IQR_25 = quantile(x, 0.25, na.rm = TRUE),
      IQR_75 = quantile(x, 0.75, na.rm = TRUE),
      missing = sum(is.na(x))
    )
  }

  # Obtain Summary Result
  ind <- num.dat[, by]
  selected_df <- data.frame(num.dat[, num.var]) %>% set_colnames(num.var) # Select all num.var in num.dat as a data.frame
  resCont <- apply(selected_df, 2, function(x) by(x, ind, sumStatsCont))
  TotCount <- table(ind) # Count total number of each level in the factor column `by`
  ind_names <- attributes(TotCount)$dimnames$ind # a vector all level names

  # Compute Statistical test and obtain the p-value
  if (test.type == "non-parametric") {
    # Kruskal-Wallis Test
    test <- apply(selected_df, 2, function(x) round(kruskal.test(x ~ ind)$p.value, p.digits))
  } else {
    # Oneway.test : lhs ~ rhs, lhs gives sample vaules and rhs gives corresponding group(factor)
    test <- apply(selected_df, 2, function(x) round(oneway.test(x ~ ind)$p.value, p.digits))
  }

  # Since the codes below e.g. arrange, melt, dcast will order the
  # results by num.label, we need to change num.label as a factor
  # and manually set the level so that the order of num.label may
  # be preserved

  num.label <- factor(num.label, levels = num.label)
  tot_num <- length(num.var) # Total number of numerical variables

  final <- matrix(unlist(resCont), byrow = T, ncol = 7) %>%
    rbind(., unname(t(apply(selected_df, 2, sumStatsCont)))) %>%
    set_colnames(c("Mean", "SD", "SEM", "Median", "IQR_25", "IQR_75", "Missing")) %>%
    data.frame(
      num.var = c(rep(num.label, each = level_num), num.label),
      by = c(rep(levels(ind), tot_num), rep("", tot_num)), .
    ) %>%
    arrange(., num.var)

  # If we can not detect any missing element or we do not require the missing parts, the "Missing" category will be removed
  if (sum(final[, "Missing"]) == 0 | showMissing == FALSE) {
    final <- final[, !names(final) %in% c("Missing")]
    showMissing <- FALSE # re-set showMissing == FALSE so that missing elements will not show up
  }

  if (dispersion == "se") {
    f.final <- final %>%
      mutate("Mean (se)" = paste(round(Mean, digits), "(", round(SEM, digits), ")", sep = "")) %>%
      mutate("Median (IQR)" = paste(round(Median, digits), "(", round(IQR_25, digits), " ~ ", round(IQR_75, digits), ")", sep = "")) %>%
      select(-c(Mean, SEM, SD, Median, IQR_25, IQR_75))

    #-----------&#177;

    if (showMissing == TRUE) {
      f.final <- f.final %>% .[, c("num.var", "by", "Mean (se)", "Median (IQR)", "Missing")]
      f.final[, "Missing"] <- as.character(f.final[, "Missing"])
    }
    f.final <- f.final %>% reshape2::melt(., id = c("num.var", "by")) %>% reshape2::dcast(., num.var + relevel(variable, ref = "Mean (se)") ~ by)

    # set colnames
    colnames(f.final)[1:2] <- c("Variable", "Levels")
    f.final <- f.final[, c(1, 2, 4:ncol(f.final), 3)] # rearrange colnames for the sake of output layout
    colnames(f.final)[ncol(f.final)] <- "Total"

    total_count <- c()
    for (i in 3:ncol(f.final)) {
      total_count <- c(total_count, TotCount[which(colnames(f.final)[i] == ind_names)])
    }
  } else if (dispersion == "sd") {
    f.final <- final %>%
      mutate("Median (IQR)" = paste(round(Median, digits), "(", round(IQR_25, digits), " ~ ", round(IQR_75, digits), ")", sep = "")) %>%
      mutate("Mean (sd)" = paste(round(Mean, digits), "(", round(SD, digits), ")", sep = "")) %>%
      select(-c(Mean, SEM, SD, Median, IQR_25, IQR_75))
    if (showMissing == TRUE) {
      f.final <- f.final %>% .[, c("num.var", "by", "Mean (sd)", "Median (IQR)", "Missing")]
      f.final[, "Missing"] <- as.character(f.final[, "Missing"])
    }
    f.final <- f.final %>% reshape2::melt(., id = c("num.var", "by")) %>% reshape2::dcast(., num.var + relevel(variable, ref = "Mean (sd)") ~ by)

    # set colnames
    colnames(f.final)[1:2] <- c("Variable", "Levels")
    f.final <- f.final[, c(1, 2, 4:ncol(f.final), 3)] # rearrange colnames for the sake of output layout
    colnames(f.final)[ncol(f.final)] <- "Total"

    total_count <- c()
    for (i in 3:ncol(f.final)) {
      total_count <- c(total_count, TotCount[which(colnames(f.final)[i] == ind_names)])
    }
  } else {
    stop("dispersion should be either sd or se")
  }

  # Since num.label is a factor, we put back the actual character name
  final$num.var <- num.label[final$num.var]
  f.final$Variable <- num.label[f.final$Variable]
  f.final$Variable <- ifelse(mod(1:nrow(f.final), ifelse(showMissing, 3, 2)) == 1, paste0("**", f.final$Variable, "**"), "")

  if (ShowTotal == TRUE) {
    # If we would like to see the total numbers
    if (test.type == "non-parametric") {
      f.final$PValue <- as.vector(rbind(format(as.character(round(test, digits = p.digits)), nsmall = p.digits), matrix(rep("", ifelse(showMissing, 2, 1) * length(test)), ncol = length(test))))
      # Add the total number
      f.final <- f.final %>% mutate_if(is.factor, as.character) # Change the factor column into character to prepare for row inserting
      if (length(num.dat[, by]) > sum(total_count)) {
        MissingNumber <- as.character(length(num.dat[, by]) - sum(total_count))
        Row.Insert <- c("", "N", total_count, c(length(num.dat[, by]), "Kruskal_Wallis"))
        print(paste(as.character(MissingNumber), "missing in the Input Argument", as.character(by), ". "))
      } else {
        Row.Insert <- c("", "N", c(total_count, length(num.dat[, by])), "Kruskal_Wallis")
      }
    } else if (test.type == "parametric") {
      f.final$PValue <- as.vector(rbind(format(as.character(round(test, digits = p.digits)), nsmall = p.digits), matrix(rep("", ifelse(showMissing, 2, 1) * length(test)), ncol = length(test))))
      # Add the total number
      f.final <- f.final %>% mutate_if(is.factor, as.character) # Change the factor column into character to prepare for row inserting
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

    f.final <- DataCombine::InsertRow(f.final, NewRow = Row.Insert, RowNum = 1)
  } else {
    if (test.type == "non-parametric") {
      f.final$PValue <- as.vector(rbind(format(round(test, digits = p.digits), nsmall = p.digits), matrix(rep("", ifelse(showMissing, 2, 1) * length(test)), ncol = length(test))))
      Row.Insert <- c("", "N", "", "", "Kruskal_Wallis")
    } else if (test.type == "parametric") {
      f.final$PValue <- as.vector(rbind(format(round(test, digits = p.digits), nsmall = p.digits), matrix(rep("", ifelse(showMissing, 2, 1) * length(test)), ncol = length(test))))
      Row.Insert <- c("", "N", "", "", "OneWay_Test")
    } else {
      stop("test.type should be either non-parametric or parametric")
    }
  }
  return(list(raw = final, formatted = f.final))
}
