unitestsCont <- function(num.dat, num.var, num.label, by, dispersion = "sd",
                         digits = 1, p.digits = 3, ShowTotal = ShowTotal, showMissing, test.type = "parametric") {

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


  # Verify `by` in num.dat is factor
  if(is.factor(num.dat[, by])) {
    # Obtain number of distinct elements in the factor
    p <- length(levels(num.dat[, by]))
  } else {
    stop('Arguement By must be factor')
  }

  # Main function used to calculate Mean, SD, SEM, Median, IQR and Number of Missings
  sumStatsCont <- function(x) {
    c(Mean = mean(x, na.rm = T), SD = sd(x, na.rm=T), SEM=sd(x, na.rm=T)/sqrt(sum(!is.na(x))), Median = round(median(x, na.rm = T), digits), IQR = IQR(x, na.rm = TRUE),
      missing = sum(is.na(x)))
  }
  # A function used to count Total Number
  CountTotal <- function(x) {
    c(n = length(!is.na(x)))
  }

  # Obtain Summary Data
  ind <- num.dat[, by]
  w <- data.frame(num.dat[, num.var]) %>% set_colnames(num.var) # Select all num.var in num.dat as a data.frame
  resCont <- apply(w, 2, function(x) by(x,ind, sumStatsCont))
  TotCount <- apply(w, 2, function(x) by(x, ind, CountTotal))[, 1] # Count the total number of each level w/ factor


  # Compute Statistical test and obtain the p-value

  if(test.type == "non-parametric"){
    # Kruskal-Wallis Test
    test <- apply(w, 2, function(x) round(kruskal.test(x ~ ind)$p.value, p.digits))
  } else{
    # Oneway.test : lhs ~ rhs, lhs gives sample vaules and rhs gives corresponding group(factor)
    test <- apply(w, 2, function(x) round(oneway.test(x ~ ind)$p.value, p.digits))
  }

  # Since the codes below e.g. arrange, melt, dcast will order the
  # results by num.label, we need to change num.label as a factor
  # and manually set the level so that the order of num.label may
  # be preserved

  num.label <- factor(num.label, levels = num.label)
  k <- length(num.var) # Total number of numerical variables

  final <- matrix(unlist(resCont), byrow = T, ncol = 6) %>%
    rbind(., unname(t(apply(w, 2, sumStatsCont)))) %>%
    set_colnames(c("Mean", "SD", "SEM", "Median", "IQR", "Missing")) %>%
    data.frame(
      num.var = c(rep(num.label, each=p), num.label),
      by=c(rep(levels(ind), k), rep("", k)), .) %>% arrange(., num.var)


  # If we can not detect any missing element or we do not require the missing parts, the "Missing" category will be removed
  if(sum(final[, "Missing"]) == 0 | showMissing == FALSE ) {
    final <- final[, !names(final) %in% c("Missing")]
    showMissing = FALSE # re-set showMissing == FALSE so that missing elements will not show up
  }

  if(dispersion == "se") {
    f.final <- final %>%
      mutate("Mean (se)" = paste(round(Mean, digits), "( "," &#177; ", round(SEM, digits), " )", sep = "")) %>%
      mutate("Median (IQR)" = paste(round(Median, digits), "( "," &#177; ", round(IQR, digits), " )", sep = "")) %>%
      select(-c(Mean, SEM, SD, Median, IQR))
    if(showMissing == TRUE){
      f.final <- f.final %>% .[, c("num.var", "by", "Mean (se)", "Median (IQR)", "Missing")]
      f.final[, "Missing"] <- as.character(f.final[, "Missing"])
    }
    f.final <- f.final %>% melt(., id = c("num.var", "by")) %>% dcast(., num.var + relevel(variable, ref = "Mean (se)") ~ by)

    # set colnames
    colnames(f.final) <- c("Variable", "Levels", levels(final$by))
    f.final <- f.final[, c(1, 2, 4: ncol(f.final), 3)] # rearrange colnames for the sake of output layout
    colnames(f.final)[ncol(f.final)] <- "Total"

  } else if (dispersion == "sd") {
    f.final <- final %>%
      mutate("Median (IQR)" = paste(round(Median, digits), "( "," &#177; ", round(IQR, digits), " )", sep = "")) %>%
      mutate("Mean (sd)" = paste(round(Mean, digits), "( "," &#177; ", round(SEM, digits), " )", sep = "")) %>%
      select(-c(Mean, SEM, SD, Median, IQR))
    if(showMissing == TRUE){
      f.final <- f.final %>% .[, c("num.var", "by", "Mean (sd)", "Median (IQR)", "Missing")]
      f.final[, "Missing"] <- as.character(f.final[, "Missing"])
    }
    f.final <- f.final %>% melt(., id = c("num.var", "by")) %>% dcast(., num.var + relevel(variable, ref = "Mean (sd)") ~ by)

    # set colnames
    colnames(f.final) <- c("Variable", "Levels", levels(final$by))
    f.final <- f.final[, c(1, 2, 4: ncol(f.final), 3)] # rearrange colnames for the sake of output layout
    colnames(f.final)[ncol(f.final)] <- "Total"
  } else {
    stop("dispersion should be either sd or se")
  }

  # Since num.label is a factor, we put back the actual character name
  final$num.var <- num.label[final$num.var]
  f.final$Variable <- num.label[f.final$Variable]
  f.final$Variable <- ifelse(mod(1: nrow(f.final), ifelse(showMissing, 3, 2)) == 1, paste0("**", f.final$Variable, "**"), "")

  if(ShowTotal == TRUE){
    # If we would like to see the total numbers
    if(test.type == "non-parametric") {
      f.final$PValue <- as.vector(rbind(format(as.character(round(test, digits = p.digits)), nsmall = p.digits), matrix(rep("", ifelse(showMissing, 2, 1)*length(test)), ncol = length(test))))
      # Add the total number
      f.final <- f.final %>% mutate_if(is.factor, as.character) # Change the factor column into character to prepare for row inserting
      if(length(num.dat[, by]) > sum(TotCount)) {
        MissingNumber <- as.character(length(num.dat[, by]) - sum(TotCount))
        Row.Insert <- c("", "N", TotCount, c(paste(length(num.dat[, by]), "with", MissingNumber, "class missing"), "Kruskal_Wallis"))
      } else {
        Row.Insert <- c("", "N", c(TotCount, paste(length(num.dat[, by]), "with no class missing")), "Kruskal_Wallis")
      }
    } else if(test.type == "parametric") {
      f.final$PValue <- as.vector(rbind(format(as.character(round(test, digits = p.digits)), nsmall = p.digits), matrix(rep("", ifelse(showMissing, 2, 1)*length(test)), ncol = length(test))))
      # Add the total number
      f.final <- f.final %>% mutate_if(is.factor, as.character) # Change the factor column into character to prepare for row inserting
      if(length(num.dat[, by]) > sum(TotCount)) {
        MissingNumber <- as.character(length(num.dat[, by]) - sum(TotCount))
        Row.Insert <- c("", "N", TotCount, c(paste(length(num.dat[, by]), "with", MissingNumber, "class missing"), "OneWay_Test"))
      } else {
        Row.Insert <- c("", "N", c(TotCount, paste(length(num.dat[, by]), "with no class missing")), "OneWay_Test")
      }
    } else {
      stop("test.type should be either non-parametric or parametric")
    }

    f.final <- InsertRow(f.final, NewRow = Row.Insert, RowNum = 1)
  } else {
    if(test.type == "non-parametric") {
      f.final$PValue <- as.vector(rbind(format(round(test, digits = p.digits), nsmall = p.digits), matrix(rep("", ifelse(showMissing, 2, 1)*length(test)), ncol = length(test))))
      Row.Insert <- c("", "N", "", "", "Kruskal_Wallis")
    } else if(test.type == "parametric") {
      f.final$PValue <- as.vector(rbind(format(round(test, digits = p.digits), nsmall = p.digits), matrix(rep("", ifelse(showMissing, 2, 1)*length(test)), ncol = length(test))))
      Row.Insert <- c("", "N", "", "", "OneWay_Test")
    } else {
      stop("test.type should be either non-parametric or parametric")
    }
  }
  return(list(raw = final, formatted = f.final))
}
