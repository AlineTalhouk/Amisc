#' some documentation
#'
#' a function to do something
#'
#' this function will do something
#'
#' @param some parameters
#' @return possibly something
#' @author Aline Talhouk
#' @export
#' @examples TODO
unitestsCat <- function(fac.dat, fac.var, fac.label, by,
                        per="col", digits=1, p.digits=3, showMissing,
                        simulate.p.value = FALSE, # for chisq.test
                        B = 2000 # for chisq.test
){
  # This function takes a data.frame(fac.dat) of categorical columns as well as a factor column, and returns a statistical summary of df based on the factor column in fac.dat
  #
  # Args:
  #   fac.dat: A data.frame of all categorical variables and one factor variable
  #   fac.var: Names of selected categorical variables
  #   fac.label: Same as fac.var
  #   by: Same factor column as by1 in describeBy
  #   ShowTotal: When set to be TRUE, it show total number of each factor
  #   showMissing: It's always set to be TRUE, but the missing ones will be shown only if there is one
  #
  #
  # Returns: A list summary data.frame summarises descriptives statistics for the input data.frame(fac.dat)


  if(is.factor(fac.dat[, by])){
    p <- length(levels(fac.dat[, by]))
  }else{
    stop('by variable must be factor')
  }

  # Main functions used to obtain the marginal totals, which are the total counts of the cases over the categories of interest
  sumStatsCat <- function(x, var, var.lab, ind, digits) {
    x <- droplevels(x) # drop unused levels from a factor
    ind <- droplevels(ind)
    count <- table(x, ind, dnn = list(var, by))
    na.r <- addmargins(table(x,ind, useNA = "always"))[length(levels(x)) + 1, -p-1]
    per.row <- prop.table(count, margin =1)
    per.col <- round(prop.table(addmargins(count,2), margin = 2)*100, digits)

    tots <- matrix(paste0(addmargins(count, 2),"(", per.col,"%)"), byrow = F, nrow = dim(count)[1]) %>%
      set_rownames(c(levels(x)))

    # Missing cases will only be shown if showMissing == TRUE and there are indeed missing ones
    if(sum(na.r) != 0 && showMissing == TRUE) {
      tots <- rbind(tots, Missing = na.r)
    }
    # re-arrange the matrix so that it will be able to rbind with continuous part
    rowname <- rownames(tots)
    tots <- tots[, c(ncol(tots), 1: ncol(tots) - 1)]

    tots <- as.data.frame(matrix(tots, ncol = length(levels(ind)) + 1), row.names = rowname) %>%
      cbind(Variable=c(paste0("**",var.lab,"**"), rep("",nrow(.)-1)), Levels=rownames(.),.) %>%
      cbind(., AssociationTest=c(format(round(chisq.test(count, simulate.p.value = simulate.p.value, B = B)$p.value, p.digits), nsmall = p.digits), rep("", nrow(.)-1)))
    %>% set_rownames(NULL) %>% set_colnames(c("Variable", "Levels", "Total", levels(ind), "PValue"))  %>% mutate_all(as.character)

    return(list(count=count,tots=tots))
  }
  # Obtain Summary Data
  ind <- fac.dat[, by]
  res = NULL
  for(i in seq_along(fac.var)){
    res<- rbind(res, sumStatsCat(factor(fac.dat[, fac.var[i]]), fac.var[i], fac.label[i], ind, digits)$tots)
  }
  return(list(formatted=res))
}