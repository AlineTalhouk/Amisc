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
  #This function works for categoric data only.
  #If not categorical error
  #determine how many categories in by

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

    tots <- matrix(paste0(addmargins(count,2),"(", per.col,"%)"), byrow = F, nrow = dim(count)[1]) %>%
       set_rownames(c(levels(x)))

    # Missing cases will only be shown if there is one
    if(sum(na.r) != 0){tots <- rbind(tots, Missing = na.r)}

    tots <- tots %>%
        as.data.frame %>%
       cbind(Variable=c(paste0("**",var.lab,"**"),rep("",nrow(.)-1)),Levels=rownames(.),.) %>%
       cbind(., AssociationTest=c(format(round(chisq.test(count, simulate.p.value=simulate.p.value, B=B)$p.value, p.digits), nsmall = p.digits),
                       rep("", nrow(.)-1))) %>%
       set_rownames(NULL) %>%
       set_colnames(c("Variable", "Levels", levels(ind), "Total", "PValue"))
    return(list(count=count,tots=tots))
  }

  # Obtain Summary Data
  ind <- fac.dat[, by]
  res=NULL
for(i in seq_along(fac.var)){
 res<- rbind(res, sumStatsCat(factor(fac.dat[, fac.var[i]]), fac.var[i], fac.label[i], ind, digits)$tots)
}
  return(list(formatted=res))
}

