unitestsCat <- function(fac.dat,fac.var,by,
                         per="col", digits=1){
  #This function works for categoric data only.
  #If not categorical error
  #determine how many categories in by

  if(is.factor(fac.dat[,by])){
    p <- length(levels(fac.dat[,by]))
  }else{
    stop('by variable must be factor')
  }

  k <- length(fac.var)


  # functions used
  sumStatsCat <- function(x,var, ind, digits) {
    x <- droplevels(x)
    ind <- droplevels(ind)
    count <- table(x,ind)
    na.r <- addmargins(table(x,ind, useNA = "always"))[length(levels(x))+1,-p-1]
    per.row <- prop.table(count,margin =1)
    per.col <- round(prop.table(addmargins(count,2), margin=2)*100,digits)
    tots <- matrix(paste0(addmargins(count,2),"(", per.col,"%)"), byrow=F,
                    nrow=dim(count)[1])%>%
       set_rownames(c(levels(x)))%>%
       rbind(Missing=na.r)%>%
       as.data.frame%>%
       cbind(Variable=c(paste0("**",var,"**"),
                        rep("",length(levels(x)))),Levels=rownames(.),.)%>%
      cbind(., AssociationTest=c(format(round(chisq.test(count)$p.value,3),nsmall=3),
                       rep("",length(levels(x)))))%>%
       set_rownames(NULL)%>%
       set_colnames(c("Variable","Levels",levels(ind),"Total","PValue"))
      return(list(count=count,tots=tots))
    }

  # Obtain Summary Data
  ind <- fac.dat[, by]
  res=NULL
for(i in seq_along(fac.var)){
 res<- rbind(res,sumStatsCat(fac.dat[,fac.var[i]], fac.var[i],ind, digits)$tots)
}
  return(list(formatted=res))
}

