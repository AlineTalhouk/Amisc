unitestsCont <- function(num.dat,num.var,num.label, by,
                         digits=1, showMissing){
#This function works for numeric data only. If not continuous error
# determine how many categories in by
if(is.factor(num.dat[,by])){
  p <- length(levels(num.dat[,by]))
}else{
  stop('by variable must be factor')
}

k <- length(num.var)
# functions used
sumStatsCont <- function(x) {
  c(Mean= mean(x, na.rm = T), SD=sd(x, na.rm=T),Median=round(median(x,na.rm = T), digits), missing=sum(is.na(x)))}

# Obtain Summary Data
ind <- num.dat[, by]
resCont <- apply(num.dat[,num.var],2, function(x) by(x,ind, sumStatsCont))

# Compute Statistical test
 para <- apply(num.dat[,num.var], 2, function(x) round(oneway.test(x ~ ind)$p.value,3))

final <- matrix(unlist(resCont), byrow=T, ncol=4)%>%
  rbind(., unname(t(apply(num.dat[,num.var], 2, sumStatsCont))))%>%
  set_colnames(c("Mean","SD","Median","Missing"))%>%
 data.frame(num.var=c(sort(rep(num.var,p)),num.var),
            by=c(rep(levels(ind),k),rep("Total",k)),.) %>%
  arrange(.,num.var)

if(showMissing==FALSE){final <- final[,!names(final)%in%c("Missing")]}

f.final <- final %>%
  mutate(MeanSD = paste(round(Mean, digits), round(SD, digits),sep = " &#177; ")) %>%
  select(-c(Mean, SD))%>%
  melt(., id=c("num.var","by"))%>%
  dcast(., num.var+ relevel(variable,ref = "MeanSD") ~ by)%>%
  set_colnames(c("Variable", "Levels", levels(final$by)))


f.final <- f.final[,c("Variable","Levels",levels(num.dat[, by]),"Total")]

f.final$Variable<- ifelse(mod(1:nrow(f.final),ifelse(showMissing,3,2))==1,paste0("**",f.final$Variable,"**"),"")
f.final$PValue<- ifelse(mod(1:nrow(f.final),ifelse(showMissing,3,2))==1,format(round(para, digits = 3),nsmall = 3),"")

return(list(raw=final, formatted=f.final))
}

