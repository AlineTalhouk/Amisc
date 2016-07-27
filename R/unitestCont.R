unitestsCont <- function(num.dat,num.var,num.label, by,
                         digits=1, p.digits=3, showMissing, test.type = "parametric"){
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
  c(Mean= mean(x, na.rm = T), SD=sd(x, na.rm=T), SEM=sd(x, na.rm=T)/sqrt(sum(!is.na(x))), Median=round(median(x,na.rm = T), digits),n=length(!is.na(x)),missing=sum(is.na(x)))}


# Obtain Summary Data
ind <- num.dat[, by]
resCont <- apply(num.dat[,num.var],2, function(x) by(x,ind, sumStatsCont))

# Compute Statistical test
para <- apply(num.dat[,num.var], 2, function(x) round(oneway.test(x ~ ind)$p.value,p.digits))
nonpara <- apply(num.dat[,num.var], 2, function(x) round(kruskal.test(x ~ ind)$p.value,p.digits))
if(test.type=="non-parametric"){
  test <- nonpara
} else{
  test <- para
}

# since the codes below e.g. arrange, melt, dcast will order the 
# results by num.label, we need to change num.label as a factor
# and manually set the level so that the order of num.label may 
# be preserved
num.label <- factor(num.label, levels=num.label)

final <- matrix(unlist(resCont), byrow=T, ncol=6)%>%
  rbind(., unname(t(apply(num.dat[,num.var], 2, sumStatsCont))))%>%
  set_colnames(c("Mean","SD","SEM","Median","N","Missing"))%>%
  data.frame(  
    num.var=c(rep(num.label,each=p),num.label),
    by=c(rep(levels(ind),k),rep("Total",k)),.) %>%
  arrange(.,num.var)

if(showMissing==FALSE){final <- final[,!names(final)%in%c("Missing")]}

f.final <- final %>%
  mutate("Mean (SD)" = paste(round(Mean, digits), " &#177; ", round(SEM, digits), " (",round(SD, digits),")", sep = "")) %>%
  select(-c(Mean, SEM, SD))%>%
  melt(., id=c("num.var","by"))%>%
  dcast(., num.var+ relevel(variable,ref = "Mean (SD)") ~ by)%>%
  set_colnames(c("Variable", "Levels", levels(final$by)))

f.final <- f.final[,c("Variable","Levels",levels(num.dat[, by]),"Total")]

# since num.label is a factor, need to put back the actual character name
final$num.var <- num.label[final$num.var]
f.final$Variable <- num.label[f.final$Variable]

f.final$Variable<- ifelse(mod(1:nrow(f.final),ifelse(showMissing,4,3))==1,paste0("**",f.final$Variable,"**"),"")
f.final$PValue<- as.vector(rbind(
  format(round(test, digits = p.digits),nsmall = p.digits),
  matrix(rep("",ifelse(showMissing,3,2)*length(test)),ncol=length(test))
))

return(list(raw=final, formatted=f.final))
}

