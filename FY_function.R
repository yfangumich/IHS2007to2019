# packages
package.check <- function(pacname){
  if (!require(pacname,character.only = TRUE)) install.packages(pacname)
  library(pacname,character.only = T)
}

# Time zone ####
Sys.setenv(TZ='UTC')

# I/O ####
readtable <- function(path){
  return(read.table(path,header=T,stringsAsFactors = F))
}
writetable <- function(file,path){
  write.table(file,path,quote=F,row.names=F,col.names=T)
}
readcsv <- function(path){
  return(read.csv(path,header=T,stringsAsFactors = F))
}
writecsv <- function(file,path){
  write.table(file,path,quote=F,row.names=F,col.names=T,sep=',')
}
readtsv <- function(path){
  return(read.table(path,header=T,stringsAsFactors = F,sep="\t"))
}

# Data ####
inversenormal <- function(x) {
  return(qnorm((rank(x,na.last="keep")-0.5)/sum(!is.na(x))))}

se <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))

addsmallnum <- function(ds,seednum){
  set.seed(seednum)
  idx <- (ds==0 & !is.na(ds))
  toadd <- runif(sum(idx),0,0.00001)
  ds[idx] <- ds[idx]+toadd
  return(ds)
}

stderr <- function(x, na.rm=FALSE) {
  if (na.rm) x <- na.omit(x)
  sqrt(var(x)/length(x))
}

