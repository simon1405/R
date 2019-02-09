#bio-3/5
library(readr)
drvis_ <- read_csv("E:/bio/drvis .csv")
colnames(drvis_)
drvis<-drvis_[,14:15]
colnames(drvis)<-c("chroname","intesite")

#gwas<-read.table("E:/bio/gwas_catalog_v1.0-studies_r2018-02-28.tsv",sep="\t")
gwas1<-read_tsv("E:/bio/gwas_catalog_v1.0-associations_e91_r2018-02-28.tsv")
colnames(gwas1)
gwas<-(gwas1[,12:13])
colnames(gwas)

nrow(gwas)
library(dplyr)
gwas<-arrange(gwas,CHR_ID)

g<-group_by(gwas,CHR_ID)
summ<-summarise(g,count=n())
summ
dictg<-c()
databaseg<-c()
gwas

  for (j in 1:22){
    databaseg[j]<-gwas[which(gwas$CHR_ID==as.character(j)),2]
    }

databaseg[23]<-gwas[which(gwas$CHR_ID=="X"),2]
databaseg[24]<-gwas[which(gwas$CHR_ID=="Y"),2]
databaseg
substring(drvis$chroname[1],1,1)
datadr<-c()

lookfor<-function(x,y){
  out<-0
  for (i in 1:length(databaseg[[x]])){
  if ((is.numeric(as.numeric(databaseg[[x]][i])))||(is.numeric(as.numeric(y)))){return(out)}  
  if (abs(as.numeric(y)-as.numeric(databaseg[[x]][i])<=100)){out<-1
  return(out)}  
  }
  return(out)
}


 

result<-c()
dictdr<-c()
dict2<-c(as.character(rep(10:23)))
dict1<-c(as.character(rep(1:9)))
  for (i in 1:nrow(drvis)){
    if (is.na(drvis$chroname)==F){
      init2<-substring(drvis$chroname[i],1,2)
      init1<-substring(drvis$chroname[i],1,1)
      if (init2 %in% dict2){
      dictdr[as.numeric(init2)]<-c(dictdr[as.numeric(init2)],drvis$intesite[i])
        }
      if ((init1 %in% dict1)&&(init2 %in% dict2==F)) {
        dictdr[as.numeric(init1)]<-c(dictdr[as.numeric(init1)],drvis[i,1])}
    }
  }

dictdr
drvis
