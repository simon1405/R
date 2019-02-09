gene<-Normalized_geneExpression_HHCsamples
colnames(gene)
x<-read.table("C:/Users/simon/Desktop/Winter Intern/MEDICAL/Liver_ImmGen_results_Ver2_new.txt",header=TRUE,)
dim(x)
dim(gene[,2:163])
colnames(gene)
rownames(x)


newgene<-as.matrix(gene[2:163,])


genename<-gene[,1]
dim(genename)
dim(genecom)
for (i in 1:37582){
  colnames(genecom)[i+6]<-genename[i,1]
}
  genecom<-cbind(x,newgene)
colnames(genecom)
genecom[79,1]

cor(genecom$)
write.csv(genecom,file="genecombined.csv")

cor.test(as.vector(genecom[2:163,2]),as.vector(genecom[2:163,7]))
genecom[4,4]

library(readr)
genecombined <- read_csv("~/genecombined.csv")

dim(genecombined)
geneN<-genecombined[1:78,2:37589]
geneT<-genecombined[79:162,2:37589]

dim(geneN)
colnames(geneN)
pvalue<-matrix(0,nrow=6,ncol=37582)
cortable<-matrix(0,nrow=6,ncol=37582)
for (i in 1:6){
  for (j in 7:37588){
    cor1<c()
    cor1<-cor.test(as.matrix(geneN[,i]),as.matrix(geneN[,j]))
    pvalue[i,j-6]<-cor1$p.value
    cortable[i,j-6]<-cor1$estimate
  }
}
pvalueN<-pvalue
rsqN<-rsq

pvalueT<-matrix(0,nrow=6,ncol=37582)
cortableT<-matrix(0,nrow=6,ncol=37582)
for (i in 1:6){
  for (j in 7:37588){
    cor1<c()
    cor1<-cor.test(as.matrix(geneN[,i]),as.matrix(geneN[,j]))
    pvalueT[i,j-6]<-cor1$p.value
    cortableT[i,j-6]<-cor1$estimate
  }
}







#genes name
gena<-read.delim("C:/Users/simon/Desktop/Winter Intern/MEDICAL/Affy2.0_GeneChip_annotation_data.txt")
newpvaluen<-t(pvalueN)
newpvaluen<-cbind((colnames(genecombined)[8:37589]),newpvaluen)
colnames(newpvaluen)<-c("ID","NavB","MEmB","CD8T","CD4T","NKcell","Monocyte")



library("dplyr")
dict<-select(gena,ID,Gene.symbol)
newpvaluen<-as.data.frame((newpvaluen))
newpvaluen<-right_join(newpvaluen,dict,by="ID")
#newpvaluen$Gene.symbol[which(newpvaluen$Gene.symbol=="")]<-"not available"
write.csv(newpvaluen,"pvalue with normal samples")

m1st<-read.delim("C:/Users/simon/Desktop/Winter Intern/MEDICAL/node.txt")
firsttry<-as.data.frame(m1st$ID)
colnames(firsttry)<-"Gene.symbol"
result1st<-inner_join(firsttry,newpvaluen,by="Gene.symbol")





#



newpvaluet<-t(pvalueT)
newpvaluet<-cbind((colnames(genecombined)[8:37589]),newpvaluet)
colnames(newpvaluet)<-c("ID","NavB","MEmB","CD8T","CD4T","NKcell","Monocyte")
newpvaluet<-as.data.frame((newpvaluet))
newpvaluet<-right_join(newpvaluet,dict,by="ID")
write.csv(newpvaluet,"pvalue with tumors samples")

tresult1st<-inner_join(firsttry,newpvaluet,by="Gene.symbol")

m2nd<-read.delim("C:/Users/simon/Desktop/Winter Intern/MEDICAL/node2.txt")
firsttry<-as.data.frame(m2nd$Approved.Symbol)
colnames(firsttry)<-"Gene.symbol"
result2nd<-inner_join(firsttry,newpvaluet,by="Gene.symbol")

m3rd<-read.delim("C:/Users/simon/Desktop/Winter Intern/MEDICAL/node3.txt")
firsttry<-as.data.frame(m3rd$Approved.Symbol)
colnames(firsttry)<-"Gene.symbol"
result3rd<-inner_join(firsttry,newpvaluet,by="Gene.symbol")

library("ggplot2")
ggplot(data=newpvaluet,aes(x=c("NavB","MEmB","CD8T","CD4T","NKcell","Monocyte"),y=ID))+geom_tile(aes(fill=pvalueT))

write.csv(tresult1st,"pvalue with tumor samples comparing to Interleukins (IL).csv")
write.csv(result2nd,"pvalue with tumor samples comparing to Interferons (IFN).csv")
write.csv(result3rd,"pvalue with tumor samples comparing to Chemokine ligands (CCL).csv")
