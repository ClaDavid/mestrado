library("e1071")
library("pso")
library("BBmisc")
library(data.table)
source("git/mestrado/fcm.r")

tav <- fread(file = "reuters_pos_tag_final.csv", header = TRUE, check.names = FALSE)

doc_previo <- cmeans(tav, 4)

doc_fcm <- fcm.build(as.matrix(tav), t(doc_previo$membership))

runTecAcuracia <- function(w=rep(1,nrow(dataset)), dataset, u){
  acc = c()
  for(i in 1:10){
    doc_fcm <- fcm.build(as.matrix(tav), t(doc_previo$membership), w)
    uf<-c(); for(i in 1:nrow(ftotal)) uf<-rbind(uf,colSums(matrix(ssc$u[as.logical(ssc$matM[i,]),],sum(ssc$matM[i,]),ncol(ssc$u)))) 
    acc<-c(acc,sum(apply(uf,2,which.max)==labels)/length(labels)*100)
  }
  #u.plot(ssc$u)
  return(mean(acc))
}