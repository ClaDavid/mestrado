library("e1071")
library("pso")
library(data.table)
library(fclust)
source("fcm.r")

tav <- fread(file = "reuters_pos_tag_final.csv", header = TRUE, check.names = FALSE)


#rodei agrupamento -> cada particula tem numero elementos = numero de grupos -> pega essa particula -> e ai roda uma tecnica de classificacao baseada em distancia

#-----> silhueta, rand ou jaccard
runTecAcuracia <- function(w=rep(1,nrow(dataset)), dataset){
  silhouette = c()
  for(i in 1:10){
    for( i in 2:floor( sqrt( nrow( dataset ) ) ) ){
      doc_previo <- cmeans(dataset, i)
      doc_fcm <- fcm.build(as.matrix(dataset), t(doc_previo$membership), w)
      silhouette = cbind(silhouette, SIL.F(Xca = dataset, U = t(doc_fcm$u), alpha = 0))
      print(silhouette)
    }
  }
  #u.plot(ssc$u)
  return(mean(silhouette))
}

#ps perguntar porque alpha tem que ser 0

#------> pra comparar, fazer o mesmo processo só que com a distancia euclidiana normal