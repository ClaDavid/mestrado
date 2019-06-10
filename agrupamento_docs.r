library("e1071")
library("pso")
library("BBmisc")
library(data.table)
source("git/mestrado/fcm.r")

tav <- fread(file = "reuters_pos_tag_final.csv", header = TRUE, check.names = FALSE)

doc_previo <- cmeans(tav, 4)

doc_fcm <- fcm.build(as.matrix(tav), t(doc_previo$membership))


rodei agrupamento -> cada particula tem numero elementos = numero de grupos -> pega essa particula -> e ai roda uma tecnica de classificacao baseada em distancia


-----> silhueta, rand ou jaccard
runTecAcuracia <- function(w=rep(1,nrow(dataset)), dataset, u){
  silhouette = c()
  for(i in 1:10){
    for( i in 2:floor( sqrt( nrow( tav ) ) ) ){
      doc_fcm <- fcm.build(as.matrix(tav), t(doc_previo$membership), w)
      
    }
    
  }
  #u.plot(ssc$u)
  return(mean(silhouette))
}

------> pra comparar, fazer o mesmo processo só que com a distancia euclidiana normal