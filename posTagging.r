library("stringr")
library("NLP")
library("openNLP")
library("kernlab")
library("reshape2")
library("tidyr")
library("e1071")
library("ppclust")
library("fclust")
#install.packages("stringdist")
library("stringdist")

# NAO ESQUECER A SEMENTE
#set.seed(42)

#significados:  
#https://www.sketchengine.eu/english-treetagger-pipeline-2/
#https://www.sketchengine.eu/modified-penn-treebank-tagset/

posText<- "I gave him my heart, and he took and pinched it to death; and flung it back to me.
           People feel with their hearts, Ellen, and since he has destroyed mine, I have not power to feel for him."

tagPOS <-  function(x, ...) 
{
  s <- as.String(x)
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- Annotation(1L, "sentence", 1L, nchar(s))
  a2 <- annotate(s, word_token_annotator, a2)
  a3 <- annotate(s, Maxent_POS_Tag_Annotator(), a2)
  a3w <- a3[a3$type == "word"]
  POStags <- unlist(lapply(a3w$features, `[[`, "POS"))
  POStagged <- paste(sprintf("%s/%s", s[a3w], POStags), collapse = " ")
  list(POStagged = POStagged, POStags = POStags)
}
data(reuters)
reuters

tokensTag <- lapply( reuters, tagPOS )
tokensTag

tokens <- c()

for( i in 1:length(reuters) )
{
    tokens <- rbind( tokens, lapply( lapply( tokensTag[[ i ]]$POStagged, unlist ), paste, collapse = '' ) )
}

tokens <- apply( tokens, 1, as.character )

#tokens <- paste( unlist(tokensTag[1]), collapse='')
words <- strsplit( tokens, " ", fixed = T )
#words <- unlist(words)
words <- lapply( words, unlist )
#counts <- table(words)
counts <- lapply( words, table )

listaToMatrix <- melt( counts )

tav <- spread( listaToMatrix, Var1, value, fill = 0 )
tav$L1 <- NULL

# H = objx$d
dim(fuzzyFeature$v)



#############################
silhoutte_final <- 0
d = NULL

start.time <- Sys.time()
for( i in 2:floor( sqrt( nrow( t(tav) ) ) ) )
{
<<<<<<< HEAD
  fuzzyFeature <- fcm(t(tav), centers = i, nstart = 10)
  result.fcm <- ppclust2(fuzzyFeature, "fclust")
  index_silhouette_fuzzy <- SIL.F(result.fcm$Xca, result.fcm$U)
  #silhoutte_final <- index_silhouette_fuzzy
  if( index_silhouette_fuzzy < 0.4 ) break
  d = rbind(d, data.frame(i, index_silhouette_fuzzy))
=======
  #fuzzyFeature <- fcm(t(tav), centers = i)  
  #result.fcm <- ppclust2(fuzzyFeature, "fclust")
  result.fcm <- FKM(t(tav), k = i)
  index_silhouette_fuzzy <- XB(result.fcm$Xca, result.fcm$U, result.fcm$H, result.fcm$m)
  #silhoutte_final <- index_silhouette_fuzzy
  if( index_silhouette_fuzzy < 0.4 ) break
    d = rbind(d, data.frame(i, index_silhouette_fuzzy))
>>>>>>> b145735a5d5a3ecb72c28fee69bd6b8755fbd2b9

  #if( silhoutte_final < index_silhouette_fuzzy ) silhoutte_final <- index_silhouette_fuzzy 
}
end.time <- Sys.time()
time.taken <- end.time - start.time

<<<<<<< HEAD

############################ Applying f cmeans to docs

fuzzyDocuments <- fcm(tav, centers = 3, nstart = 10)
=======
write.csv(d, file = "Silhueta_Features_Teste_2.csv", row.names = FALSE)
plot(d$i, d$index_silhouette_fuzzy, type = "o")
axis(side=1, at=c(2:(length(d$i) + 1)))

summary(fuzzyFeature)


cat("Fuzzy Silhouette Index: ", silhoutte_final)
>>>>>>> b145735a5d5a3ecb72c28fee69bd6b8755fbd2b9

#############################

#################################################################

## Distance between two points
## Before: dist.vSimples
#distance_between_two_points <- function( point, centroid ){
#  return( sum( (point - centroid)*(point - centroid) ) )
#}

## Before dist.v
#distance.centroid <- function( point, centroid ){
#  distance_centroid = c()
  
#  for( i in 1:nrow(centroid) )
#  {
#    distance_centroid = rbind( distance_centroid, distance_between_two_points( point, centroid[i,] ) )
#  }
  
#  return( distance_centroid )
#}

## Before dist.xv
#distance.dataset_to_centroid <- function( dataset, centroid ){
#  distance_dataset_centroid = c()
  
#  for( t in 1:nrow(dataset) )
#  {
#    distance_dataset_centroid = cbind( distance_dataset_centroid, distance.centroid( dataset[t,], centroid ) )
#  }
#  return( distance_dataset_centroid )
#}

## Before dv.dpord
#distance_centroid_dataset_to_dataset <- function( dataset, centroid, w=rep(1,nrow(dataset))){
#  return( ( distance.dataset_to_centroid( t(dataset), centroid ) ) / ( distance.dataset_to_centroid( t(dataset), centroid ) ) )
#}

## Distance between two points


distance_between_two_points <- function( point_1, point_2)
{
    return( sqrt( sum( ( point_1 - point_2 )^2 ) ) )
}

## Weighted Average Distance

average_distance <- function( point, others_points, weight = rep( 1, number_of_clusters ) )
{
    distances <- c()
    weight <- weight / sum(weight)
    
    for( i in 1:nrow( others_points ) )
    {
        distances <- distance_between_two_points( point, i )
    }
    return( sum(distances * weight) / length(distances) )
}

# number_of_clusters = 3

for( cluster_quantity in 2:floor( sqrt( nrow(tav) ) ) )
{
    clustering_feature <- cmeans( t(tav), cluster_quantity )
    
}


clustering_documents <- cmeans(tav)

featureOptimization.function <- function( tav, w=rep(1,nrow(dataset)) )
{
  
  fuzzyFeature <- cmeans(t(tav), 2)
  
  
}

#agrupamento de features
fuzzyFeature <- cmeans(t(tav), 2)

#Tecnica SSC
#funcao de construcao da matriz p
p.build<-function(labels, cluster){
  p<-matrix(0,length(unique(labels)),3)
  for(i in 1:length(unique(labels))){
    vetorMaiores<-cluster[which(labels==i)]
    #print(222222222)
    #print(length(unique(cluster)))
    for(j in 1:length(unique(cluster)))
      p[i,j]<-length(which(vetorMaiores==j))
  }
  return(p)
}

#funcao de construcao de matriz P em porcentagem
ppercentage.build <- function(labels, cluster){
  matrizP <- p.build(labels, cluster)
  return((matrizP*100)/rowSums(matrizP))
}

#contrucao da matriz m
m.build <- function(matP){
  m = matP
  m[which(apply(m, 2, function(x) x != max(x,na.rm=TRUE)))] <- 0
  m[which(apply(m, 2, function(x) x == max(x,na.rm=TRUE)))] <- 1
  return(m)
}

#construcao da matriz f
ftotal.build <- function(labels){
  ftotal <- matrix(0, length(unique(labels)), length(labels))
  for(i in 1:length(unique(labels))) ftotal[i,which(labels==i)]=1
  return(ftotal)
}

#funcao que escolhe os quantas e quais serao as dicas
f.random<- function(f, num){
  return(sample(ncol(f), num))
}

#funcao que constroi o delta com as dicas
delta.build <- function(fdicas,labels){
  delta <- c(rep(0, length(labels)))
  delta[fdicas] <- 1  
  return(delta)
}

#equacao 13
eq13 <- function(util, utilant, beta, delta, f.dicas, matM){
  #return(utilant + (t(matM)%*%((2*beta*delta)*(f.dicas - (matM%*%util)))))
  #print(dim(matM))
  #print(dim(util))
  #print(util)
  parte1 = (matM%*%util)
  #print(parte1)
  return(utilant + (t(matM)%*%((2*beta*delta)*(f.dicas - parte1))))
}
#funcao de multiplicacao de vetor por matriz
mult.vectormatrix <- function(matrix, vector){
  return(rowSums(t(t(matrix)*vector)))
}
#funcao de multiplicacao de matriz por matriz
multmatrix.geral <- function(matrix1, matrix2){
  matrix.mult <- c()
  for(i in 1:nrow(matrix1)){
    matrix.mult <- rbind(matrix.mult, mult.vectormatrix(matrix2, matrix1[i, ]))
  }
  return(matrix.mult)
}
#equacao 12
eq12 <- function(u, alfa, util, dataset){
  return((((u^2) + alfa*((u - util)^2))%*%dataset)/rowSums((u^2) + alfa*((u - util)^2)))
}
#funcoes de distancias
#dist.vSimples <- function(x, v){
#  return(t(x-v)%*%(x-v))
#}

###no caso, quando for uma matriz no w, 
#tem que pegar a quantidade de colunas ou linhas (ver se ta transposto ou nÃ£o)
#colocar o valor de w para todas as distancias

dist.vSimples <- function(x, v, w=rep(1,length(x))){
  w=w/sum(w)
  return(sum((x-v)*(x-v)*w))
}

dist.v <- function(x, v, w=rep(1,nrow(x))){
  dv = c()
  for(i in 1:nrow(v)){
    dv = rbind(dv, dist.vSimples(x, v[i,], w))
  }
  return(dv)
}
dist.xv <- function(dataset, v, w=rep(1,nrow(dataset))){
  dxv = c()
  for(t in 1:nrow(dataset)){
    dxv = cbind(dxv, dist.v(dataset[t,], v, w))        
  }
  return(dxv)
}
dv.dpord <- function(dataset, v, w=rep(1,nrow(dataset))){
  return((dist.xv(t(dataset), v, w))/(dist.xv(t(dataset), v, w)))
}
#equacao 11
eq11 <- function(u, util, alfa, dataset, v, w=rep(1,nrow(dataset))){
  parte1 = (alfa*util)/(1+alfa)
  parte2 = 1-((alfa/(1+alfa))*(colSums(util)))
  parte3 = parte2/colSums(dv.dpord(dataset, v, w))
  u = parte1 + matrix(rep(parte3, nrow(util)), nrow(util), ncol(util), byrow = T)
  return(u)
}
#=matrix(c(1,0,0,0,1,0,0,0,1), nrow = 3, ncol = 3, byrow = TRUE)

#funcao semi-supervisionado clustering
ssc.build <- function(dataset, f.dicas, delta, u, labels, alfa, w=rep(1,nrow(dataset))){
  util = u
  cluster = apply(u,2,which.max)
  #print(cluster)
  #print(labels)
  #print(length(cluster))
  matP <- p.build(labels, cluster)
  matM <- m.build(matP)
  #print(dim(matP))
  #print(dim(matM))
  beta = 0.06
  for(i in 1:30){
    repeat{
      utilant <- util
      util <- eq13(util, utilant, beta, delta, f.dicas, matM)
      cluster = apply(util,2,which.max)
      #print(cluster)
      matP <- p.build(labels, cluster)
      matM <- m.build(matP)
      #print(matM)
      #print(mean(abs((util) - (utilant))))
      if(max(util) >= 7.993123e+307) break
      else if(mean(abs((util) - (utilant))) < 0.00001) break
    }
    repeat{
      uant <- u
      v <- eq12(u, alfa, util, dataset)
      u <- eq11(u, util, alfa, dataset, v, w)
      #print((u))
      #print(dim(uant))
      #print(mean(abs((u) - (uant))))
      #7.993123e+307
      if(mean(abs((u) - (uant))) < 0.00001) break
    }    
  }
  ssc <- list(u = u, util = util, v = v, matM = matM)
  return(ssc)
}
#apply(u,2,which.max)


runTecAcuracia <- function(w=rep(1,nrow(dataset)), numDicas, alfa, ftotal, dataset, delta, u, labels){
  acc = c()
  for(i in 1:10){
    f.rand = f.random(ftotal, numDicas)
    delta <- delta.build(f.rand,labels)
    f.dicas[ , f.rand] <- ftotal[ , f.rand]
    ssc <- ssc.build(dataset, f.dicas, delta, u, labels, alfa, w)
    uf<-c(); for(i in 1:nrow(ftotal)) uf<-rbind(uf,colSums(matrix(ssc$u[as.logical(ssc$matM[i,]),],sum(ssc$matM[i,]),ncol(ssc$u)))) 
    acc<-c(acc,sum(apply(uf,2,which.max)==labels)/length(labels)*100)
  }
  #u.plot(ssc$u)
  return(mean(acc))
}

#Atribuicao de pesos aos grupos



#todos os grupos de atributos comeÃ§am com peso 1

#agrupamento de documentos
fuzzy <- cmeans(tav, 2)







#####

f <- function(x) {
  pr <- unlist(
    lapply(
      strsplit(x, ' '), 
      function(i) combn(sort(i), 2, paste, collapse=' ')
    )
  )
  
  tbl <- table(pr)
  
  d <- do.call(rbind.data.frame, strsplit(names(tbl), ' '))
  names(d) <- c('word1', 'word2')
  d$Freq <- tbl
  
  d
}


#NNP e IN, NN e DT



##quanteda
mytexts <- c("The new law included a capital gains tax, and an inheritance tax.",
             "New York City has raised taxes: an income tax and inheritance taxes.")
mytoks <- tokens(mytexts, remove_punct = TRUE)
mydict1 <- dictionary(list(tax=c("tax", "income tax", "capital gains tax", "inheritance tax*")))
(cw2 <- tokens_compound(mytoks, mydict1))

tagPOS(cw2)
