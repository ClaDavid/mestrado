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
eq12 <- function(u, dataset){
  v = ((u^2) %*% dataset)/rowSums(u^2)
  return(v)
}
#funcoes de distancias

###no caso, quando for uma matriz no w, 
#tem que pegar a quantidade de colunas ou linhas (ver se ta transposto ou não)
#colocar o valor de w para todas as distancias

dist.vSimples <- function(v, x, w=rep(1,length(x))){
  w=w/sum(w)
  return(sum((x-v)*(x-v)*w))
}

dist.v.antigo <- function(x, v, w=rep(1,nrow(x))){
  dv = c()
  for(i in 1:nrow(v)){
    dv = rbind(dv, dist.vSimples(x, v[i,], w))
  }
  return(dv)
}
dist.xv.antigo <- function(dataset, v, w=rep(1,nrow(dataset))){
  dxv = c()
  for(t in 1:nrow(dataset)){
    dxv = cbind(dxv, dist.v(dataset[t,], v, w))        
  }
  return(dxv)
}

dist.v<-function(v,dataset,w.tudo){
  v.tudo=matrix(v,nrow(dataset),ncol(dataset),byrow=T)
  return(rowSums((dataset-v.tudo)^2*w.tudo))
}

dist.xv <- function(dataset, v, w=rep(1,ncol(dataset))){
  w.tudo=matrix(w/sum(w),nrow(dataset),ncol(dataset),byrow=T)
  dxv=apply(v,1,dist.v,dataset=dataset,w.tudo=w.tudo)
  return(t(dxv))
}


dv.dpord <- function(dataset, v, w=rep(1,nrow(dataset))){
  return((dist.xv(t(dataset), v, w))/(dist.xv(t(dataset), v, w)))
}
#equacao 11
eq11 <- function(u, dataset, v, w=rep(1,nrow(dataset))){
  parte = 1/colSums((dv.dpord(dataset, v, w))^2)
  u = matrix(rep(parte, nrow(u)), nrow(u), ncol(u), byrow = T)
  #print(dim(u))
  return(u)
}

#funcao fcm clustering
fcm.build <- function(dataset, u, w=rep(1,nrow(dataset))){
  for(i in 1:30){
    repeat{
      uant <- u
      v <- eq12(u, dataset)
      u <- eq11(u, dataset, v, w)
      #print((u))
      #print(dim(uant))
      #print(mean(abs((u) - (uant))))
      #7.993123e+307
      if(mean(abs((u) - (uant))) < 0.00001) break
    }    
  }
  ssc <- list(u = u, v = v)
  return(ssc)
}
