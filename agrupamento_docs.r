library("e1071")
library("pso")
library(data.table)
library(fclust)
source("fcm.r")
source("pso.r")
library("reticulate")

tav <- fread(file = "Documentos/mestrado/reuters_pos_tag_final.csv", header = TRUE, check.names = FALSE)


#rodei agrupamento -> cada particula tem numero elementos = numero de grupos -> pega essa particula -> e ai roda uma tecnica de classificacao baseada em distancia

#-----> silhueta, rand ou jaccard
runTecAcuracia <- function(w=rep(1,nrow(tav))){
  silhouette = c()
 for(i in 1:10){
   # da saltos 2 em 2, 3 em 3, 4 em 4
  #for( i in 2:floor( sqrt( nrow( dataset ) ) ) ){
   for (i in seq(2, floor( sqrt( nrow( tav ) ) ), 4)){
      # colocar timer aqui
      doc_previo <- cmeans(tav, i)
      # coloca timer aqui
      doc_fcm <- fcm.build(as.matrix(tav), t(doc_previo$membership), w)
      # coloca timer aqui
      silhouette = cbind(silhouette, SIL.F(Xca = tav, U = t(doc_fcm$u), alpha = 0))
      #print(silhouette)
   }
 }
  #u.plot(ssc$u)
  return(max(silhouette))
}

#ps perguntar porque alpha tem que ser 0

##PSO
#para a iris é o numero de atributos
#colocar timer aqui
start_time <- Sys.time()
#pso1 <- psoptim(rep(NA,ncol(tav)), runTecAcuracia, lower=0, upper=1, dataset=tav, control=list(maxit=100, trace=1, REPORT = 1,  trace.stats = FALSE, s = 100, c.p = 1.49618, c.g = 2, w = c(0.4, 0.9)))
#pso1 = pso.m(input = rep(NA,ncol(tav)), part.size = 100, part.max = c(0.4, 0.9), fit.calc.func = runTecAcuracia, part2param = tav)
end_time <- Sys.time()
print(end_time - start_time)



psolib <- import_from_path("pso")
numpylib <- import("numpy")

consts_list = list(0.7, 1.4, 1.4)
consts_list_in_python = r_to_py(consts_list)

start_time <- Sys.time()
pso1 = psolib$run_pso(eval_func = runTecAcuracia, consts = consts_list_in_python)
end_time <- Sys.time()

print(end_time - start_time)

x0 = numpylib$random$uniform(low = 0, high = 1,  size = ncol(tav))
options_pso1 = py_dict(keys = 'disp', values = T, convert = FALSE)
pso1 = psolib$minimize(fun = runTecAcuracia, x0 = x0, args = tav)


psolib <- import("pyswarms")

options = py_dict(keys = c('c1', 'c2', 'w'), values = c(0.5, 0.3, 0.3), convert = FALSE)

optimizer = psolib$global_best$GlobalBestPSO(n_particles = 100, dimensions = 1774, options = options)


####fitness=0.001343
#pso1 <- optim_ppso_robust(objective_function = runTecAcuracia(lower=0, upper=1, numDicas=numDicas, alfa=alfa, ftotal=ftotal, dataset=dataset, delta=delta, u=u, labels=labels), number_of_particles=500,max_number_of_iterations=100,  parameter_bounds = rep(NA,ncol(dataset)), C1 = 1.49618, C2 = 2, w = c(0.4, 0.9), nslaves=2, projectfile=NULL, logfile=NULL)

#o resultado vai ser o pdar
#numDicas = floor(nrow(dataset)/2)
particula = pso1$par
print(particula)

retiraPerdedores<-function(dataset,particula,frac=0.1){
  ordem<-order(particula)
  retirarnum=round(length(ordem)*frac)
  return(dataset[,-ordem[1:retirarnum]])
}

res1 <- retiraPerdedores(dataset, particula, frac=0.1)
res2 <- retiraPerdedores(dataset, particula, frac=0.2)
res3 <- retiraPerdedores(dataset, particula, frac=0.3)
res4 <- retiraPerdedores(dataset, particula, frac=0.4)

################---->ALTERAR CAMINHO<----------##############
write.csv(res1, file = "Resultado1.csv")
write.csv(res2, file = "Resultado2.csv")
write.csv(res3, file = "Resultado3.csv")
write.csv(res4, file = "Resultado4.csv")




#------> pra comparar, fazer o mesmo processo só que com a distancia euclidiana normal