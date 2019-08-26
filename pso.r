#_______________________________________________________________________________
# Program: PSO
# ------------------------------------------------------------------------------
# Designer: Andre Luis D. Rossi
# Email: alrossi@icmc.usp.br
# ------------------------------------------------------------------------------
# Last update: 13/07/2007
# Description:
# Este programa implementa o PSO para otimizar os valores dos parametros livres 
# das SVMs. Utiliza o resultado de classificacao destas como funcao de aptidao
# ------------------------------------------------------------------------------
# O algoritmo foi implementado com as modificacoes segundo esse trabalho:
#@InProceedings{ shi1998,
#	title = {A Modified Particle Swarm Optimizer},
#	author = {Yuhui Shi and Russell Eberhart}, 
#	booktitle = {Proceedings of the IEEE International Conference on Evolutionary Computation},
#	pages = {69-73},     
#	year = {1998},
#	address = {Anchorage, Alaska},
#	doi = {10.1109/ICEC.1998.699146}
#}
#_______________________________________________________________________________


#algoritmo PSO

#source("configuracao_pso_debora.R")
max.iteracoes <- 3; # numero maximo de iteracoes
		
#C1<-2;C2<-2;w<-1.05
#C1 <- 1.494   
#C2 <- 1.494  #constantes de aceleracao
###C1 <- 1.6   
###C2 <- 1.6  #constantes de aceleracao                                     
#w <- 0.729 

#C1<-0.8;C2<-0.8;w<-0.6

#C1<-1.193;C2<-C1;w<-0.721
#C1.orig<-1.193;C2.orig<-C1.orig    

C1<-2;C2<-C1;
w<-0.9
C1.orig<-2;C2.orig<-C1.orig;w.max<-0.9;w.min<-0.2    

#	// According to Clerc's Stagnation Analysis
#	param.w = 1 / (2 * log ((double) 2)); // 0.721
#	param.c = 0.5 + log ((double) 2); // 1.193
#

##################################################################################################
## GERENCIAMENTO (PRINCIPAL) DO ALGORITMO PSO
##################################################################################################
pso.w <- function(input,part.size,part.max,fit.calc.func,part2param){

  pop.size <- 100     # tamanho da populacao

#  vmax <- c(0.7*part.max[,2]); #a velocidade e um fator de 0.6 em relacao a posicao
  vmax <- c(part.max[,2]); #a velocidade e um fator de 0.6 em relacao a posicao

   #uma particula e formada pela sua posicao, velocidade e aptidao.
   #uma particula e capaz de 'lembrar' de suas melhores posicoes e respectivas velocidades
   particula <- list(posicao=matrix(0, pop.size, part.size), 
         velocidade=matrix(0,pop.size, part.size), 
         aptidao=vector('numeric', pop.size) + Inf,
         melhores.aptidoes=c(),
         melhor=list(posicao=matrix(0, pop.size, part.size),
         aptidao=vector('numeric', pop.size) + Inf)
   );
   #melhor posicao e aptidao globais
   melhor.global <- list(posicao=vector('numeric', part.size), aptidao=+Inf);

   #gera uma populacao inicial aleatoriamente
#   source(pop.fixed)
     populacao <- gera.pop(pop.size,part.size,part.max,vmax)
#     dump(c("populacao"),paste("pop1-",proc.time()[3],".txt",sep=""))

#if(input$represent=="real"){
##     init.pop<-rbind(populacao$posicao[1:(pop.size/2),],init.pop.gen4(pop.size/2,input$cluster.n,input$dataset,input$weight,input$ds.sd,input$wi.sum)$pop);dump(c("init.pop"),paste("init-pop-",proc.time()[3],".txt",sep=""))
#  init.pop<-init.pop.gen4(pop.size,input$cluster.n,input$dataset,input$weight,input$ds.sd,input$wi.sum)$pop
##    source("init-pop-1152033.32.txt")
#  particula$posicao<-init.pop
##    if(length(input$init.elem)>0) particula$posicao[1,]<-input$init.elem    
#}
#

if(input$use.initpop) particula$posicao<-input$initpop
else particula$posicao <- populacao$posicao

particula$velocidade <- populacao$velocidade
dump(c("particula"),paste("init-pop-",proc.time()[3],".txt",sep=""))
   iteracao <- 1                                                                                   
   iteracao2 <- 1                                                                                   
   parar <- 0
   c.update<-0
   #executa bloco enquanto criterio de parada nao for satisfeito

#   procn<-2; create.parallel.folders(procn,input)
   while(!condicao.parada(particula$melhores.aptidoes, iteracao,iteracao2)){

#DECREASING INERTIA WEIGHT
      w<<-w.max-iteracao*(w.max-w.min)/max.iteracoes

#      res.calc.aptidao <- calc.aptidao(particula,melhor.global,input,fit.calc.func) #calcula aptidao da populacao
      res.calc.aptidao <- calc.aptidao.opt(particula,melhor.global,input,fit.calc.func,part2param) #calcula aptidao da populacao
#      res.calc.aptidao <- calc.aptidao.parallel(particula,melhor.global,input,procn) #calcula aptidao da populacao

      particula<-res.calc.aptidao$particula
      melhor.global<-res.calc.aptidao$melhor.global
#browser()                                 
#      if (iteracao==1) aptidao.pop1<-mean(particula$aptidao[which(particula$aptidao!=Inf)])
      #,sd((particula$aptidao)))

      #calcula novos valores para a velocidade de cada particula
      particula$velocidade <- calc.velocidade(particula,vmax,melhor.global) 

      #calcula a nova posicao de cada particula
      particula <- calc.posicao(particula,part.max)

      cat('iteracao: ',iteracao,'aptidao: ',melhor.global$aptidao,'\n')
  		iteracao <- iteracao+1
  		iteracao2 <- iteracao2+1
#      if ((c.upd!="n")&&(iteracao2>8))
#      {
#        if(sd(particula$melhores.aptidoes[1:8])<0.001)
#        {
#          if((length(grep("m",c.upd))>0)&&(c.update<=5))
#          {                                                                              
#            iteracao2<-0
#            c.update<-c.update+1
#            C1<<-(1-1*c.update/10)*C1.orig
#            C2<<-(1-1*c.update/10)*C2.orig
#            cat('diminuiu: ',C1,'\n')
#          }
#        }
#        else
#        {
#          if((length(grep("p",c.upd))>0)&&(c.update>=(-5)))
#          {
#            iteracao2<-0
#            c.update<-c.update-1
#            C1<<-(1-1*c.update/10)*C1.orig
#            C2<<-(1-1*c.update/10)*C2.orig
#            cat('aumentou: ',C1,'\n')        
#          }
#        }
#      }
#
   }
   file.create("end.txt")
   cat('fim\n')
   return(list(param=melhor.global$posicao,aptidao=melhor.global$aptidao,melhores=particula$melhores.aptidoes))
#   , aptidao.pop1=aptidao.pop1))
}
##################################################################################################
pso.pm <- function(input,part.size,part.max,fit.calc.func,part2param){
  pop.size <- 100     # tamanho da populacao
  vmax <- c(part.max[,2]); #a velocidade e um fator de 0.6 em relacao a posicao
   particula <- list(posicao=matrix(0, pop.size, part.size), 
         velocidade=matrix(0,pop.size, part.size), 
         aptidao=vector('numeric', pop.size) + Inf,
         melhores.aptidoes=c(),
         melhor=list(posicao=matrix(0, pop.size, part.size),
         aptidao=vector('numeric', pop.size) + Inf)
   );
   melhor.global <- list(posicao=vector('numeric', part.size), aptidao=+Inf);

     populacao <- gera.pop(pop.size,part.size,part.max,vmax)
     dump(c("populacao"),paste("pop1-",proc.time()[3],".txt",sep=""))

  if(input$represent=="real"){
    init.pop<-init.pop.gen4(pop.size,input$cluster.n,input$dataset,input$weight,input$ds.sd,input$wi.sum)$pop
    particula$posicao<-init.pop
  }
  else particula$posicao <- populacao$posicao

  particula$velocidade <- populacao$velocidade

   iteracao <- 1                                                                                   
   iteracao2 <- 1                                                                                   
   parar <- 0
   c.update<-0

   while(!condicao.parada(particula$melhores.aptidoes, iteracao,iteracao2)){

     res.calc.aptidao <- calc.aptidao.opt(particula,melhor.global,input,fit.calc.func,part2param) #calcula aptidao da populacao
     particula<-res.calc.aptidao$particula
     melhor.global<-res.calc.aptidao$melhor.global
     particula$velocidade <- calc.velocidade(particula,vmax,melhor.global) 
     particula <- calc.posicao(particula,part.max)
     cat('iteracao: ',iteracao,'aptidao: ',melhor.global$aptidao,'\n')
 		iteracao <- iteracao+1
 		iteracao2 <- iteracao2+1
            
      if (iteracao2>8)
      {
        if(sd(particula$melhores.aptidoes[1:8])<0.001)
        {
          if(c.update<=5)
          {                                                                              
            iteracao2<-0
            c.update<-c.update+1
            C1<<-(1-1*c.update/10)*C1.orig
            C2<<-(1-1*c.update/10)*C2.orig
            cat('diminuiu: ',C1,'\n')
          }
        }
        else
        {
          if(c.update>=(-5))
          {
            iteracao2<-0
            c.update<-c.update-1
            C1<<-(1-1*c.update/10)*C1.orig
            C2<<-(1-1*c.update/10)*C2.orig
            cat('aumentou: ',C1,'\n')        
          }
        }
      }

   }
   return(list(param=melhor.global$posicao,aptidao=melhor.global$aptidao,melhores=particula$melhores.aptidoes))
}
##################################################################################################
pso.m <- function(input,part.size,part.max,fit.calc.func,part2param){
  pop.size <- 100     # tamanho da populacao
  vmax <- c(part.max[,2]); #a velocidade e um fator de 0.6 em relacao a posicao
   particula <- list(posicao=matrix(0, pop.size, part.size), 
         velocidade=matrix(0,pop.size, part.size), 
         aptidao=vector('numeric', pop.size) + Inf,
         melhores.aptidoes=c(),
         melhor=list(posicao=matrix(0, pop.size, part.size),
         aptidao=vector('numeric', pop.size) + Inf)
   );
   melhor.global <- list(posicao=vector('numeric', part.size), aptidao=+Inf);

     populacao <- gera.pop(pop.size,part.size,part.max,vmax)
     dump(c("populacao"),paste("pop1-",proc.time()[3],".txt",sep=""))

  if(input$represent=="real"){
    init.pop<-init.pop.gen4(pop.size,input$cluster.n,input$dataset,input$weight,input$ds.sd,input$wi.sum)$pop
    particula$posicao<-init.pop
  }
  else particula$posicao <- populacao$posicao

  particula$velocidade <- populacao$velocidade

   iteracao <- 1                                                                                   
   iteracao2 <- 1                                                                                   
   parar <- 0
   c.update<-0

   while(!condicao.parada(particula$melhores.aptidoes, iteracao,iteracao2)){

     res.calc.aptidao <- calc.aptidao.opt(particula,melhor.global,input,fit.calc.func,part2param) #calcula aptidao da populacao
     particula<-res.calc.aptidao$particula
     melhor.global<-res.calc.aptidao$melhor.global
     particula$velocidade <- calc.velocidade(particula,vmax,melhor.global) 
     particula <- calc.posicao(particula,part.max)
     cat('iteracao: ',iteracao,'aptidao: ',melhor.global$aptidao,'\n')
 		iteracao <- iteracao+1
 		iteracao2 <- iteracao2+1
            
      if (iteracao2>8)
      {
        if(sd(particula$melhores.aptidoes[1:8])<0.001)
        {
          if(c.update<=5)
          {                                                                              
            iteracao2<-0
            c.update<-c.update+1
            C1<<-(1-1*c.update/10)*C1.orig
            C2<<-(1-1*c.update/10)*C2.orig
            cat('diminuiu: ',C1,'\n')
          }
        }
      }

   }
   return(list(param=melhor.global$posicao,aptidao=melhor.global$aptidao,melhores=particula$melhores.aptidoes))
}
##################################################################################################
pso.trad <- function(input,part.size,part.max,fit.calc.func,part2param){
  pop.size <- 100     # tamanho da populacao
  vmax <- c(part.max[,2]); #a velocidade e um fator de 0.6 em relacao a posicao
   particula <- list(posicao=matrix(0, pop.size, part.size), 
         velocidade=matrix(0,pop.size, part.size), 
         aptidao=vector('numeric', pop.size) + Inf,
         melhores.aptidoes=c(),
         melhor=list(posicao=matrix(0, pop.size, part.size),
         aptidao=vector('numeric', pop.size) + Inf)
   );
   melhor.global <- list(posicao=vector('numeric', part.size), aptidao=+Inf);

     populacao <- gera.pop(pop.size,part.size,part.max,vmax)
     dump(c("populacao"),paste("pop1-",proc.time()[3],".txt",sep=""))

  if(input$represent=="real"){
    init.pop<-init.pop.gen4(pop.size,input$cluster.n,input$dataset,input$weight,input$ds.sd,input$wi.sum)$pop
    particula$posicao<-init.pop
  }
  else particula$posicao <- populacao$posicao

  particula$velocidade <- populacao$velocidade

   iteracao <- 1                                                                                   
   iteracao2 <- 1                                                                                   
   parar <- 0
   c.update<-0

   while(!condicao.parada(particula$melhores.aptidoes, iteracao,iteracao2)){

     res.calc.aptidao <- calc.aptidao.opt(particula,melhor.global,input,fit.calc.func,part2param) #calcula aptidao da populacao
     particula<-res.calc.aptidao$particula
     melhor.global<-res.calc.aptidao$melhor.global
     particula$velocidade <- calc.velocidade(particula,vmax,melhor.global) 
     particula <- calc.posicao(particula,part.max)
     cat('iteracao: ',iteracao,'aptidao: ',melhor.global$aptidao,'\n')
 		iteracao <- iteracao+1
 		iteracao2 <- iteracao2+1
            
   }
   return(list(param=melhor.global$posicao,aptidao=melhor.global$aptidao,melhores=particula$melhores.aptidoes))
}

##################################################################################################
## VERIFICA SE A CONDICAO DE PARADA FOI SATISFEITA
##################################################################################################
condicao.parada <- function(melhores, iteracao, iteracao2){
  if (iteracao==max.iteracoes) return (TRUE)
  else if (iteracao2>10){
    if(sd(melhores[1:10])<0.001){
      if ((melhores[1]-melhores[length(melhores)])!=0) return (TRUE)
      else return(FALSE)
    }
    else return(FALSE)
  }  
  else return (FALSE);
}

##################################################################################################
## GERACAO DA POPULACAO INICIAL DE FORMA ALEATORIA
##################################################################################################
gera.pop <- function(pop.size,part.size,part.max,vmax){

   pop <- list(posicao=matrix(0, pop.size, part.size), velocidade=matrix(0, pop.size, part.size))
   for (i in 1:part.size){ 
    #set.seed(i*3)
		pop$posicao[,i] <- runif(pop.size, part.max[i,1], part.max[i,2])
    #set.seed(i*6)
		pop$velocidade[,i] <- rep(0,pop.size)
#		pop$velocidade[,i] <- runif(pop.size, -vmax[i], vmax[i])
	}

   return (pop)
}

##################################################################################################
## CALCULA A VELOCIDADE DE CADA PARTICULA
##################################################################################################
calc.velocidade <- function(particula,vmax,melhor.global){
#browser()
pop.size<-nrow(particula$posicao)
part.size<-ncol(particula$posicao)
    velocidade <- matrix(0, pop.size, part.size);
    aux.melhor.global = matrix(rep(melhor.global$posicao, pop.size), pop.size, part.size, byrow=TRUE);
    #set.seed(3)
    rand1 = matrix(c(runif(pop.size*part.size, 0, 1)), pop.size, part.size);
    #set.seed(4)
    rand2 = matrix(c(runif(pop.size*part.size, 0, 1)), pop.size, part.size);
  	
    velocidade = w*particula$velocidade + 
                    rand1*C1*(particula$melhor$posicao - particula$posicao) +
                    rand2*C2*(aux.melhor.global - particula$posicao)
	#se a velicidade estiver fora do intervalo predefinido, alterar a velocidade para o valor 
	# de suas fronteiras
	#se a velicidade estiver fora do intervalo predefinido, alterar a velocidade para o valor 
	# de suas fronteiras
   for (i in 1:part.size){
      velocidade[,i] = replace(velocidade[,i], velocidade[,i] > vmax[i], vmax[i]); 
      velocidade[,i] = replace(velocidade[,i], velocidade[,i] < -vmax[i], -vmax[i]);
	  
   }
            
    return (velocidade);
}

##################################################################################################
## CALCULA A POSICAO DE CADA PARTICULA
##################################################################################################
calc.posicao <- function(particula,part.max){
	particula$posicao = particula$posicao + particula$velocidade
		
    return (particula);
}

##################################################################################################
## CALCULA A APTIDAO DE CADA PARTICULA CONFORME A FUNCAO DE APTIDAO
##################################################################################################
calc.aptidao <- function(particula,melhor.global,input,fit.calc.func){
    #para cada particula da populacao, calcula a funcao objetivo.

pop.size<-nrow(particula$posicao)

  for (i in 1:pop.size){

    posicao1 <- particula$posicao[i,]
#    aux<-eval(call(fit.calc.func,posicao1,input))
#    particula$aptidao[i] <- aux$Q.val
    particula$aptidao[i] <- fit.calc.func(posicao1,input)$Q.val
    #browser()
#cat(particula$aptidao[i],",",particula$melhor$aptidao[i],"\n")
        #se a funcao objetivo retornou um valor menor que o melhor local armazenado
        #atualmente, substituir o melhor local pelo valor atual encontrado.                   
#browser()
        if (particula$aptidao[i] < particula$melhor$aptidao[i]){
            particula$melhor$aptidao[i] <- particula$aptidao[i];
            particula$melhor$posicao[i,] <- particula$posicao[i,];
        }

    }
        #se a aptidao encontrada na particula atual e menor que a aptidao global,
        #substituir a aptidao global pela aptidao encontrada por essa particula
        if (min(particula$aptidao) < melhor.global$aptidao){
            melhor.global$aptidao <- min(particula$aptidao)
            melhor.global$posicao <- particula$posicao[which.min(particula$aptidao),]
            #melhor.svm <- resultado
        }
        particula$melhores.aptidoes<-c(melhor.global$aptidao,particula$melhores.aptidoes)
	list (particula=particula,melhor.global=melhor.global)
}
##################################################################################################
create.parallel.folders<-function(procn,input){
  dump("input","input.dat")                                                                            
#  file.names<-c("command2.dat")
  file.remove("end.txt")
  for(i in 1:procn){
  write(paste('source("run-exp.r");while(end==FALSE){while((file.exists("pop-part',i,'.txt")==FALSE)&&(file.exists("end.txt")==FALSE)) Sys.sleep(0.5);if(file.exists("end.txt")==FALSE){source("pop-part',i,'.txt");file.remove("pop-part',i,'.txt");source("input.dat");apt<-apply(pop,1,fit.calc.pso,input=input);rm(pop);dump("apt","apt-part',i,'.txt");rm(apt);}else{end<-TRUE}}',sep=""),file=paste("command-part",i,".dat",sep=""))  
  system(paste("R CMD BATCH command-part",i,".dat out-part",i,".log &",sep=""))
#  write(paste('source("run-exp.r");source("pop-part',i,'.txt");source("input.dat");apt<-matrix(0,nrow(pop));for(i in 1:nrow(pop)){apt[i]<-fit.calc(pop[i,],input)$Q.val};dump("apt","apt-part',i,'.txt");',sep=""),file=paste("command",i,".dat",sep=""))
#  write(paste('source("../run-exp.r");source("../pop-part',i,'.txt");source("../input.dat");for(i in 1:',nrow(pop),'){apt<-fit.calc(pop[i,],input)$Q.val};dump("apt","../apt-part',i,'.txt");',sep=""),file=paste("command",i,".dat",sep=""))
#    dir.create(paste(i))
#    file.names<-c(paste("command",i,".dat",sep=""))
#    file.copy(file.names,dir.name,overwrite=TRUE)
  }
}

##################################################################################################
calc.aptidao.parallel <- function(particula,melhor.global,input,procn){
    #para cada particula da populacao, calcula a funcao objetivo.
#procn<-10
pop.size<-nrow(particula$posicao)
pop.part.size<-pop.size/procn
pop.all<-particula$posicao

#file.remove(list.files()[grep("pop-part",list.files())])
#file.remove(list.files()[grep("apt-part",list.files())])
for (i in 1:(procn-1)){
  pop<-pop.all[1:pop.part.size,]
  pop.all<-pop.all[-(1:pop.part.size),]
  dump("pop",paste("pop-part",i,".txt",sep=""))
##  system(paste("cd ",i,sep=""))
#  system(paste("R CMD BATCH command",i,".dat out",i,".log &",sep=""))
##  system("cd ..")
##  system(paste(".",i,"R CMD BATCH command2.dat out.log &",sep="/"))
}
pop<-pop.all
apt<-apply(pop,1,fit.calc.pso,input=input)
#dump("apt","apt-part',i,'.txt")

while(length(grep("apt-part", list.files()))<(procn-1)) Sys.sleep(1)
apt.all<-apt
#apt.all<-c()
for (i in (procn-1):1){
  rm(apt);source(paste("apt-part",i,".txt",sep=""))
  file.remove(paste("apt-part",i,".txt",sep=""))
  apt.all<-c(apt,apt.all)
}
  particula$aptidao<-apt.all  

  aptidao.e.melhor<-cbind(particula$aptidao,particula$melhor$aptidao)
  wch.min<-apply(aptidao.e.melhor,1,which.min)
  particula$melhor$aptidao<-aptidao.e.melhor[cbind(1:pop.size,wch.min)]
  particula$melhor$posicao[which(wch.min==1),]<-particula$posicao[which(wch.min==1),]
  particula$melhor$posicao[which(wch.min==2),]<-particula$melhor$posicao[which(wch.min==2),]
  
        #se a aptidao encontrada na particula atual e menor que a aptidao global,
        #substituir a aptidao global pela aptidao encontrada por essa particula
        if (min(particula$aptidao) < melhor.global$aptidao){
            melhor.global$aptidao <- min(particula$aptidao)
            melhor.global$posicao <- particula$posicao[which.min(particula$aptidao),]
            #melhor.svm <- resultado
        }
        particula$melhores.aptidoes<-c(melhor.global$aptidao,particula$melhores.aptidoes)
	list (particula=particula,melhor.global=melhor.global)
}
##################################################################################################
calc.aptidao.opt <- function(particula,melhor.global,input,fit.calc.func,part2param){
    #para cada particula da populacao, calcula a funcao objetivo.

pop.size<-nrow(particula$posicao)
#browser()
particula$aptidao<-apply(particula$posicao,1,get(fit.calc.func),input=input,part2param=part2param)
#browser()
  aptidao.e.melhor<-cbind(particula$aptidao,particula$melhor$aptidao)

  wch.min<-apply(aptidao.e.melhor,1,which.min)
  particula$melhor$aptidao<-aptidao.e.melhor[cbind(1:pop.size,wch.min)]
  particula$melhor$posicao[which(wch.min==1),]<-particula$posicao[which(wch.min==1),]
  particula$melhor$posicao[which(wch.min==2),]<-particula$melhor$posicao[which(wch.min==2),]

        #se a aptidao encontrada na particula atual e menor que a aptidao global,
        #substituir a aptidao global pela aptidao encontrada por essa particula
        if (min(particula$aptidao) < melhor.global$aptidao){
            melhor.global$aptidao <- min(particula$aptidao)
            melhor.global$posicao <- particula$posicao[which.min(particula$aptidao),]
            #melhor.svm <- resultado
        }
        particula$melhores.aptidoes<-c(melhor.global$aptidao,particula$melhores.aptidoes)
	list (particula=particula,melhor.global=melhor.global)
}
##################################################################################################

calc.aptidao <- function(particula,melhor.global,input,fit.calc.func){
    #para cada particula da populacao, calcula a funcao objetivo.

pop.size<-nrow(particula$posicao)                                                                           

  for (i in 1:pop.size){

    posicao1 <- particula$posicao[i,]
    aux<-eval(call(fit.calc.func,posicao1,input))
    particula$aptidao[i] <- aux$Q.val
#    particula$aptidao[i] <- fit.calc(posicao1,input)$Q.val
    #browser()
#cat(particula$aptidao[i],",",particula$melhor$aptidao[i],"\n")
        #se a funcao objetivo retornou um valor menor que o melhor local armazenado
        #atualmente, substituir o melhor local pelo valor atual encontrado.                   
#browser()
        if (particula$aptidao[i] < particula$melhor$aptidao[i]){
            particula$melhor$aptidao[i] <- particula$aptidao[i];
            particula$melhor$posicao[i,] <- particula$posicao[i,];
        }

    }
        #se a aptidao encontrada na particula atual e menor que a aptidao global,
        #substituir a aptidao global pela aptidao encontrada por essa particula
        if (min(particula$aptidao) < melhor.global$aptidao){
            melhor.global$aptidao <- min(particula$aptidao)
            melhor.global$posicao <- particula$posicao[which.min(particula$aptidao),]
            #melhor.svm <- resultado
        }
        particula$melhores.aptidoes<-c(melhor.global$aptidao,particula$melhores.aptidoes)
	list (particula=particula,melhor.global=melhor.global)
}
##################################################################################################
calc.aptidao.pareto <- function(particula,data,hints,prev.vals,tgt,Ch,melhor.global){
    #para cada particula da populacao, calcula a funcao objetivo.
#browser()
pop.size<-nrow(particula$posicao)
dim.n<-ncol(data)
cluster.n<-sum(Ch)
obj.n<-3                          
v.len<-dim.n*cluster.n
A.len<-(dim.n^2)*cluster.n
A2.len<-((dim.n+1)*dim.n/2)*cluster.n
ab.len<-2

for (i in 1:pop.size){
  posicao1 <- particula$posicao[i,]
  if (tgt$v==1) {v.val <- t(matrix(posicao1[1:v.len],dim.n,cluster.n))}
  else v.val<-t(matrix(prev.vals$v,dim.n,cluster.n))
  if (tgt$A==1) {A.val<- t(matrix(new.pop.calc(matrix(posicao1[(tgt$v*v.len + 1):(tgt$v*v.len + A.len)],1,A.len),dim.n,cluster.n),dim.n^2,cluster.n))}
  else if (tgt$A2==1) {A.val<-t(matrix(feasible.projection(posicao1[(tgt$v*v.len + 1):(tgt$v*v.len + A2.len)],dim.n,cluster.n)$parametros,dim.n^2,cluster.n))}
  else A.val<-t(matrix(prev.vals$A,dim.n^2,cluster.n))
  U<-U.calc(data,v.val,A.val)
  resultados<-performance.index.hxc.pareto(data,U,v.val,A.val,hints,Ch)
  #      particula$aptidao[i] <- resultados$Q.val
#browser()
  particula$aptidao3[i,]<-resultados
  
  #se a funcao objetivo retornou um valor menor que o melhor local armazenado
  #atualmente, substituir o melhor local pelo valor atual encontrado.
  #        if (particula$aptidao[i] < particula$melhor$aptidao[i]){
  #            particula$melhor$aptidao[i] <- particula$aptidao[i];
  #            particula$melhor$posicao[i,] <- particula$posicao[i,];
  #        }
}

objs<-rbind(melhor.global$aptidao3,particula$melhor$aptidao3,particula$aptidao3)
ind.n<-nrow(objs)
cdp<-matrix(0,ind.n)
Sp<-matrix(0,ind.n,ind.n)
for(i in 1:ind.n){
le.eq<-matrix(0,ind.n,obj.n)
le<-matrix(0,ind.n,obj.n)
gr.eq<-matrix(0,ind.n,obj.n)
gr<-matrix(0,ind.n,obj.n)
  for(j in 1:obj.n){
    le.eq[which(objs[,j]<=objs[i,j],arr.ind=TRUE),j]<-1
    le[which(objs[,j]<objs[i,j],arr.ind=TRUE),j]<-1
    gr.eq[which(objs[,j]>=objs[i,j],arr.ind=TRUE),j]<-1
    gr[which(objs[,j]>objs[i,j],arr.ind=TRUE),j]<-1
  }
leql.sum<-cbind(apply(le.eq,1,sum),apply(le,1,sum))  
geqg.sum<-cbind(apply(gr.eq,1,sum),apply(gr,1,sum))  
leql<-matrix(0,ind.n,2)
geqg<-matrix(0,ind.n,2)
leql[which(leql.sum[,1]==obj.n,arr.ind=TRUE),1]<-1
leql[which(leql.sum[,2]>0,arr.ind=TRUE),2]<-1
geqg[which(geqg.sum[,1]==obj.n,arr.ind=TRUE),1]<-1
geqg[which(geqg.sum[,2]>0,arr.ind=TRUE),2]<-1
leql<-apply(leql,1,sum)
geqg<-apply(geqg,1,sum)
bdom<-matrix(0,ind.n)
bdom[which(leql==2,arr.ind=TRUE)]<-1
cdp[i]<-sum(bdom)
Sp[i,which(geqg==2,arr.ind=TRUE)]<-1
}

posicoes<-rbind(melhor.global$posicao,particula$melhor$posicao,particula$posicao)
ranking<-order(cdp)
#ranks<-apply(objs,2,rank)
#rank.all<-apply(ranks,1,sum)
particula.melhor<-ranking[sample(1:3,2)]
particula.melhor<-particula.melhor[which(particula.melhor!=1)][1]
global.melhor<-ranking[sample(1:3,1)]

particula$melhor$posicao<-posicoes[particula.melhor,]        
particula$melhor$aptidao3<-objs[particula.melhor,]
melhor.global$posicao<-posicoes[global.melhor,]        
melhor.global$aptidao3<-objs[global.melhor,]        
           #se a aptidao encontrada na particula atual e menor que a aptidao global,
        #substituir a aptidao global pela aptidao encontrada por essa particula
#        if (min(particula$aptidao) < melhor.global$aptidao){
#            melhor.global$aptidao <- min(particula$aptidao)
#            melhor.global$aptidao2 <- particula$aptidao2[which.min(particula$aptidao)]
#            melhor.global$posicao <- particula$posicao[which.min(particula$aptidao),]
#            #melhor.svm <- resultado
#        }
        particula$melhores.aptidoes<-rbind(melhor.global$aptidao3,particula$melhores.aptidoes)
	list (particula=particula,prev.vals=prev.vals,melhor.global=melhor.global)
}

