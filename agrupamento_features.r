library(e1071)
library(data.table)
library(zoo)
library(ppclust)
library(fclust)


tav <- fread(file = "reuters_pos_tag_final.csv", header = TRUE, check.names = FALSE)

#######################
# clustering features #
#######################

#####fukuyama.sugeno

start.time <- Sys.time()


for( index in 1:5  )
{
  d = NULL
  
  
  for( i in 2:floor( sqrt( nrow( t(tav) ) ) ) )
  {
    best_start <- .Machine$integer.max
    for( index_best_start in 1:100 )
    {
      fuzzyFeature <- cmeans(t(tav), i)
      if( best_start > fuzzyFeature$withinerror )
      {
        best_start <- fuzzyFeature$withinerror
        fuzzyFeatureBest <- fuzzyFeature
      }
      
    }
    
    
    result_index <- fclustIndex(fuzzyFeatureBest, t(tav), index = "fukuyama.sugeno")
    d = rbind(d, data.frame(i, result_index))
  }
  
  write.csv(d, file = paste(paste('/home/semantix/Documentos/mestrado/mestrado/fukuyama/fukuyama_sugeno_teste', index, sep ="_" ), 'csv',sep="."), row.names = FALSE)
  png(filename = paste(paste("/home/semantix/Documentos/mestrado/mestrado/fukuyama/fukuyama_sugeno_teste", index, sep = "_"), 'png', sep = "." ) )
  plot(d$i, d$result_index, type = "o")
  dev.off()

}
end.time <- Sys.time()
time.taken <- end.time - start.time
#####xie beni
for( index in 1:10  )
{
  d = NULL
  
  for( i in 2:floor( sqrt( nrow( t(tav) ) ) ) )
  {
    fuzzyFeature <- cmeans(t(tav), i)
    result_index <- fclustIndex(fuzzyFeature, t(tav), index = "xie.beni")
    d = rbind(d, data.frame(i, result_index))
  }
  
  write.csv(d, file = paste(paste('git//mestrado//xb//xb_teste', index, sep ="_" ), 'csv',sep="."), row.names = FALSE)
  png(filename = paste(paste("git//mestrado//xb//xb_teste", index, sep = "_"), 'png', sep = "." ) )
  plot(d$i, d$result_index, type = "o")
  dev.off()
  
}

#####joelho
for(index in 1:10){
knee = c()

for( i in 2:floor( sqrt( nrow( t(tav) ) ) ) )
{
  fuzzyFeature <- cmeans(t(tav), i)
  knee <- c(knee, fuzzyFeature$withinerror)
  
}
delta_one_element = diff(knee)
soma_deltas = rollapply(delta_one_element, 2, sum)
cluster_utilizado = which.min(delta_one_element) + 2
cluster_knee = c(2:floor( sqrt( nrow( t(tav) ) ) ))
png(filename = paste(paste("/home/semantix/Documentos/mestrado/mestrado/joelho/joelho_teste", index, sep = "_"), 'png', sep = "." ) )
plot(cluster_knee, knee, type = "o", xlab="cluster",ylab="erro")
dev.off()
print(cluster_utilizado)
}

#####silhueta

start.time <- Sys.time()

d = NULL
fuzzyFeature = list()
for( i in 2:floor( sqrt( nrow( t(tav) ) ) ) )
{
  fuzzyClustering <- cmeans(t(tav), i)
  fuzzyFeature[[i]] <- fuzzyClustering$membership
}

for( i in 2:floor( sqrt( nrow( t(tav) ) ) ) )
{
  result_index <- SIL.F(t(tav), fuzzyFeature[[i]], alpha = 1)
  d = rbind(d, data.frame(i, result_index))
}


write.csv(d, file = '/home/semantix/Documentos/mestrado/mestrado/silhueta/silhueta_teste.csv', row.names = FALSE)
png(filename = "/home/semantix/Documentos/mestrado/mestrado/silhueta/silhueta_teste.png" )
plot(d$i, d$result_index, type = "o")
dev.off()

end.time <- Sys.time()

time.taken <- end.time - start.time


# the ideal number of cluster is 5

fuzzyFeature <- cmeans(t(tav), 5)

###################
# clustering docs #
###################

