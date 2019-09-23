library(e1071)
library(data.table)
library(zoo)
library(ppclust)
library(fclust)

set.seed(42)

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
    
    write.csv(d, file = paste(paste('fukuyama/fukuyama_sugeno_teste', index, sep ="_" ), 'csv',sep="."), row.names = FALSE)
    png(filename = paste(paste("fukuyama/fukuyama_sugeno_teste", index, sep = "_"), 'png', sep = "." ) )
    plot(d$i, d$result_index, type = "o")
    dev.off()
  }
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time

#####xie beni
for( index in 1:5  )
{
  d = NULL
  
  for( i in 2:floor( sqrt( nrow( t(tav) ) ) ) )
  {
    fuzzyFeature <- cmeans(t(tav), i)
    result_index <- fclustIndex(fuzzyFeature, t(tav), index = "xie.beni")
    d = rbind(d, data.frame(i, result_index))
  }
  
  write.csv(d, file = paste(paste('xb/xb_teste', index, sep ="_" ), 'csv',sep="."), row.names = FALSE)
  png(filename = paste(paste("xb/xb_teste", index, sep = "_"), 'png', sep = "." ) )
  plot(d$i, d$result_index, type = "o")
  dev.off()
  
}

#####joelho
for(index in 1:5){
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
png(filename = paste(paste("joelho/joelho_teste", index, sep = "_"), 'png', sep = "." ) )
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


write.csv(d, file = 'silhueta/silhueta_teste_10.csv', row.names = FALSE)
png(filename = "silhueta/silhueta_teste_10.png" )
plot(d$i, d$result_index, type = "o")
dev.off()

end.time <- Sys.time()

time.taken <- end.time - start.time


# the ideal number of cluster is 7 - reuters

fuzzyFeature <- cmeans(t(tav), 7)
hard_cluster_features = as.data.frame(fuzzyFeature[["cluster"]])

write.csv(hard_cluster_features, file = "hard_cluster_features.csv", row.names = T, col.names = FALSE)
###################
# clustering docs #
###################

