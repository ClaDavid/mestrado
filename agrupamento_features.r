library(e1071)
library(data.table)


tav <- fread(file = "reuters_pos_tag_final.csv", header = TRUE, check.names = FALSE)

#######################
# clustering features #
#######################

#####fukuyama.sugeno


for( index in 1:10  )
{
  d = NULL
  
  for( i in 2:floor( sqrt( nrow( t(tav) ) ) ) )
  {
    fuzzyFeature <- cmeans(t(tav), i)
    result_index <- fclustIndex(fuzzyFeature, t(tav), index = "fukuyama.sugeno")
    d = rbind(d, data.frame(i, result_index))
  }
  
  write.csv(d, file = paste(paste('git//mestrado//fukuyama//fukuyama_sugeno_teste', index, sep ="_" ), 'csv',sep="."), row.names = FALSE)
  png(filename = paste(paste("git//mestrado//fukuyama//fukuyama_sugeno_teste", index, sep = "_"), 'png', sep = "." ) )
  plot(d$i, d$result_index, type = "o")
  dev.off()

}
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
  knee = c()
  for( i in 2:floor( sqrt( nrow( t(tav) ) ) ) )
  {
    fuzzyFeature <- cmeans(t(tav), i)
    knee <- c(knee, fuzzyFeature$withinerror)
  }
cluster_knee = c(2:floor( sqrt( nrow( t(tav) ) ) ))
png(filename = "git//mestrado//joelho//joelho_teste.png")
plot(cluster_knee, knee, type = "o", xlab="erro",ylab="cluster")
dev.off()

# the ideal number of cluster is 5

fuzzyFeature <- cmeans(t(tav), 5)

###################
# clustering docs #
###################

