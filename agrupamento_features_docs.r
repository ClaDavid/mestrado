library(e1071)


tav <- read.csv(file = "Reuters_Pos_Tag.csv", header = TRUE, check.names = FALSE)

#######################
# clustering features #
#######################

d = NULL
start.time <- Sys.time()
for( i in 2:floor( sqrt( nrow( t(tav) ) ) ) )
{
  fuzzyFeature <- cmeans(t(tav), i)
  result_index_xie_beni <- fclustIndex(fuzzyFeature, t(tav), index = "proportion.exponent")
  d = rbind(d, data.frame(i, result_index_xie_beni))
}
end.time <- Sys.time()
time.taken <- end.time - start.time



write.csv(d, file = "Silhueta_Features_Teste_4.csv", row.names = FALSE)
plot(d$i, d$result_index_xie_beni, type = "o")
axis(side=1, at=c(2:(length(d$i) + 1)))

# the ideal number of cluster is 5

fuzzyFeature <- cmeans(t(tav), 5)

###################
# clustering docs #
###################

