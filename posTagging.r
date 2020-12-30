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
library(data.table)
##
library(devtools)
source_gist("https://gist.github.com/mrdwab/6424112")

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

df = read.table("<dataset>.csv", header = T, sep = "|")

df$col = tolower(df$col)
tokensTag <- lapply( df$col, tagPOS )
tokensTag

tokens <- c()

for( i in 1:length(df$col) )
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

listToMatrix <- melt( counts )
write.csv(listToMatrix, file = "listToMatrix<dataset>.csv", row.names = FALSE, col.names = FALSE)