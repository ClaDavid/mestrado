library("stringr")
library("NLP")
library("openNLP")
library("kernlab")
library("reshape2")
library("quanteda")

#significados:  
#https://www.sketchengine.eu/english-treetagger-pipeline-2/
#https://www.sketchengine.eu/modified-penn-treebank-tagset/

posText<- "I gave him my heart, and he took and pinched it to death; and flung it back to me.
           People feel with their hearts, Ellen, and since he has destroyed mine, I have not power to feel for him."

tagPOS <-  function(x, ...) {
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

tokensTag <- lapply(reuters, tagPOS)
tokensTag

tokens <- c()
for(i in 1:length(reuters)){
   tokens <- rbind(tokens, lapply(lapply(tokensTag[[i]]$POStagged, unlist), paste, collapse = ''))
}

tokens <- apply(tokens, 1, as.character)

#tokens <- paste( unlist(tokensTag[1]), collapse='')
words <- strsplit(tokens, " ", fixed = T)
#words <- unlist(words)
words <- lapply(words, unlist)
#counts <- table(words)
counts <- lapply(words, table)

listaToMatrix <- melt(counts)

tav <- matrix(0, nrow = length(words), ncol = length(unique(listaToMatrix$Var1)))
colnames(tav) <- unique(listaToMatrix$Var1)

for(i in 1:nrow(tav)){
  for(j in 1:ncol(tav)){
    if(listaToMatrix$L1[i] == i && listaToMatrix$Var1[j] == j){
      tav[i, j] = listaToMatrix$value[j]
    }
  }
}


matrix.please<-function(x) {
  m<-as.matrix(x[,-1])
  rownames(m)<-x[,1]
  m
}

dfToMatrix <- matrix(as.numeric(unlist(listaToMatrix)),nrow=nrow(listaToMatrix))
dfToMatrix <- matrix.please(dfToMatrix)

for(i in 1:nrow(tav)){
  for(j in 1:ncol(dfToMatrix)){
    if(dfToMatrix[i] == i){
      tav[i, tav[colnames(tav) %in% listaToMatrix$Var1[i]]] = listaToMatrix$value[j]
    }
  }
}


for(i in 1:nrow(tav)){
  subsetDf <- subset(listaToMatrix, listaToMatrix$L1 == i)
  tav[i, which(colnames(tav) == subsetDf$Var1)] <- subsetDf$value
  
}


colnames(tav) <- unique(listaToMatrix$Var1)





#listaToMatrix$Var1[1] %in% colnames(tav)

#&& which(colnames(tav)[j] == listaToMatrix$Var1[j], arr.ind = T )){
#which(listaToMatrix$Var1[j] == "colnames(tav)")

for(i in 1:nrow(tav)){
  if(listaToMatrix$L1[i] == i ){
tav[outer(colnames(tav), listaToMatrix$Var1, "==")] <- listaToMatrix$value
  }
}





tav[outer(colnames(tav), rownames(), "==")]

part1 <- matrix(as.numeric(unlist(listaToMatrix)),nrow=nrow(listaToMatrix))

firstTav

for(i in 1:nrow(tav)){
  for(j in 1:ncol(tav)){
    if(listaToMatrix$L1[i] == i){
      tav[i, tav[colnames(tav) %in% listaToMatrix$Var1[i]]] = listaToMatrix$value[j]
    }
  }
}

for(i in 1:nrow(tav)){
  for(j in 1:ncol(tav)){
  tav[which(listaToMatrix$L1 == i), ] = listaToMatrix$value[j]

    }
}


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