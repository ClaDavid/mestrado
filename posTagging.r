library("stringr")
library("NLP")
library("openNLP")
library("kernlab")
library("reshape2")
library("tidyr")
library("quanteda")
library("e1071")

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

tav <- spread(listaToMatrix, Var1, value, fill = 0)
tav$L1 <- NULL


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