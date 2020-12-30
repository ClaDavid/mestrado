library(data.table)


tav <- read.csv(file = "Reuters_Pos_Tag.csv", header = TRUE, check.names = FALSE)

pos_tagging_clean <- function(tav_dataframe)
{
  tav_dataframe = tav_dataframe[, -grep("/CD$", colnames(tav_dataframe))]
  tav_dataframe = tav_dataframe[, -grep("/,$", colnames(tav_dataframe))]
  tav_dataframe = tav_dataframe[, -grep("/.$", colnames(tav_dataframe))]
  # ps as vezes dava 0, talvez seja pq nao acha, tratar isso depois
  #tav_dataframe = tav_dataframe[, -grep("(.*)?>(.*)?/NNP$", colnames(tav_dataframe))]
  #tav_dataframe = tav_dataframe[, -grep("(.*)?<(.*)?/NNP$", colnames(tav_dataframe))]
  tav_dataframe = tav_dataframe[, -grep("'/POS$", colnames(tav_dataframe))]
  tav_dataframe = tav_dataframe[, -grep("(.*)?/\'\'", colnames(tav_dataframe))]
  tav_dataframe = tav_dataframe[, -grep("(.*)?/\`\`", colnames(tav_dataframe))]
  tav_dataframe = tav_dataframe[, -grep("\\(/", colnames(tav_dataframe))]
  tav_dataframe = tav_dataframe[, -grep("\\)/", colnames(tav_dataframe))]
  return(tav_dataframe)
}

pos_tagging_col_names <- function(tav_dataframe)
{
  colnames(tav_dataframe) <- gsub("^\"", "", colnames(tav_dataframe))
  colnames(tav_dataframe) <- gsub("^\\W*|\\.(\\w)*?>|>", "", colnames(tav_dataframe))
  return(tav_dataframe)
}

pos_tagging_col_duplicate <- function(tav_dataframe)
{
  tav_dataframe = as.data.table(tav_dataframe)
  tav_dataframe = setnames(tav_dataframe, tolower(names(tav_dataframe[1:length(tav_dataframe)])))
  #tav_dataframe = t(rowsum(t(tav_dataframe), group = colnames(tav_dataframe), na.rm = T))
  #tav_transposta <- as.data.frame(t(tav_dataframe))
  aggr <- as.data.frame(do.call(cbind, by(t(tav_dataframe),INDICES=colnames(tav_dataframe),FUN=colSums)))
  return(aggr)
}

tav_clean = pos_tagging_clean(tav)
tav_clean_part_2 = pos_tagging_col_names(tav_clean)
tav_final = pos_tagging_col_duplicate(tav_clean_part_2)

write.csv(tav_final, file = "<dataset_pos_tag>.csv", row.names = FALSE, col.names = FALSE)
