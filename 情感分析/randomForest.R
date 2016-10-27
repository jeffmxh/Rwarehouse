library(jiebaR)
library(parallel)
library(tm)
library(randomForest)
library(dplyr)

make_dtm <- function(word_corpus, 
                     min_word_len = 2, min_repeat = 1, 
                     rmNum = TRUE, rmPun = FALSE, stopwords_list = stopwords.list,
                     wt = weightTf, encod = "UTF-8"){
  return_dtm <- DocumentTermMatrix(word_corpus,
                                   control = list(
                                     wordLengths=c(min_word_len, Inf), # to allow long words
                                     bounds = list(global = c(min_repeat, Inf)), 
                                     # each term appears in at least 2 docs
                                     removeNumbers = rmNum, 
                                     removePunctuation = rmPun,
                                     #removePunctuation  = list(preserve_intra_word_dashes = FALSE),
                                     stopwords = stopwords_list,
                                     weighting = wt, 
                                     encoding = encod))
  return(return_dtm)
}

data_pos <- readLines("/home/jeffmxh/pos.txt")
data_neg <- readLines("/home/jeffmxh/neg.txt")
data_pos <- data.frame("content" = data_pos, "label" = rep("positive", length(data_pos)), stringsAsFactors = FALSE)
data_neg <- data.frame("content" = data_neg, "label" = rep("negative", length(data_neg)), stringsAsFactors = FALSE)
data_all <- rbind(data_pos, data_neg)
data_all <- data.frame("index" = c(1:nrow(data_all)), data_all)
seg_list <- emotion_text_segmenter(data_all, "content")
seg_list <- mclapply(1:length(seg_list), function(i){seg_list[[i]][seg_list[[i]] %in% emotion_dict$word]}, mc.cores = 16)
corpus <- Corpus(VectorSource(seg_list))
stop_words <- readLines("/home/jeffmxh/stopwords_utf8.txt")
stop_words <- stop_words[!stop_words %in% emotion_dict$word]
dtm_obj <- make_dtm(corpus, stopwords.list = stop_words)
dtm_matrix <- as.matrix(dtm_obj) %>% as.data.frame()
dtm_matrix <- cbind(dtm_matrix, factor(data_all$label))
colnames(dtm_matrix)[4333] <- "label"
dtm_matrix_small = sample_n(dtm_matrix, 3000)
time_temp <- Sys.time()
model_1_5000 <- randomForest(label~., data = dtm_matrix_small, importance = TRUE, proximity = FALSE, ntree = 150)
# model_1_5000 <- randomForest(x = subset(dtm_matrix, select = -label), y = dtm_matrix[, "label"], importance = TRUE, proximity = FALSE, ntree = 100)
cat("用时:", Sys.time()-time_temp, "\n", sep = "")
