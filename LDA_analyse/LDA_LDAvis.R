library(readxl)
library(jiebaR)
library(parallel)
library(LDAvis)
library(lda)

# load data---------------------------------------------

instance_noodles <- read_excel("/home/jeffmxh/康师傅私房牛肉_康师傅私房牛肉用户微博转发_retweeted_content_output_content_rela.xlsx", sheet = 4)
cat("Data loaded!\n")

# define data to deal-----------------------------------

target_data <- instance_noodles[,-1]
target_column <- "content"

# load the stop_words list------------------------------

stop_words <- readLines("/home/jeffmxh/stopwords_utf8.txt", encoding = "UTF-8")
cat("Stop words list loaded!\n")

# 对数据框特定列分词，返回list--------------------------

emotion_text_segmenter <- function(data_emotion, column_deal, stop_word_path = "/home/jeffmxh/stopwords_utf8.txt"){
  if(.Platform$OS.type=="windows"){
    n_cores = 1
  }else{
    n_cores = floor(detectCores(logical = TRUE) * 0.75)
  }
  data_emotion <- as.data.frame(data_emotion)
  target_text <- data_emotion[, column_deal]
  cc <- worker(stop_word = stop_word_path)
  segment_list <- mclapply(1:length(target_text), function(i){
    seg_list <- tryCatch(
      {
        cc[target_text[i]]
      },
      error = function(e){seg_list <- c()},
      warning = function(w){seg_list <- c()}
    )
    return(seg_list)
  }, mc.cores = n_cores
  )
  return(segment_list)
}

# Remove useless pharases---------------------------

text_vec <- target_data[,target_column]
text_vec <- gsub(" ", ",", text_vec)
text_vec <- gsub("\\[.+?\\]", "", text_vec)
text_vec <- gsub("\\#.+?\\#", "", text_vec)
text_vec <- gsub("【.+?】", "", text_vec)
text_vec <- gsub("[a-zA-Z0-9]", "", text_vec)
text_vec <- gsub(" ", "", text_vec)
target_data[, target_column] <- text_vec
cat("Useless pharases removed!\n")

# Generate documents for lda model---------------------

doc.list <- emotion_text_segmenter(target_data, target_column)
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)
del <- names(term.table) %in% stop_words | term.table < 5
term.table <- term.table[!del]
vocab <- names(term.table)
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list, get.terms)
cat("Documents generated!\n")

# Compute some statistics-----------------------------------

D <- length(documents)  # number of documents (2,000)
W <- length(vocab)  # number of terms in the vocab (14,568)
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document [312, 288, 170, 436, 291, ...]
N <- sum(doc.length)  # total number of tokens in the data (546,827)
term.frequency <- as.integer(term.table)  # frequencies of terms in the corpus [8939, 5544, 2411, 2410, 2143,
cat("Statistics computed!\n")

# MCMC and model tuning parameters--------------------------

K <- 20
G <- 5000
alpha <- 0.02
eta <- 0.02

# Fit the model---------------------------------------

cat("Start fitting model!\n")
set.seed(357)
system.time({
  fit <- lda.collapsed.gibbs.sampler(documents = documents, 
                                     K = K, 
                                     vocab = vocab, 
                                     num.iterations = G, 
                                     alpha = alpha, 
                                     eta = eta, 
                                     initial = NULL, 
                                     burnin = 0,
                                     compute.log.likelihood = TRUE)
})
cat("Model fitted!\n")

theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

# Generate json file-------------------------------------------

cl <- makeCluster(16)

json <- createJSON(phi = phi, 
                   theta = theta, 
                   doc.length = doc.length, 
                   vocab = vocab, 
                   term.frequency = term.frequency,
                   cluster = cl)
rm(cl)
cat("Json file generated!\n")

# View result--------------------------------------------------

serVis(json)











