Args <- commandArgs()
options(warn = -1)
require(readxl, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(jiebaR, quietly = TRUE)
require(parallel, quietly = TRUE)
require(LDAvis, quietly = TRUE)
require(lda, quietly = TRUE)
require(RMySQL, quietly = TRUE)

# load data---------------------------------------------
file_index <- as.numeric(Args[6])
file_handle <- list.files("/home/jeffmxh/data_temp")
file_name <- file_handle[file_index]

file_path <- paste0("/home/jeffmxh/data_temp/", file_name)
target_data <- read_excel(file_path)
file_name <- gsub("\\.xlsx", "", file_name)

cat("---------------------------", file_name, "---------------------------\n")
start_time <- Sys.time()
print(start_time)

# if(grepl(file_name, pattern = "转发")){
#   target_data <- target_data %>%
#     mutate(bind_content = paste(content, retweeted_content, sep = ","))
#   target_column <- "bind_content"
# }else{
#   target_column <- "content"
# }
target_column <- "content"
cat("Data loaded!\n")
K <- floor(sqrt(nrow(target_data))/3)
# K <- 30

# load the stop_words list------------------------------

source("/home/jeffmxh/r projects/txt_excel_io.R")
stop_words <- readLines("/home/jeffmxh/stopwords_utf8.txt", encoding = "UTF-8")
target_data <- as.data.frame(target_data)
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
  cc <- worker(stop_word = stop_word_path, user = "/home/jeffmxh/user_dict.txt", user_weight = "max")
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

nchar_bool <- function(str){
  return(nchar(str)>1)
}

assign_topic <- function(num_vec){
  if(length(num_vec)==0){
    return("None")
  }else{
    num_vec <- as.numeric(num_vec)
    topic_index <- names(sort(table(num_vec), decreasing = TRUE)[1])
    topic_index <- as.character(as.numeric(topic_index)+1)
    return(topic_index)
  }
}

# tSNE方法进行聚类的函数----------------------------------

jstSNE <- function(phi, maxIter = 1000, preEpoch = 100){
  jensenShannon <- function(x,y){
    m <- 0.5 * (x + y)
    0.5 * sum(x * log(x/m)) + 0.5 * sum(y * log(y/m))
  }
  dist.mat <- proxy::dist(x = phi, method = jensenShannon)
  pca.fit <- tsne::tsne(dist.mat, initial_dims = dim(phi)[2], max_iter = maxIter, epoch = preEpoch)
  return(data.frame(x = pca.fit[, 1], y = pca.fit[, 2]))
}

js_tSNE <- function(phi){jstSNE(phi=phi, maxIter = 1000, preEpoch = 1001)}

# Remove useless pharases---------------------------

text_vec <- target_data[,target_column]

text_vec <- gsub("\\[.+?\\]", "", text_vec)
text_vec <- gsub("\\#.+?\\#", "", text_vec)
text_vec <- gsub("【.+?】", "", text_vec)
text_vec <- gsub('https?:[a-zA-Z\\/\\.0-9_]+','',text_vec)
text_vec <- gsub("[A-Za-z0-9]", "", text_vec)
text_vec <- gsub('@.+?[,，：:\ )]|@.+?$','',text_vec)
text_vec <- gsub('我在(\\w){0,2}[:：](\\w*)','',text_vec)

target_data[, target_column] <- text_vec
cat("Useless pharases removed!\n")

# Generate documents for lda model---------------------

doc.list <- emotion_text_segmenter(target_data, target_column)
doc.list <- lapply(doc.list, function(vec){vec[nchar_bool(vec)]})
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)
del <- term.table < 10
term.table <- term.table[!del]
vocab <- names(term.table)
get.terms <- function(x){
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list, get.terms)
cat("Documents generated!\n")

# Compute some statistics-----------------------------------

doc.length <- sapply(documents, function(x) sum(x[2, ])) 
term.frequency <- as.integer(term.table) 
cat("Statistics computed!\n")

# MCMC and model tuning parameters--------------------------

alpha <- 0.02
eta <- 0.02

# Fit the model---------------------------------------

cat("Start fitting model!\n")
set.seed(2016)
fit <- lda.collapsed.gibbs.sampler(documents = documents, 
                                   K = K, 
                                   vocab = vocab, 
                                   num.iterations = 1000, 
                                   alpha = alpha, 
                                   eta = eta, 
                                   initial = NULL, 
                                   burnin = 0,
                                   compute.log.likelihood = TRUE)
cat("Model fitted!\n")
topic_vec <- sapply(fit$assignments, assign_topic)
target_data$assign_title <- topic_vec

theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

# Generate json file-------------------------------------------

cl <- makeCluster(16)
json_tSNE <- createJSON(phi = phi, 
                        theta = theta, 
                        doc.length = doc.length, 
                        vocab = vocab, 
                        term.frequency = term.frequency,
                        mds.method = js_tSNE,
                        cluster = cl)

cat("Json file generated!\n")

stopCluster(cl)

# View result--------------------------------------------------

json_path_tSNE <- paste0("/home/jeffmxh/LDA_json_data/R_", file_name, ".json")
write(json_tSNE, file = json_path_tSNE)

excel_path <- paste0("/home/jeffmxh/LDA_excel_data/", file_name, ".xlsx")
ssave_excel(target_data, excel_path)

print(Sys.time()-start_time)
# load("/home/jeffmxh/wechat_articles1.RData")
# target_file_name <- ""
# target_data <- data_raw
# target_column <- "content"
# file_name <- "肯德基圣诞烤鸡按关键词提取"


# rm(target_data, target_column, text_vec, doc.list, term.table,
#    del, vocab, documents, D, W, doc.length, N, term.frequency,
#    K, G, alpha, eta, fit, theta, phi, json)










