Args <- commandArgs()
options(warn = -1)
require(readxl, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(jiebaR, quietly = TRUE)
require(parallel, quietly = TRUE)
require(LDAvis, quietly = TRUE)
require(lda, quietly = TRUE)
require(RMySQL, quietly = TRUE)

# 根据SQL语言查询数据库-----------------------------

source('/home/jeffmxh/r projects/mysql_io.R')

# load data---------------------------------------------

keyword_id <- Args[6]

cat("-----------------------", keyword_id, "-----------------------\n")
start_time <- Sys.time()
print(start_time)

query0 <- paste0("SELECT info_source_sheet FROM project_key WHERE keyword_id=\'", keyword_id, "\'")
info_source_sheet <- get_db_data(query0)
info_source_sheet <- as.character(info_source_sheet)

query1 <- paste0("SELECT keyword FROM project_key WHERE keyword_id=\'", keyword_id, "\'")
project_name <- get_db_data(query1)
project_name <- as.character(project_name)

query2 <- paste0("SELECT * FROM ", info_source_sheet, " WHERE keyword_id=\'", keyword_id, "\'")
target_data <- get_db_data(query2)
target_data <- as.data.frame(target_data)

if(grepl(project_name, pattern = "转发")){
  target_data <- target_data %>%
    mutate(bind_content = paste(content, retweeted_content, sep = ","))
  target_column <- "bind_content"
}else{
  target_column <- "content"
}

cat("Data loaded!\n")
file_name <- project_name
K <- floor(sqrt(nrow(target_data))/3)

# load the stop_words list------------------------------

source("/home/jeffmxh/r projects/txt_excel_io.R")
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

text_vec <- gsub(" ", ",",text_vec)
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
del <- term.table < 5
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

# D <- length(documents)
# W <- length(vocab)  # number of terms in the vocab (14,568)
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document [312, 288, 170, 436, 291, ...]
# N <- sum(doc.length)  # total number of tokens in the data (546,827)
term.frequency <- as.integer(term.table)  # frequencies of terms in the corpus [8939, 5544, 2411, 2410, 2143,
cat("Statistics computed!\n")

# MCMC and model tuning parameters--------------------------


G <- 1000
alpha <- 0.02
eta <- 0.02

# Fit the model---------------------------------------

cat("Start fitting model!\n")
set.seed(2016)
fit <- lda.collapsed.gibbs.sampler(documents = documents, 
                                     K = K, 
                                     vocab = vocab, 
                                     num.iterations = G, 
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
# json_jsPCA <- createJSON(phi = phi, 
#                    theta = theta, 
#                    doc.length = doc.length, 
#                    vocab = vocab, 
#                    term.frequency = term.frequency,
#                    cluster = cl)

cat("Json file generated!\n")

stopCluster(cl)

# View result--------------------------------------------------

# json_path_jsPCA <- paste0("/home/jeffmxh/LDA_json_data/R_", file_name, "_jsPCA.json")
# # save(json_jsPCA, file = json_path_jsPCA)
# write(json_jsPCA, file = json_path_jsPCA)

json_path_tSNE <- paste0("/home/jeffmxh/LDA_json_data/R_", keyword_id, file_name, ",json")
write(json_tSNE, file = json_path_tSNE)

excel_path <- paste0("/home/jeffmxh/LDA_excel_data/", file_name, ".xlsx")
ssave_excel(target_data, excel_path)

print(Sys.time()-start_time)






