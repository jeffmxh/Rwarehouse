require(tm, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(jiebaR, quietly = TRUE)
require(parallel, quietly = TRUE)
require(topicmodels, quietly = TRUE)
# require(igraph, quietly = TRUE)
require(Cairo, quietly = TRUE)

####################################################
# 根据SQL语言查询数据库-----------------------------

killDbConnections <- function(){
  if(length(dbListConnections(MySQL()))!=0){
    all_cons <- dbListConnections(MySQL())
    for(con in all_cons) dbDisconnect(con)
  }
}

get_db_data <- function(dbname="dp_relation",sql.str){
  require(RMySQL, quietly = TRUE)
  killDbConnections()
  db.con <- dbConnect(MySQL(),  
                      user = "shiny",
                      password = "shiny@tbs2016",
                      dbname = dbname,
                      host = "127.0.0.1")
  dbSendQuery(db.con, 'SET NAMES utf8')
  res <- dbSendQuery(db.con, sql.str)
  result <- dbFetch(res, n = -1)
  dbClearResult(res)
  dbDisconnect(db.con)
  return(result)
}

# 参数修改-----------------------------

start_time <- Sys.time()

# load("/home/jeffmxh/wechat_articles1.RData")
# target_data <- articles1[1:200,]
# target_data <- get_db_data("dp_relation", "SELECT * FROM weibo_raw_data WHERE keyword_id='10_1'")
target_column <- "content"
project_name <- "try_samp"

# 修改工作目录--------------------------

origin_wd <- getwd()
project_path <- paste0("/home/jeffmxh/LDA_result/", project_name)
dir.create(project_path)
setwd(project_path)
stop_words <- readLines("/home/jeffmxh/stopwords_utf8.txt", encoding = "UTF-8")

# load io_module-----------------

source("/home/jeffmxh/r projects/txt_excel_io.R")

# connect to spark---------------

# source('/home/jeffmxh/r projects/spark_connect.R')

# 对数据框特定列分词，返回list--------------------------

emotion_text_segmenter <- function(data_emotion, column_deal, stop_word_path = "/home/jeffmxh/stopwords_utf8.txt"){
  require(jiebaR, quietly = TRUE)
  require(parallel, quietly = TRUE)
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

# 生成dtm矩阵------------------------------------------------

make_dtm <- function(word_corpus, 
                     min_word_len = 2, min_repeat = 1, 
                     rmNum = TRUE, rmPun = FALSE, stopwords_list = stopwords.list,
                     wt = weightTf, encod = "UTF-8"){
  require(tm, quietly = TRUE)
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

# 进行抽样---------------------------------------------

smp <- function(cross=fold_num, n, seed)
{
  set.seed(seed)
  dd=list()
  aa0=sample(rep(1:cross,ceiling(n/cross))[1:n],n)
  for (i in 1:cross) dd[[i]]=(1:n)[aa0==i]
  return(dd)
}


# find the number of topics --------------------------------

selectK_par <- function(dtm,kv = kv_num, SEED = 2016, cross = fold_num, sp){ # change 60 to 15
  require(parallel, quietly = TRUE)
  require(topicmodels, quietly = TRUE)
  if(.Platform$OS.type=="windows"){
    max_cores <- 1
  }else{
    max_cores <- floor(detectCores(logical = TRUE) * 0.75)
  }
  result_list <- mclapply(kv, function(k){
    result_list_k <- mclapply(1:3, function(i){
      te <- sp[[i]]
      tr <- setdiff(1:dtm$nrow, te)
      
      # Gibbs <- LDA(dtm[tr, ], k = k, control = list(seed = SEED)),
      # VEM_fixed <- LDA(dtm[tr,], k = k, control = list(estimate.alpha = FALSE, seed = SEED)),
      
      # Gibbs <- topicmodels::CTM(dtm[tr, ], k = k,
      #                          control = list(seed = SEED, var = list(tol = 10^-4), em = list(tol = 10^-3)))
      
      Gibbs <- LDA(dtm[tr,], k = k, method = "Gibbs",
                  control = list(seed = SEED, burnin = 1000,thin = 100, iter = 1000))
      per <- perplexity(Gibbs, newdata = dtm[te,])
      loglik <- logLik(Gibbs, newdata = dtm[te,])
      return(c(per, loglik))
    }, mc.cores = 3)
    per <- unlist(result_list_k)[c(1,3,5)]
    loglik <- unlist(result_list_k)[c(2,4,6)]
    return(c(per, loglik))
  }, mc.cores = 7)
  result_table <- do.call(rbind, result_list)
  per_lda <- result_table[,1:3]; rownames(per_lda) <- rep("per", 7)
  log_lda <- result_table[,4:6]; rownames(log_lda) <- rep("loglik", 7)
  return(list(perplex = per_lda, loglik = log_lda))
}

# filter terms whose length is less than 2-----------------------

nchar_bool <- function(str){
  return(nchar(str)>1)
}

# generate dtm matrix--------------------------------------------

make_dtm_for_LDA <- function(data_to_deal, column_to_deal){
  # 分词----
  all_seg_list <- emotion_text_segmenter(data_to_deal, column_deal = column_to_deal)
  all_seg_list <- lapply(all_seg_list, function(vec){vec[nchar_bool(vec)]})
  corpus <- Corpus(VectorSource(all_seg_list))
  dtm_matrix <- make_dtm(corpus, stopwords_list = stop_words)
  # filter empty rows----
  row_sum <- apply(as.matrix(dtm_matrix), 1, sum)
  delete_index <- as.numeric(names(row_sum[row_sum<=5]))
  delete_index <- sort(delete_index, decreasing = TRUE)
  data_to_deal <- data_to_deal[-delete_index, ]
  for(i in delete_index){
    all_seg_list[[i]] <- NULL
  }
  corpus <- Corpus(VectorSource(all_seg_list))
  dtm_matrix <- make_dtm(corpus, stopwords_list = stop_words)
  return(list("raw_data" = data_to_deal, "dtm_matrix" = dtm_matrix))
}

# Select number of topics-----------------------------------------------

fold_num <- 10
kv_num <- c(5, 10*c(1:5), 100)

for(i in 1:nrow(target_data)){
  target_data[i, target_column] <- gsub("[a-zA-Z0-9:/]", "", target_data[i, target_column])
}
cat("去除无用词完毕!\n")

dtm_result <- make_dtm_for_LDA(target_data, target_column)
cat("DTM矩阵生成完毕!\n")

dtm_matrix <- dtm_result$dtm_matrix
target_data <- dtm_result$raw_data
rm(dtm_result)

sp <- smp(n = dtm_matrix$nrow, seed = 2016)

cat("开始并行拟合模型!\n")
ldaK <- selectK_par(dtm = dtm_matrix, kv = kv_num, SEED = 2016, cross = fold_num, sp = sp)
cat("模型拟合完毕!\n")

# plot the perplexity-------------------------------

m_per <- apply(ldaK[[1]],1,mean)
m_log <- apply(ldaK[[2]],1,mean)

df_1 <-  ldaK[[1]]  # perplexity matrix
CairoPNG("Perplexity.png", width=5, height=5, units="in", res=700)
matplot(kv_num, df_1, type = c("b"), xlab = "Number of topics",
        ylab = "Perplexity", pch=1:5, col = 1, main = "")
dev.off()

df_2 = ldaK[[2]]  # perplexity matrix
CairoPNG("likelyhood.png", width=5, height=5, units="in", res=700)
matplot(kv_num, df_2, type = c("b"), xlab = "Number of topics",
        ylab = "Likelyhood", pch=1:5, col = 1, main = '')
dev.off()
cat("图片保存完毕!\n")

# 正式进行模型拟合------------------------------------

# Gibbs <- topicmodels::CTM(dtm_matrix, k = 10, control = list(seed = 2016, var = list(tol = 10^-4), em = list(tol = 10^-3)))
# Gibbs <- topicmodels::LDA(dtm_matrix, k = 20, method = "Gibbs",control = list(seed = 2015, burnin = 1000,thin = 100, iter = 1000))
# topicmodels::terms(Gibbs, 10)

# 绘图可视化------------------------------------------

# plot_terms <- function(LDA_result, nterms) {
#   terms_10 <- terms(Gibbs, nterms)
#   tfs <- as.data.frame(terms_10, stringsAsFactors = F)
#   adjacent_list <- lapply(1:10, function(i) embed(tfs[,i], 2)[, 2:1]) 
#   edgelist <- as.data.frame(do.call(rbind, adjacent_list), stringsAsFactors = FALSE)
#   topic <- unlist(lapply(1:10, function(i) rep(i, 9)))
#   edgelist$topic <- topic
#   g <- graph.data.frame(edgelist,directed = T)
#   l <- layout.fruchterman.reingold(g)
#   edge.color <- "black"
#   nodesize <- centralization.degree(g)$res 
#   V(g)$size <- log( centralization.degree(g)$res )
#   nodeLabel <- V(g)$name
#   E(g)$color <-  unlist(lapply(sample(colors()[26:137], 10), function(i) rep(i, 9)))
#   return(g)
# }

# g <- plot_terms(LDA_result = Gibbs, nterms = 10)
# 保存图片格式
# CairoPNG("net_graph.png", width=5, height=5,  units="in", res=700)
# plot(g, vertex.label= nodeLabel,  edge.curved = TRUE, 
#      vertex.label.cex = 0.8,  edge.arrow.size = 0.15, layout = l )
# dev.off()
# 
# # generate final result------------------------------------------------
# 
# topic1 <- topics(Gibbs, 1)
# table(topic1)
# 
# terms <- terms(Gibbs, 10)
# terms
# 

save(project_name, target_data, target_column, file = "/home/jeffmxh/LDA_temp_vis.RData")
setwd(origin_wd)
cat("总计用时：", Sys.time()-start_time, "\n")
# rm(origin_wd, fold_num, kv_num, sp, ldaK, m_per, m_log, df, Gibbs, terms_10, tfs, adjacent_list, edgelist, topic, g, l, edge.color, nodesize, nodeLabel)
