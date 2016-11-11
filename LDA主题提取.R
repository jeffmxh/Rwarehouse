library(tm)
library(dplyr)
library(jiebaR)
library(parallel)
library(topicmodels)
library(igraph)

# 对数据框特定列分词，返回list--------------------------
stop_word_path = "/home/jeffmxh/stopwords_utf8.txt"
cc <- worker(stop_word = stop_word_path)
emotion_text_segmenter <- function(data_emotion, column_deal){
  if(.Platform$OS.type=="windows"){
    n_cores = 1
  }else{
    n_cores = floor(detectCores(logical = TRUE) * 0.75)
  }
  target_text <- data_emotion[, column_deal]
  cc <- worker()
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

# generate dtm matrix--------------------------------------------

data_to_deal <- content_all
all_seg_list <- emotion_text_segmenter(data_to_deal, "content")

corpus <- Corpus(VectorSource(all_seg_list))
dtm_matrix <- make_dtm(corpus, stopwords_list = stop_words)

row_sum <- apply(as.matrix(dtm_matrix), 1, sum)
delete_index <- as.numeric(names(row_sum[row_sum==0]))
delete_index <- sort(delete_index, decreasing = TRUE)
data_to_deal <- data_to_deal[-delete_index, ]
for(i in delete_index){
  all_seg_list[[i]] <- NULL
}
corpus <- Corpus(VectorSource(all_seg_list))
dtm_matrix <- make_dtm(corpus, stopwords_list = stop_words)

# Select number of topics-----------------------------------------------

fold_num <- 10
kv_num <- c(5, 10*c(1:5), 100)
seed_num <- 2003


smp<-function(cross=fold_num, n, seed)
{
  set.seed(seed)
  dd=list()
  aa0=sample(rep(1:cross,ceiling(n/cross))[1:n],n)
  for (i in 1:cross) dd[[i]]=(1:n)[aa0==i]
  return(dd)
}

selectK < -function(dtm, kv = kv_num, SEED = seed_num, cross = fold_num, sp) # change 60 to 15
{
  per_ctm = NULL
  log_ctm = NULL
  for (k in kv)
  {
    per = NULL
    loglik = NULL
    for (i in 1:3)  #only run for 3 replications#
    {
      cat("R is running for", "topic", k, "fold", i,
          as.character(as.POSIXlt(Sys.time(), "Asia/Shanghai")),"\n")
      te=sp[[i]]
      tr=setdiff(1:nrow(dtm), te)
      
      # VEM = LDA(dtm[tr, ], k = k, control = list(seed = SEED)),
      # VEM_fixed = LDA(dtm[tr,], k = k, control = list(estimate.alpha = FALSE, seed = SEED)),
      
      # CTM = topicmodels::CTM(dtm[tr, ], k = k,
      #           control = list(seed = SEED, var = list(tol = 10^-4), em = list(tol = 10^-3)))
      
      Gibbs = LDA(dtm[tr,], k = k, method = "Gibbs",
                  control = list(seed = SEED, burnin = 1000,thin = 100, iter = 1000))
      
      per = c(per,perplexity(Gibbs, newdata = dtm[te,]))
      loglik = c(loglik, logLik(Gibbs, newdata = dtm[te,]))
    }
    per_ctm = rbind(per_ctm, per)
    log_ctm = rbind(log_ctm, loglik)
  }
  return(list(perplex = per_ctm, loglik = log_ctm))
}

# transform to parallel --------------------------------

selectK_par <- function(dtm,kv = kv_num, SEED = seed_num, cross = fold_num, sp){ # change 60 to 15
  if(.Platform$OS.type=="windows"){
    max_cores = 1
  }else{
    max_cores = floor(detectCores(logical = TRUE) * 0.75)
  }
  result_list <- mclapply(kv, function(k){
    result_list_k <- mclapply(1:3, function(i){
      te <- sp[[i]]
      tr <- setdiff(1:dtm$nrow, te)
      Gibbs = LDA(dtm[tr,], k = k, method = "Gibbs",
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


sp=smp(n = dtm_matrix$nrow, seed = seed_num)

system.time((ldaK <- selectK_par(dtm = dtm_matrix, kv = kv_num, SEED=seed_num, cross=fold_num, sp=sp)))

# plot the perplexity-------------------------------

m_per=apply(ldaK[[1]],1,mean)
m_log=apply(ldaK[[2]],1,mean)

k=c(kv_num)
df = ldaK[[1]]  # perplexity matrix
matplot(k, df, type = c("b"), xlab = "Number of topics",
        ylab = "Perplexity", pch=1:5,col = 1, main = '')
legend("bottomright", legend = paste("fold", 1:5), col=1, pch=1:5)

# 正式进行模型拟合------------------------------------

Gibbs <- LDA(dtm_matrix, k = 30, method = "Gibbs",control = list(seed = 2015, burnin = 1000,thin = 100, iter = 1000))
terms(Gibbs, 10)

# 绘图可视化------------------------------------------

termsForSave3 = terms(Gibbs, 10)
tfs = as.data.frame(termsForSave3, stringsAsFactors = F); tfs[,1]
adjacent_list = lapply(1:10, function(i) embed(tfs[,i], 2)[, 2:1]) 
edgelist = as.data.frame(do.call(rbind, adjacent_list), stringsAsFactors =F)
topic = unlist(lapply(1:10, function(i) rep(i, 9)))
edgelist$topic = topic
g <-graph.data.frame(edgelist,directed=T )
l<-layout.fruchterman.reingold(g)
edge.color="black"
nodesize = centralization.degree(g)$res 
V(g)$size = log( centralization.degree(g)$res )

nodeLabel = V(g)$name
E(g)$color =  unlist(lapply(sample(colors()[26:137], 10), function(i) rep(i, 9))); unique(E(g)$color)

# 保存图片格式
png(paste(getwd(), "/topic_graph_gibbs.png", sep=""), width=5, height=5,  units="in", res=700)
plot(g, vertex.label= nodeLabel,  edge.curved=TRUE, 
     vertex.label.cex =0.8,  edge.arrow.size=0.04, layout=l )

# 结束保存图片
dev.off()

# generate final result------------------------------------------------

Gibbs <- LDA(dtm_matrix, k = 5, method = "Gibbs",control = list(seed = 2015, burnin = 1000,thin = 100, iter = 1000))
terms(Gibbs, 10)


topic1 <- topics(Gibbs, 1)
table(topic1)

Terms1 <- terms(Gibbs, 10)
Terms1