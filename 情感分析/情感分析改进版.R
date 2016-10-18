library(jiebaR)
library(parallel)
library(openxlsx)
library(dplyr)
library(ggplot2)
library(data.table)
# library(fastmatch)

# 加载情感词典----------------------------------

emotion_load_dict <- function(filepath)
{
  emotion_dict <- read.xlsx(filepath)
  emotion_dict <- emotion_dict[1:10]
  colnames(emotion_dict)<- c("word", "pos", "meaning_count", "meaning_index", 
                             "emotion_1", "strength_1", "polar_1", 
                             "emotion_2", "strength_2", "polar_2")
  emotion_dict <- emotion_dict %>% dplyr::filter(meaning_index==1)
  return(emotion_dict)
}

# 对评论进行分词处理---------------------------

emotion_text_segmenter <- function(data_emotion, column_deal){
  target_text <- data_emotion[, column_deal]
  cc <- worker()
  segment_list = mclapply(1:length(target_text), function(i){
    seg_list <- tryCatch(
      {
        cc[target_text[i]]
      },
      error = function(e){seg_list <- c()},
      warning = function(w){seg_list <- c()}
    )
    return(seg_list)
  },mc.cores = 32
  )
  return(segment_list)
}


# 词汇情感分析------------------------------------

emotion_word_classify <- function(keyword, emotion_dict){
  emotions_temp <- emotion_dict %>% dplyr::filter(word==keyword)
  emotions_temp <- emotions_temp[1,]
  emotion_table <- data.frame(t(rep(0,21)))
  colnames(emotion_table) <- c("PA", "PE", "PD", "PH", "PG", "PB", "PK", "NZ", "NB", "NJ", "NH", 
                               "PF", "NI", "NC", "NG", "NE", "ND", "NN", "NK", "NL", "PC")
  if(nrow(emotions_temp)==0){
    return(emotion_table)
  }else{
    emotion_table[1,emotions_temp$emotion_1] <- emotions_temp$strength_1
    if(!is.na(emotions_temp$emotion_2)){
      emotion_table[1,emotions_temp$emotion_2] <- emotions_temp$strength_2
    }
    return(emotion_table)
  }
}

# 统计句子中的情感--------------------------------

emotion_sentence_stat <- function(seg_list, emotion_dict){
  emotion_table <- data.frame(t(rep(0,21)))
  colnames(emotion_table) <- c("PA", "PE", "PD", "PH", "PG", "PB", "PK", "NZ", "NB", "NJ", "NH", 
                               "PF", "NI", "NC", "NG", "NE", "ND", "NN", "NK", "NL", "PC")
  seg_list <- dplyr::intersect(seg_list, emotion_dict$word)
    #seg_list[!is.na(fmatch(seg_list, emotion_dict$word))]
    # seg_list[seg_list %chin% emotion_dict$word]
  if(length(seg_list)==0){
    return(emotion_table)
  }else{
    a <- lapply(1:length(seg_list), function(i){emotion_word_classify(seg_list[i], emotion_dict)})
    a <- do.call(rbind, a)
    result <- apply(a, 2, sum)
    return(result)
  }
}

# 把情感标签转化为中文----------------------------

emotion_trans <- function(emo_eng){
  switch(emo_eng,
         "PA" = "快乐",         "PE" = "安心",
         "PD" = "尊敬",         "PH" = "赞扬",
         "PG" = "相信",         "PB" = "喜爱",
         "PK" = "祝愿",         "NZ" = "愤怒",
         "NB" = "悲伤",         "NJ" = "失望",
         "NH" = "内疚",         "PF" = "思",
         "NI" = "慌",           "NC" = "恐惧",
         "NG" = "羞",           "NE" = "烦闷",
         "ND" = "憎恶",         "NN" = "贬责",
         "NK" = "妒忌",         "NL" = "怀疑",
         "PC" = "惊奇",         "happy" = "乐",
         "praise" = "好",       "angry" = "怒",
         "sad" = "哀",          "fear" = "惧",
         "disagreeable" = "恶", "surprise" = "惊"
  )
}

# 情感归类--------------------------------------

emotion_classify <- function(data_analysed){
  data_analysed <- data.frame(
    data_analysed,
    "happy" = data_analysed$PA + data_analysed$PE,
    "praise" = data_analysed$PD + data_analysed$PH + data_analysed$PG + data_analysed$PB + data_analysed$PK,
    "angry" = data_analysed$NZ,
    "sad" =  data_analysed$NB + data_analysed$NJ + data_analysed$NH + data_analysed$PF,
    "fear" = data_analysed$NI + data_analysed$NC + data_analysed$NG,
    "disagreeable" = data_analysed$NE + data_analysed$ND + data_analysed$NN + data_analysed$NK + data_analysed$NL,
    "surprise" = data_analysed$PC
  )
  return(data_analysed)
}

# content标记情感--------------------------------------

emotion_sign <- function(result_line){
  if(sum(result_line[22:28])==0){
    return("None")
  }else{
    result_line_sub <- result_line[22:28]
    emotion <- colnames(result_line_sub)[result_line_sub==max(result_line_sub)]
    return(emotion[1])
  }
}

# 数据集标记情感---------------------------------------

emotion_analysed_sign <- function(data_analysed){
  n_core = 32
  result_list <- mclapply(1:nrow(data_analysed), function(i){emotion_sign(data_analysed[i,])}, mc.cores = n_core)
  result_table <- do.call(rbind, result_list)
  data_analysed <- data.frame(data_analysed, "emotion" = result_table)
  return(data_analysed)
}

# 输入数据框处理函数----------------------------

emotion_analyse <- function(data_emotion, column_to_deal, emotion_dict){
  segment_list <- emotion_text_segmenter(data_emotion, column_to_deal)
  stat_list = mclapply(1:length(segment_list), function(i){
    emotion_sentence_stat(segment_list[[i]],emotion_dict)}, 
    mc.cores = 32)
  stat_list = do.call(rbind, stat_list) %>% as.data.frame()
  stat_list <- emotion_classify(stat_list)
  for(i in 1:ncol(stat_list)){
    colnames(stat_list)[i] <- emotion_trans(colnames(stat_list)[i])
  }
  stat_list <- emotion_analysed_sign(stat_list)
  result = list()
  result[["raw_data"]] <- cbind(data_emotion, stat_list)
  stat_sum = apply(stat_list[,1:28],2,sum)
  stat_names = names(stat_sum)
  stat_result = as.data.frame(stat_sum)
  rownames(stat_result)=1:nrow(stat_result)
  stat_result = data.frame("type" = stat_names, stat_result)
  result[["stat_result"]] <- stat_result
  return(result)
}
# data_raw = as.data.frame(articles1)
time_temp = Sys.time()
result_temp = emotion_analyse(data_raw[1:10000,], "content", emotion_dict)
cat("用时:", Sys.time() - time_temp, sep = "")
# result_all$raw_data = rbind(result_all$raw_data, result_temp$raw_data)
# result_all$stat_result$stat_sum = result_all$stat_result$stat_sum + result_temp$stat_result$stat_sum
# p = ggplot(result$stat_result, aes(x = type,y = stat_sum, fill = type))
# p = p + geom_bar(stat="identity") + xlab("情感") + ylab("加权求和") + ggtitle("情感统计") + theme(legend.position = "none")