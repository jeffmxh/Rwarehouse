library(jiebaR)
library(parallel)
library(openxlsx)
library(dplyr)
library(ggplot2)
# library(data.table)
# library(fastmatch)

# 加载情感词典----------------------------------

emotion_load_dict <- function(filepath)
{
  emotion_dict <- read.xlsx(filepath)
  emotion_dict <- emotion_dict[1:10]
  colnames(emotion_dict)<- c("word", "pos", "meaning_count", "meaning_index", 
                             "emotion_1", "strength_1", "polar_1", 
                             "emotion_2", "strength_2", "polar_2")
  emotion_dict <- emotion_dict %>% 
    dplyr::filter(meaning_index==1) %>%
    dplyr::filter(polar_1 %in% c(0,1,2))
  emotion_dict[emotion_dict$polar_1==2,7]=-1
  return(emotion_dict)
}

# 对评论进行分词处理---------------------------

emotion_text_segmenter <- function(data_emotion, column_deal){
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
  },mc.cores = 16
  )
  return(segment_list)
}


# 词汇情感分析------------------------------------

emotion_word_classify <- function(keyword, emotion_dictionary){
  emotions_temp <- emotion_dictionary %>% dplyr::filter(word==keyword)
  
  emotion_table <- data.frame(t(rep(0,22)))
  colnames(emotion_table) <- c("PA", "PE", "PD", "PH", "PG", "PB", "PK", "NZ", "NB", "NJ", "NH", 
                               "PF", "NI", "NC", "NG", "NE", "ND", "NN", "NK", "NL", "PC", "POLAR")
  if(nrow(emotions_temp)==0){
    return(emotion_table)
  }else{
    emotions_temp <- emotions_temp[1,]
    emotion_table[1, emotions_temp$emotion_1] <- emotions_temp$strength_1
    emotion_table[1, "POLAR"] <- emotions_temp$polar_1
    if(!is.na(emotions_temp$emotion_2)){
      emotion_table[1,emotions_temp$emotion_2] <- emotions_temp$strength_2
    }
    return(emotion_table)
  }
}

# 分析句子情感, 用于分析结果------------------------------------

emotion_sentence_analyse <- function(sentence, emotion_dictionary){
  cc <- worker()
  seg_list <- cc[sentence]
  if(length(seg_list)>1){
    seg_list <- seg_list[seg_list %in% emotion_dict$word]
  }
  result_list <- lapply(1:length(seg_list), function(i){emotion_word_classify(seg_list[i], emotion_dictionary)})
  result_list <- do.call(rbind, result_list)
  result_list <- emotion_classify(result_list)
  result_list <- data.frame("word" = seg_list, result_list)
  result <- list()
  result[["原文"]] <- sentence
  result[["褒义"]] <- result_list %>% filter(POLAR>0) %>% select(word) %>% unlist() %>% as.character()
  result[["贬义"]] <- result_list %>% filter(POLAR<0) %>% select(word) %>% unlist() %>% as.character()
  result[["乐"]] <- result_list %>% filter(happy>0) %>% select(word) %>% unlist() %>% as.character()
  result[["好"]] <- result_list %>% filter(praise>0) %>% select(word) %>% unlist() %>% as.character()
  result[["怒"]] <- result_list %>% filter(angry>0) %>% select(word) %>% unlist() %>% as.character()
  result[["哀"]] <- result_list %>% filter(sad>0) %>% select(word) %>% unlist() %>% as.character()
  result[["惧"]] <- result_list %>% filter(fear>0) %>% select(word) %>% unlist() %>% as.character()
  result[["恶"]] <- result_list %>% filter(disagreeable>0) %>% select(word) %>% unlist() %>% as.character()
  result[["惊"]] <- result_list %>% filter(surprise>0) %>% select(word) %>% unlist() %>% as.character()
  return(result)
}

# 统计句子中的情感--------------------------------

emotion_sentence_stat <- function(seg_list, emotion_dict){
  emotion_table <- data.frame(t(rep(0,22)))
  colnames(emotion_table) <- c("PA", "PE", "PD", "PH", "PG", "PB", "PK", "NZ", "NB", "NJ", "NH",
                               "PF", "NI", "NC", "NG", "NE", "ND", "NN", "NK", "NL", "PC", "POLAR")
  # sub_emotion_dict <- emotion_dict[emotion_dict$word %in% seg_list,]
  sub_emotion_dict <- emotion_dict %>% dplyr::filter(word %in% seg_list)
  if(nrow(sub_emotion_dict)==0){
    return(emotion_table)
  }else{
    if(length(seg_list)!=1){
      seg_list <- seg_list[seg_list %in% sub_emotion_dict$word]
    }
    a <- lapply(1:length(seg_list), function(i){emotion_word_classify(seg_list[i], sub_emotion_dict)})
    a <- do.call(rbind, a)
    result <- apply(a, 2, sum)
    return(result)
  }
}

# 把情感标签转化为中文----------------------------

emotion_trans <- function(emo_eng){
  # emo_vec <- c("PA", "PE", "PD", "PH", "PG", "PB", "PK", "NZ", "NB", "NJ", "NH",
  #              "PF", "NI", "NC", "NG", "NE", "ND", "NN", "NK", "NL", "PC", "happy",
  #              "praise", "angry", "sad", "fear", "disagreeable", "surprise", "POLAR")
  # names(emo_vec) <- c("快乐", "安心", "尊敬", "赞扬", "相信", "喜爱", "祝愿", "愤怒", "悲伤",
  #                     "失望", "内疚", "思", "慌", "恐惧", "羞", "烦闷", "憎恶", "贬责", "妒忌",
  #                     "怀疑", "惊奇", "语义", "乐", "好", "怒",  "哀", "惧", "恶", "惊")
  # return(names(emo_vec[emo_vec==emo_eng]))
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
         "disagreeable" = "恶", "surprise" = "惊",
         "POLAR" = "语义"
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
  if(sum(result_line[23:29])==0){
    return("None")
  }else{
    result_line_sub <- result_line[23:29]
    emotion <- colnames(result_line_sub)[result_line_sub==max(result_line_sub)]
    return(emotion[1])
  }
}

# 数据集标记情感---------------------------------------

emotion_analysed_sign <- function(data_analysed){
  n_core = 16
  result_list <- mclapply(1:nrow(data_analysed), function(i){emotion_sign(data_analysed[i,])}, mc.cores = n_core)
  result_table <- do.call(rbind, result_list)
  data_analysed <- data.frame(data_analysed, "emotion" = result_table)
  return(data_analysed)
}

# 输入数据框处理函数----------------------------

emotion_analyse <- function(data_emotion, column_to_deal, emotion_dict){
  cat("------------------------------------------\n")
  cat("开始分词：\n")
  time_temp <- Sys.time()
  segment_list <- emotion_text_segmenter(data_emotion, column_to_deal)
  cat("分词完成，用时：", Sys.time()-time_temp, "\n", sep = "")
  cat("------------------------------------------\n")
  time_temp <- Sys.time()
  cat("开始情感分析：\n")
  stat_list <- mclapply(1:length(segment_list), function(i){
    emotion_sentence_stat(segment_list[[i]],emotion_dict)},
    mc.cores = 16)
  cat("情感分析完成，用时：", Sys.time()-time_temp, "\n", sep = "")
  cat("------------------------------------------\n")
  time_temp <- Sys.time()
  cat("开始do.call()：\n")
  stat_list <- do.call(rbind, stat_list) %>% as.data.frame()
  cat("do.call()完成，用时：", Sys.time()-time_temp, "\n", sep = "")
  cat("------------------------------------------\n")
  time_temp <- Sys.time()
  cat("开始情感分类：\n")
  stat_list <- emotion_classify(stat_list)
  cat("情感分类完成，用时：", Sys.time()-time_temp, "\n", sep = "")
  cat("------------------------------------------\n")
  time_temp <- Sys.time()
  cat("开始转化情感中文标记：\n")
  colnames(stat_list) <- c("快乐", "安心", "尊敬", "赞扬", "相信", "喜爱", "祝愿", 
                           "愤怒", "悲伤", "失望", "内疚", "思", "慌", "恐惧", "羞",
                           "烦闷", "憎恶", "贬责", "妒忌", "怀疑", "惊奇", "语义", 
                           "乐", "好", "怒",  "哀", "惧", "恶", "惊")
  cat("转化情感中文标记完成，用时：", Sys.time()-time_temp, "\n", sep = "")
  cat("------------------------------------------\n")
  time_temp <- Sys.time()
  cat("开始标记情感分类：\n")
  stat_list <- emotion_analysed_sign(stat_list)
  cat("标记情感分类完成，用时：", Sys.time()-time_temp, "\n", sep = "")
  cat("------------------------------------------\n")
  time_temp <- Sys.time()
  cat("开始生成结果：\n")
  result = list()
  result[["raw_data"]] <- cbind(data_emotion, stat_list)
  stat_sum <- apply(stat_list[,1:29], 2, sum)
  stat_names <- names(stat_sum)
  stat_result <- as.data.frame(stat_sum)
  rownames(stat_result) <- 1:nrow(stat_result)
  stat_result <- data.frame("type" = stat_names, stat_result)
  result[["stat_result"]] <- stat_result
  cat("成功生成结果对象，用时：", Sys.time()-time_temp, "\n", sep = "")
  cat("------------------------------------------\n")
  return(result)
}
# 分析过程------------------------------------------
# data_raw = as.data.frame(articles1)
time_temp <- Sys.time()
result_emotion <- emotion_analyse(data_raw, "content", emotion_dict)
cat("总计用时:", Sys.time() - time_temp, sep = "")
rm(time_temp)
# result_all$raw_data <- rbind(result_all$raw_data, result_emotion$raw_data)
# result_all$stat_result$stat_sum <- result_all$stat_result$stat_sum + result_emotion$stat_result$stat_sum
# write.table(result_all$raw_data, file = "/home/jeffmxh/emotion_result_all.txt", row.names = FALSE, sep = "\t")
# rm(result_emotion)
# 画图显示结果--------------------------------------
p = ggplot(result_all$stat_result[-22,], aes(x = type,y = stat_sum, fill = type))
p = p + geom_bar(stat="identity") + xlab("情感") + ylab("加权求和") + ggtitle("情感统计") + theme(legend.position = "none")
ggsave(file = "/home/jeffmxh/情感统计.png", plot=p, width = 30, height = 20, units = "cm")
