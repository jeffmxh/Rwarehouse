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

# 去除句子中的特殊符号----------------------------

sentence_trim <- function(sentences){
  sentences <- gsub("【.+?】", "", sentences)
  sentences <- gsub("\\#.+?\\#", "", sentences)
  sentences <- gsub("\\[.+?\\]", "", sentences)
  sentences <- gsub("\t", "", sentences) #有时需要使用\\\t 
  sentences <- gsub("[[:digit:]]*", "", sentences) #清除数字[a-zA-Z]
  sentences <- gsub("[a-zA-Z]", "", sentences)
  sentences <- gsub("~|'", "", sentences)
  sentences <- gsub("～", "", sentences)
  sentences <- gsub("[/_:：@-]", "", sentences)
  sentences <- gsub("\\\"", "", sentences)
  sentences <- gsub(pattern = " ", replacement ="", sentences)
  return(sentences)
}

# 统计句子中的情感--------------------------------

emotion_sentence_stat <- function(content, emotion_dict){
  emotion_table <- data.frame(t(rep(0,21)))
  colnames(emotion_table) <- c("PA", "PE", "PD", "PH", "PG", "PB", "PK", "NZ", "NB", "NJ", "NH", 
                               "PF", "NI", "NC", "NG", "NE", "ND", "NN", "NK", "NL", "PC")
  seg_list <- tryCatch(
    {
      cc = worker()
      seg_list <- cc[content]
      seg_list <- dplyr::intersect(seg_list, emotion_dict$word)#seg_list[seg_list %in% emotion_dict$word]
    },
    error = function(e){seg_list <- list()},
    warning = function(w){seg_list <- list()}
  )
  if(length(seg_list)==0){
    return(emotion_table)
  }else{
    result_list <- lapply(1:length(seg_list), function(i){emotion_word_classify(seg_list[i], emotion_dict)})
    result_list <- do.call(rbind, result_list)
    result <- apply(result_list, 2, sum)
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
         "PC" = "惊奇"
  )
}


# 输入数据框处理函数----------------------------

emotion_analyse <- function(data_emotion, emotion_dict){
  n_core = 1
  stat_list = mclapply(1:nrow(data_emotion), function(i){cat(i, "/", nrow(data_emotion), " done!\n", sep = "");
    return(emotion_sentence_stat(data_emotion$content[i],emotion_dict))}, 
    mc.cores = n_core)
  stat_list = do.call(rbind, stat_list)
  for(i in 1:ncol(stat_list)){
    colnames(stat_list)[i] <- emotion_trans(colnames(stat_list)[i])
  }
  result = list()
  result[["raw_data"]] <- stat_list
  stat_sum = apply(stat_list,2,sum)
  stat_names = names(stat_sum)
  stat_result = as.data.frame(stat_sum)
  rownames(stat_result)=1:nrow(stat_result)
  stat_result = data.frame("type" = stat_names, stat_result)
  result[["stat_result"]] <- stat_result
  return(result)
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

emotion_analysed_sign <- function(data_raw_text){
  n_core = 1
  result_list <- mclapply(1:nrow(data_raw_text), function(i){emotion_sign(data_raw_text[i,])}, mc.cores = n_core)
  result_table <- do.call(rbind, result_list)
  data_raw_text <- data.frame(data_raw_text, "emotion" = result_table)
  return(data_raw_text)
}