library(jiebaR)
library(parallel)

# 词汇情感分析------------------------------------

emotion_word_classify <- function(keyword, emotion_dict){
  emotions_temp <- emotion_dict %>% filter(word==keyword)
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
  sentences <- gsub(pattern = " ", replacement ="", sentences)
  sentences <- gsub("\t", "", sentences) #有时需要使用\\\t 
  sentences <- gsub("[[:digit:]]*", "", sentences) #清除数字[a-zA-Z]
  sentences <- gsub("[a-zA-Z]", "", sentences)
  sentences <- gsub("\\.", "", sentences)
  sentences <- gsub(",", "，", sentences)
  sentences <- gsub("~|'", "", sentences)
  sentences <- gsub("\\\"", "", sentences)
  return(sentences)
}

# 统计句子中的情感--------------------------------

emotion_sentence_stat <- function(content, emotion_dict){
  emotion_table <- data.frame(t(rep(0,21)))
  colnames(emotion_table) <- c("PA", "PE", "PD", "PH", "PG", "PB", "PK", "NZ", "NB", "NJ", "NH", 
                               "PF", "NI", "NC", "NG", "NE", "ND", "NN", "NK", "NL", "PC")
  cc = worker()
  seg_list <- cc[content]
  seg_list <- seg_list[seg_list %in% emotion_dict$word]
  a <- mclapply(1:length(seg_list), function(i){emotion_word_classify(seg_list[i], emotion_dict)}, mc.cores = 16)
  a <- do.call(rbind, a)
  result <- apply(a, 2, sum)
  return(result)
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

# 加载情感词典----------------------------------

emotion_load_dict <- function(filepath)
{
  emotion_dict <- read.xlsx(filepath)
  emotion_dict <- emotion_dict[1:10]
  colnames(emotion_dict)<- c("word", "pos", "meaning_count", "meaning_index", 
                             "emotion_1", "strength_1", "polar_1", 
                             "emotion_2", "strength_2", "polar_2")
  emotion_dict <- emotion_dict %>% filter(meaning_index==1)
  return(emotion_dict)
}

# 输入数据框处理函数----------------------------

emotion_analyse <- function(data_emotion, emotion_dict){
  stat_list = mclapply(1:nrow(data_emotion), function(i){emotion_sentence_stat(data_emotion$content[i],emotion_dict)}, mc.cores = 16)
  stat_list = do.call(rbind, stat_list)
  stat_sum = apply(stat_list,2,sum)
  stat_names = names(stat_sum)
  for(i in 1:length(stat_names)) stat_names[i] = emotion_trans(stat_names[i])
  stat_result = as.data.frame(stat_sum)
  rownames(stat_result)=1:nrow(stat_result)
  stat_result = data.frame("type" = stat_names, stat_result)
  return(stat_result)
}

# 准备数据--------------------------------------

data_temp <- lapply(1:nrow(data),function(i){data$content[i] = sentence_trim(data$content[i])})
data_temp <- do.call(rbind, data_temp)
data1 = data
data1$content=data_temp
rm(data_temp)

# 处理数据---------------------------------------

stat_result = emotion_analyse(data1, emotion_dict)

# 绘图分析结果

p = ggplot(stat_result, aes(x = type,y = stat_sum))
q = p + geom_bar(stat="identity") + xlab("情感") + ylab("加权求和") + ggtitle("宝来2016情感分析")
ggsave(file = "/home/jeffmxh/情感统计.png", plot=q, width = 30, height = 20, units = "cm")

