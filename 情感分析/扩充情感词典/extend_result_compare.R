# 设置核心数------------------------------------

get_cores <- function(){
  if(.Platform$OS.type=="windows"){
    n_cores <- 1
  }else{
    n_cores <- floor(detectCores(logical = TRUE)/2)
  }
  return(n_cores)
}

# 对评论进行分词处理---------------------------

emotion_text_segmenter <- function(data_emotion, column_deal){
  require(jiebaR, quietly = TRUE)
  require(parallel, quietly = TRUE)
  n_cores <- get_cores()
  data_emotion <- as.data.frame(data_emotion)
  target_text <- as.character(data_emotion[, column_deal])
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
  },mc.cores = n_cores
  )
  return(segment_list)
}

# 加载扩充前后的词典----------------------------------

pos_list <- readLines('full_pos_dict.txt')
neg_list <- readLines('full_neg_dict.txt')
big_list = c(pos_list, neg_list)
load('emotion_dict.RData')
load('extend_dict.RData')
cat('词典加载完成！\n')

# 从数据库读取数据并分词------------------------------

source('/home/jeffmxh/r projects/mysql_io.R')
data = get_db_data("SELECT * FROM weibo_messages WHERE keyword_id='30_8'")
cat('数据读取完成！\n')
seg_list = emotion_text_segmenter(data, 'content')
cat('分词完成！\n')

# result_list = lapply(seg_list, function(segments){length(segments[segments %in% big_list])})
# count_new = unlist(result_list)
# print(length(count_new[count_new!=0]))
# 
# 
# result_old = lapply(seg_list, function(segments){length(segments[segments %in% sub_dict$word])})
# count_old = unlist(result_old)
# print(length(count_old[count_old!=0]))

a = unique(unlist(seg_list))
dif1 = a[a %in% big_list]
dif2 = a[a %in% sub_dict$word]
extend_words <- dif1[!dif1 %in% dif2]

# 由扩充的词汇查询原始词汇----------------------------

find_origin_word <- function(word, example_dict){
  require(dplyr, quietly = TRUE)
  word_wrap <- paste0("'", word, "'")
  origin_word <- example_dict %>%
    dplyr::filter(grepl(nearby_words, pattern = word_wrap)) 
  result_word = origin_word$word[1]
  result_polar = origin_word$polar_1[1]
  return(c(result_word, result_polar))
}

compare_table = data.frame('extend_word' = extend_words)
result_list = lapply(1:nrow(compare_table), function(i){find_origin_word(compare_table$extend_word[i], extend_dict)})
result = do.call(rbind, result_list)
colnames(result) = c("origin_word", "polar")
compare_table = cbind(compare_table, result)

all_seg_list = unlist(seg_list)
all_seg_list= all_seg_list[all_seg_list %in% extend_words]
extend_word_count <- plyr::count(all_seg_list) %>% arrange(desc(freq))
colnames(extend_word_count)[1] = "extend_word" 

compare_table <- compare_table %>% 
  left_join(extend_word_count, by = "extend_word") %>% 
  arrange(desc(freq))

View(compare_table)
