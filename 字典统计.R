dict_vec = c("元旦","小寒","大寒","腊八节","小年","立春","除夕","春节","情人节","雨水","元宵节","上元节","惊蛰","二月二","龙抬头","春分","寒食节","清明节","谷雨","立夏","佛诞","母亲节","小满","芒种","端午节","夏至","小暑","大暑","立秋","七夕","中元节","处暑","白露","中秋节","秋分","国庆节","寒露","重阳节","霜降","立冬","小雪","大雪","冬至")

##################################
#统计条数
find_dict_word <- function(sentence){
  data = c()
  for(i in 1:length(dict_vec)){
    if(grepl(dict_vec[i],sentence)) data = c(data, dict_vec[i])
  }
  return(data)
}
find_all_dict_content <- function(data, colname){
  dict=c()
  for(i in 1:nrow(data)) {
    a=find_dict_word(data[i,colname])
    if(!is.null(a)) dict = c(dict,a)
  }
  temp =  plyr::count(dict) %>%
    dplyr::arrange(desc(freq)) %>%
    rename("keyword"=x)
   return(temp)
}

##################################
#单纯统计频率
find_all_dict_word <- function(data, colname){
stat=data.frame("keyword"=dict_vec, freq=rep(0,length(dict_vec)))
all_text = paste0(data[,colname], collapse = ",")
for(i in 1:nrow(stat)){
  stat[i,2] = length(strsplit(all_text,stat[i,1])[[1]])-1
}
stat = stat %>% filter(freq>0) %>% arrange(desc(freq))
return(stat)
}