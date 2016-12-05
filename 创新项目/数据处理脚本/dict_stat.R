##################################
#主函数

find_dict_word <- function(data, colname, dict, dupli_bool = TRUE, filter_bool = TRUE){
  colnames(data)[colnames(data)==colname] = "target_word"
  if(dupli_bool==TRUE){
    result_list <- sapply(1:length(dict), function(i){ 
      data %>% 
        filter(grepl(target_word, pattern = dict[i])) %>%
        nrow()
    })
  }else{
    all_text = paste0(data$target_word, collapse = ",")
    result_list <- sapply(1:length(dict), function(i){
      length(stringr::str_match_all(all_text, dict[i])[[1]])
    })
  }
  stat <-  data.frame("keyword" = dict, "freq" = result_list, stringsAsFactors = FALSE)
  if(filter_bool){
    stat = stat %>%
      dplyr::filter(freq>0) %>%
      dplyr::arrange(desc(freq))
  }
  return(stat)
}
