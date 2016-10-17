library(stringr)
library(plyr)

make_string_table <- function(tar_string){
  string_list <- str_split(tar_string, "")[[1]]
  string_table <- plyr::count(string_list) %>% filter(freq>1)
  string_table$x <- as.character(string_table$x)
  return(string_table)
}

string_dup <- function(raw_string){
  string_table <- make_string_table(raw_string)
  segments <- string_table$x  
  if(nrow(string_table)==0){
    return(raw_string)
  }else{
    for(letter in segments){
      seg_list <- str_split(raw_string, letter)[[1]]
      num <- length(seg_list)
      if(num<=2) next
      for(i in 1:(num-2)){
        str1 <- paste0(letter, seg_list[i+1])
        str2 <- paste0(letter, seg_list[i+2])
        str2 <- substring(str2, 1, nchar(str1))
        if(str1==str2){
          seg_list <- seg_list[-(i+1)]
          # seg_list[i+1] <- "____"
          raw_string <- paste(seg_list, collapse = letter)
          # raw_string <- sub(pattern = paste0(letter, "____"), "", raw_string)
        }
      }
    }
    result_string <- raw_string
    return(result_string)
  }
}
raw_string = "asdfasdfasd"ad
string_dup(raw_string)

