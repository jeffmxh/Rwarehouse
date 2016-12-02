# 过滤数据框中全部空白的列-------------------------------------------

filter_na_col <- function(data_frame){
  if(!is.data.frame(data_frame)){
    stop("Input data type error! Please input a data.frame\n")
  }else if(nrow(data_frame)==0){
    stop("Input data error! Empty data.frame found!\n")
  }else{
    column_preserve <- lapply(data_frame, function(col){
      return("FALSE" %in% is.na(col))
    })
    column_preserve <- unlist(column_preserve)
    data_frame <- data_frame[, column_preserve]
    column_dup <- lapply(data_frame, function(col){
      return(nchar(paste0(col, collapse = ""))==0)
    })
    column_dup <- unlist(column_dup)
    data_frame <- data_frame[, !column_dup]
    return(data_frame)
  }
}
