library(readxl)
library(openxlsx)

# 用于批量读取一个文件夹下全部txt, 返回一个list--------------

sopen_txt <- function(dirpath){
  file_name <- unlist(list.files(dirpath, pattern = "\\.txt$"))
  if(length(file_name)==0){
    cat("No txt files found!\n")
  }else{
    file_path <- paste0(dirpath, "/", file_name)
    file_handler <- lapply(1:length(file_path), function(i) read.table(
      file_path[i], sep="\t", quote = "", check.names = FALSE, 
      allowEscapes = FALSE, flush = TRUE, stringsAsFactors = FALSE))
    names(file_handler) <- gsub(".txt", "", file_name)
    return(file_handler)
  }
}

# 用于批量读取一个文件夹下全部excel, 返回一个list------------

sopen_excel <- function(dirpath, sheet_index = 1){
  file_name <- list.files(dirpath, pattern = ".xlsx$")
  if(length(file_name)==0){
    cat("No excel files found!\n")
  }else{
    if(!require(readxl, quietly = TRUE)){
      cat("Please install package 'readxl' first!\n")
    }else{
      file_path <- paste0(dirpath, "/",file_name)
      file_handler <- lapply(1:length(file_path), function(i){read_excel(file_path[i], sheet = sheet_index)})
      names(file_handler) <- gsub(".xlsx", "", file_name)
      return(file_handler)
    }
  }
}

# 用于快速保存txt, data.frame -> txt--------------------------

ssave_txt <- function(data_frame, file_path){
  if(nrow(data_frame)==0){
    cat("Input data error, no data found!\n")
  }else{
    data_frame = as.data.frame(data_frame)
    write.table(data_frame, file = file_path, sep = "\t", row.names = FALSE, fileEncoding = "UTF-8")
    cat("file saved successfully!\n")
  }
}

# 用于保存excel, data.frame -> excel-------------------------

ssave_excel <- function(data_to_save, file_name){
  if((!is.data.frame(data_to_save))||(nrow(data_to_save)==0)){
    cat("Input data error!\n")
  }else if(!require(openxlsx, quietly = TRUE)){
    cat("Please install package 'openxlsx' first!\n")
  }else{
    wb <- createWorkbook()
    addWorksheet(wb, sheetName = "sheet1")
    writeData(wb, sheet = "sheet1", data_to_save)
    saveWorkbook(wb, file = file_name, overwrite = TRUE)
    cat("Data saved successfully!")
  }
}

# 用于批量保存excel, list -> excel----------------------------

ssave_multi_excel <- function(data_list, file_name){
  if((!is.list(data_list))||(length(data_list)==0)){
    cat("Input data error!\n")
  }else if(!require(openxlsx, quietly = TRUE)){
    cat("Please install package 'openxlsx' first!\n")
  }else{
    wb <- createWorkbook()
    for(i in 1:length(data_list)){
      addWorksheet(wb, sheetName = names(data_list)[i])
      writeData(wb, sheet = names(data_list[i]), data_list[[i]])
      cat(names(data_list)[i], " saved!\n")
    }
    saveWorkbook(wb, file = file_name, overwrite = TRUE)
    cat("Data saved successfully!")
  }
}





