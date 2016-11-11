library(readxl)
library(openxlsx)

sopen_txt <- function(dirpath){
  file_name <- unlist(list.files(dirpath, pattern = '\\.txt'))
  file_path <- paste0(dirpath, "/", file_name)
  file_handler <- lapply(1:length(file_path), function(i) read.table(
    file_path[i], sep="\t", quote = '', check.names = FALSE, 
    allowEscapes = FALSE, flush = TRUE, stringsAsFactors = FALSE))
  names(file_handler) <- gsub(".txt", "", file_name)
  return(file_handler)
}

sopen_excel <- function(dirpath, sheet_index = 1){
  file_name <- list.files(dirpath, pattern = ".xlsx$")
  file_path <- paste0(dirpath, "/",file_name)
  file_handler <- lapply(1:length(file_path), function(i){read_excel(file_path[i], sheet = sheet_index)})
  names(file_handler) <- gsub(".xlsx", "", file_name)
  return(file_handler)
}

ssave_txt <- function(data_frame, file_path){
  data_frame = as.data.frame(data_frame)
  write.table(data_frame, file = file_path, sep = "\t", row.names = FALSE, fileEncoding = "UTF-8")
  cat("file saved successfully!\n")
}

ssave_excel <- function(data_to_save, file_name){
  if(!is.data.frame(data_to_save)){
    cat("输入数据格式错误\n")
  }else{
    wb <- createWorkbook()
    addWorksheet(wb, sheetName = "sheet1")
    writeData(wb, sheet = "sheet1", data_to_save)
    saveWorkbook(wb, file = file_name, overwrite = TRUE)
    cat("Data saved successfully!")
  }
}

ssave_multi_excel <- function(data_list, file_name){
  if(!is.list(data_list)){
    cat("输入数据格式错误\n")
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





