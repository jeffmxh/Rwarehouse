require(openxlsx, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(stringr, quietly = TRUE)

# 过滤NA--------------------------------------------

filter_na <- function(str_vec){
  return(str_vec[!is.na(str_vec)])
}

filter_na_row <- function(matrix){
  na_vec <- sapply(1:nrow(matrix), function(i){
    return(TRUE %in% unique(is.na(matrix[i,])))
  })
  return(matrix[!na_vec,])
}

# 学号格式------------------------------------------

change_index <- function(index){
  if(nchar(index)==8){
    index <- paste0("0", index)
  }
  return(index)
}

change_department_index <- function(index){
  vec <- c(1:28)
  names(vec) <- c("文学院", "历史学系", "法学院", "哲学系", "新闻传播学院", "政府管理学院", 
                  "信息管理系", "社会学院", "商学院", "外国语学院", "数学系", "物理学院",
                  "化学化工学院", "生命科学学院", "地球科学与工程学院", "地理与海洋科学学院",
                  "电子科学与工程学院", "大气科学学院", "现代科学与应用工程学院", "环境学院",
                  "天文与空间科学学院", "计算机科学与技术系", "医学院", "匡亚明学院", "软件学院",
                  "工程管理学院", "海外教育学院", "建筑与城市规划学院")
  index <- as.numeric(index)
  return(names(vec[index]))
}

# 读取数据------------------------------------------

data <- read.xlsx("surveyAnswerStatistics.xls.xlsx")
data$学号 <- as.character(data$学号)

# 学号对齐------------------------------------------

data$学号 <- sapply(data$学号, change_index)
data <- data %>% mutate(year = substring(学号, 1, 2))
data <- data %>% mutate(department_index = substring(学号, 4, 5))
data <- data %>% mutate(department = change_department_index(department_index))

# 提取括号内容--------------------------------------

kuohao_ying <- filter_na(unlist(str_match_all(data$课程, pattern = "\\(.+?\\)")))
kuohao_han <- filter_na(unlist(str_match_all(data$课程, pattern = "（.+?）")))
kuohao_book <- filter_na(unlist(str_match_all(data$课程, pattern = "《.+?》")))

# 过滤括号内容--------------------------------------

data$课程 <- gsub("《.+?》", "", data$课程)
data$课程 <- gsub("\\(.+?\\)", "", data$课程)
data$课程 <- gsub("（.+?）", "", data$课程)

# 相关性分析----------------------------------------

cov_data_class <- filter_na_row(data[,c(5:12)])
cov_data_teacher <- filter_na_row(data[,c(14:22)])
cov_class <- cov(cov_data_class)
cov_teacher <- cov(cov_data_teacher)
p <- term_matrix_vis(cov_class, "课程")
q <- term_matrix_vis(cov_teacher, "教师")

ggsave(plot = p, filename = "课程相关性图.png", width = 30, height = 20, units = "cm")
ggsave(plot = q, filename = "教师相关性图.png", width = 30, height = 20, units = "cm")

result_emotion <- emotion_analyse(data, "第一部分D", emotion_dict)
emotion_plot <- emotion_plot_detail(result_emotion)
ggsave(plot = emotion_plot, filename = "情感统计.png", width = 30, height = 20, units = "cm")

# 统计课程-------------------------------------------

class <- paste(data$课程, collapse = ",")
class <- unlist(strsplit(class, split = ","))
class <- unlist(strsplit(class, split = "；"))
class <- unlist(strsplit(class, split = "、"))
class <- unlist(strsplit(class, split = "/"))
class_stat <- plyr::count(class) %>% arrange(desc(freq)) %>% filter(x!="NA")

teacher <- paste(data$老师, collapse = ",")
teacher <- unlist(strsplit(teacher, split = ","))
teacher <- unlist(strsplit(teacher, split = "；"))
teacher <- unlist(strsplit(teacher, split = "、"))
teacher <- unlist(strsplit(teacher, split = "/"))
teacher_stat <- plyr::count(teacher) %>% arrange(desc(freq)) %>% filter(x!="NA")
