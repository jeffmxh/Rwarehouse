require(openxlsx, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(stringr, quietly = TRUE)

# 加载读写excel的模块-------------------------

source("txt_excel_io.R")

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

filter_na_row_5 <- function(data_frame){
  pre_row <- sapply(1:nrow(data_frame), function(i){
    return(length(data_frame[i,is.na(data_frame[i,])])<(0.7*length(data_frame[i,])))
  })
  return(data_frame[pre_row, ])
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

# 用于可视化交叉频次热力图，接受数据为行名列名为term，值为freq的数据框----------------

source("cross_vis.R")

# 用于字典统计--------------------------------------------

source("dict_stat.R")

# 读取数据------------------------------------------

data <- read.xlsx("2016-12-04_21-23-45_surveyAnswerStatistics.xlsx")
data <- filter_na_row_5(data)
colnames(data) <- c("student_id", "name", "course", "teacher", "Part1_A1", "Part1_A2", 
                    "Part1_A3", "Part1_B1", "Part1_B2", "Part1_B3", "Part1_C1", "Part1_C2", 
                    "Part1_D", "Part2_A1", "Part2_A2", "Part2_B1", "Part2_B2", "Part2_C1",
                    "Part2_C2", "Part2_D1", "Part2_D2", "Part2_D3", "Part2_E", "Part3_1", "Part3_2")

data$student_id <- as.character(data$student_id)

# 学号对齐------------------------------------------

data$student_id <- sapply(data$student_id, change_index)
data <- data %>% mutate(year = substring(student_id, 1, 2))
data <- data %>% mutate(department_index = substring(student_id, 4, 5))
data <- data %>% mutate(department = change_department_index(department_index))

# 分离给校友中心的反馈------------------------------

ADC_1 <- data %>% select(Part3_1) %>% filter(nchar(Part3_1)>2) 
colnames(ADC_1) <- "您对目前的校友建设有哪些建议？"
ADC_2 <- data %>% select(Part3_2) %>% filter(!is.na(Part3_2))
contact_dict <- c("微信", "邮件", "微博", "论坛", "小百合", "讲座")
method_count <- find_dict_word(data, "Part3_2", dict = contact_dict, dupli_bool = FALSE, filter_bool = TRUE)
colnames(ADC_2) <- "您希望通过哪些方式与在校生们交流、提供帮助？"
ADC_list <- list()
ADC_list[["对校友建设的建议"]] <- ADC_1
ADC_list[["交流方式"]] <- ADC_2
ADC_list[["方式关键词统计"]] <- method_count
ssave_multi_excel(data_list = ADC_list, file_name = "校友中心反馈.xlsx")
data <- data %>% select(-c(Part3_1, Part3_2))

# 提取括号内容--------------------------------------

kuohao_ying <- filter_na(unlist(str_match_all(data$course, pattern = "\\(.+?\\)")))
kuohao_han <- filter_na(unlist(str_match_all(data$course, pattern = "（.+?）")))
kuohao_book <- filter_na(unlist(str_match_all(data$course, pattern = "《.+?》")))

# 过滤括号内容--------------------------------------

data$course <- gsub("《.+?》", "", data$course)
data$course <- gsub("\\(.+?\\)", "", data$course)
data$course <- gsub("（.+?）", "", data$course)

# 相关性分析----------------------------------------

cor_data_class <- filter_na_row(data[,c(5:12)])
cor_data_teacher <- filter_na_row(data[,c(14:22)])
cor_class <- cor(cor_data_class)
cor_teacher <- cor(cor_data_teacher)
p <- term_matrix_vis(cor_class, "课程热力图")
q <- term_matrix_vis(cor_teacher, "教师热力图")

ggsave(plot = p, filename = "课程相关性图.png", width = 30, height = 20, units = "cm")
ggsave(plot = q, filename = "教师相关性图.png", width = 30, height = 20, units = "cm")

# 对第一部分D进行情感分析---------------------------

source("情感分析.R")
emotion_dict <- emotion_load_dict("情感词汇本体.xlsx")
result_emotion <- emotion_analyse(data, "Part1_D", emotion_dict)
emotion_plot <- emotion_plot_detail(result_emotion)
ggsave(plot = emotion_plot, filename = "情感统计.png", width = 30, height = 20, units = "cm")
ssave_excel(result_emotion$raw_data, file_name = "情感统计详细结果.xlsx")

# 统计课程-------------------------------------------

class <- paste(data$course, collapse = ",")
class <- unlist(strsplit(class, split = ","))
class <- unlist(strsplit(class, split = "；"))
class <- unlist(strsplit(class, split = "、"))
class <- unlist(strsplit(class, split = "/"))
class <- class[class!="NA"]
class_stat <- plyr::count(class) %>% arrange(desc(freq))
ssave_excel(class_stat, "最喜欢的课程.xlsx")
class_content <- data %>% 
  filter(!is.na(Part1_D)) %>% 
  filter(nchar(Part1_D)>5) %>%
  filter(!grepl(Part1_D, pattern = "没.+?变化")) %>%
  arrange(year, department_index) %>%
  select(year, department_index, department, student_id, course, Part1_D)
ssave_excel(class_content, "课程评价.xlsx")


teacher <- paste(data$teacher, collapse = ",")
teacher <- unlist(strsplit(teacher, split = ","))
teacher <- unlist(strsplit(teacher, split = "；"))
teacher <- unlist(strsplit(teacher, split = "、"))
teacher <- unlist(strsplit(teacher, split = "/"))
teacher <- gsub("老师|教授", "", teacher)
teacher <- teacher[nchar(teacher)<5]
teacher <- teacher[teacher!="NA"]
teacher_stat <- plyr::count(teacher) %>% arrange(desc(freq))
ssave_excel(teacher_stat, "最喜欢的老师.xlsx")

teacher_content <- data %>% 
  filter(!is.na(Part2_E)) %>%
  filter(!is.na(teacher)) %>%
  arrange(year, department_index) %>%
  select(year, department_index, department, student_id, teacher, Part2_E)

ssave_excel(teacher_content, "教师评价.xlsx")
data_filter <- data %>%           
  filter(!is.na(teacher)) %>% 
  filter(!is.na(Part2_E)) 

teacher_vec <- paste(data_filter$teacher, collapse = ",")
teacher_vec <- unlist(strsplit(teacher_vec, split = ","))
teacher_vec <- unlist(strsplit(teacher_vec, split = "；"))
teacher_vec <- unlist(strsplit(teacher_vec, split = "、"))
teacher_vec <- unlist(strsplit(teacher_vec, split = "/"))
teacher_vec <- gsub("老师|教授", "", teacher_vec)
teacher_vec <- teacher_vec[nchar(teacher_vec)<5]
teacher_vec <- teacher_vec[teacher_vec!="NA"]

content_list <- lapply(1:length(teacher_vec), function(i){
  return(data_filter %>% 
           filter(grepl(teacher, pattern = teacher_vec[i])) %>% 
           select(Part2_E))
})
names(content_list) <- teacher_vec
count <- lapply(content_list, nrow)
teacher_names <- rep(teacher_vec, count)
teacher_word <- data.frame("教师名" = teacher_names, "话" = unlist(content_list))
ssave_excel(teacher_word, "对老师说的话.xlsx")

# 年级分布--------------------------------------------

p <- ggplot(data[data$year!="07",], aes(x = year, y = ..count.., fill = year)) + 
  geom_bar() + 
  theme(legend.position="none",
        plot.title = element_text(colour = "black", face = "bold", size = 25, vjust = 1),
        text = element_text(family = "STHeiti"),
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "inches")) +
  xlab("年级") + 
  ylab("计数") + 
  ggtitle("年级分布图")
ggsave(plot = p, filename = "年级分布图.png", width = 30, height = 20, units = "cm")

m <- ggplot(data, aes(x = department, y = ..count.., fill = department)) +
  geom_bar() + 
  theme(legend.position="none",
        plot.title = element_text(colour = "black", face = "bold", size = 25, vjust = 1),
        text = element_text(family = "STHeiti"),
        axis.text.x = element_text(angle=45),
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "inches")) +
  xlab("院系") + 
  ylab("计数") + 
  ggtitle("院系分布图")
ggsave(plot = m, filename = "院系分布图.png", width = 30, height = 20, units = "cm")

q <- ggplot(data[data$year!="07",], aes(x = department, y = ..count.., fill = department)) + facet_grid(year~.) +
  geom_bar() + 
  theme(legend.position="none",
        plot.title = element_text(colour = "black", face = "bold", size = 25, vjust = 1),
        text = element_text(family = "STHeiti"),
        axis.text.x = element_text(angle=45),
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "inches")) +
  xlab("院系") + 
  ylab("计数") + 
  ggtitle("院系-年级交叉分布图")
ggsave(plot = q, filename = "院系-年级分布图(去除07).png", width = 30, height = 20, units = "cm")
