get_plot_data7 <- function(result_emotion, source){
  plot_data <- result_emotion$stat_result
  plot_data <- plot_data[23:29,]
  plot_data$stat_sum <- plot_data$stat_sum/sum(plot_data$stat_sum)
  plot_data <- data.frame(plot_data, "brand" = rep(source, 7))
  return(plot_data)
}
get_plot_data21 <- function(result_emotion, source){
  plot_data <- result_emotion$stat_result
  plot_data <- plot_data[1:21,]
  plot_data$stat_sum <- plot_data$stat_sum/sum(plot_data$stat_sum)
  plot_data <- data.frame(plot_data, "brand" = rep(source, 21))
  return(plot_data)
}

plot_data_meizhiyuan <- get_plot_data7(result_meizhiyuan, "美汁源")
plot_data_yili <- get_plot_data7(result_yili, "伊利")
plot_data_mengniu <- get_plot_data7(result_mengniu, "蒙牛")
plot_data_wahaha <- get_plot_data7(result_wahaha, "哇哈哈")

plot_data <- rbind(plot_data_meizhiyuan, plot_data_yili, plot_data_mengniu, plot_data_wahaha)
rownames(plot_data) <- c(1:nrow(plot_data))

p = ggplot(plot_data, aes(x = type,y = stat_sum, fill = brand)) 
p = p + geom_bar(stat="identity", position = "dodge") + xlab("情感") + ylab("情感占比") + ggtitle("乳饮品对比")
p = p + theme(#plot.background = element_rect(colour = "black", size = 1, linetype = 1, fill = "lightblue"),
              plot.title = element_text(colour = "black", size = 25, vjust = 1),
              plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "inches"),
              text = element_text(family = "STHeiti"))
              # legend.position = "none")
p
p_ly <- ggplotly(p)

plot_data_meizhiyuan <- get_plot_data21(result_meizhiyuan, "美汁源")
plot_data_yili <- get_plot_data21(result_yili, "伊利")
plot_data_mengniu <- get_plot_data21(result_mengniu, "蒙牛")
plot_data_wahaha <- get_plot_data21(result_wahaha, "哇哈哈")

plot_data <- rbind(plot_data_meizhiyuan, plot_data_yili, plot_data_mengniu, plot_data_wahaha)
rownames(plot_data) <- c(1:nrow(plot_data))

q = ggplot(plot_data, aes(x = type,y = stat_sum, fill = brand)) 
q = q + geom_bar(stat="identity", position = "dodge") + xlab("情感") + ylab("情感占比") + ggtitle("乳饮品对比")
q = q + theme(#plot.background = element_rect(colour = "black", size = 1, linetype = 1, fill = "lightblue"),
  plot.title = element_text(colour = "black", size = 25, vjust = 1),
  plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "inches"),
  # legend.position = "none",
  text = element_text(family = "STHeiti"))

q
q_ly <- ggplotly(q)

target_data <- readxl::read_excel("/home/jeffmxh/康师傅私房牛肉面口碑-2015.xlsx")
target_column <- "内容"
text_vec <- target_data[,target_column]

text_vec = gsub(" ", ",",text_vec)
text_vec <- gsub("\\[.+?\\]", "", text_vec)
text_vec <- gsub("\\#.+?\\#", "", text_vec)
text_vec <- gsub("【.+?】", "", text_vec)
text_vec = gsub('https?:[a-zA-Z\\/\\.0-9_]+','',text_vec)
text_vec <- gsub("[A-Za-z0-9]", "", text_vec)
text_vec = gsub('@.+?[,，：:\ )]|@.+?$','',text_vec)
text_vec = gsub('我在(\\w){0,2}[:：](\\w*)','',text_vec)
# text_vec = gsub("\\W+", "",text_vec)

target_data[, target_column] <- text_vec
