library(jiebaR)
library(parallel)
library(openxlsx)
library(dplyr)
library(ggplot2)

准备数据--------------------------------------

data_temp <- lapply(1:nrow(data),function(i){data$content[i] = sentence_trim(data$content[i])})
data_temp <- do.call(rbind, data_temp)
data1 = data
data1$content=data_temp
rm(data_temp)

处理数据---------------------------------------

result_new = emotion_analyse(data[1:10,], emotion_dict)
result$raw_data = rbind(result$raw_data, result_new$raw_data)
result$stat_result$stat_sum <- result$stat_result$stat_sum + result_new$stat_result$stat_sum
result_all = result$raw_data
keys = colnames(data_analysed)
data_analysed = data.frame(data1[1:nrow(result_all),], result_all) %>% select(one_of(keys))
data_analysed = emotion_classify(data_analysed)
data_analysed$content = as.vector(data_analysed$content)
data_analysed = emotion_analysed_sign(data_analysed)

write.table(data_analysed, file = "d:\\情感分析结果\\处理后数据.txt", row.names = FALSE, sep = "\t")
# 
# # 绘图分析结果
# 
p = ggplot(result$stat_result, aes(x = type,y = stat_sum, fill = type))
p = p + geom_bar(stat="identity") + xlab("情感") + ylab("加权求和") + ggtitle("情感统计") + theme(legend.position = "none")
ggsave(file = "d:\\情感分析结果\\情感统计.png", plot=p, width = 30, height = 20, units = "cm")

