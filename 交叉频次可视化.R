# 用于可视化交叉频次热力图，接受数据为行名列名为term，值为freq的数据框----------------

term_matrix_vis <- function(dict_matrix){
  require(ggplot2, quietly = TRUE)
  dict_matrix_reshape = data.frame("term1" = "a", "term2" = "a", "freq" = 0, stringsAsFactors = FALSE)
  for(i in 1:nrow(dict_matrix)){
    for(j in 1:ncol(dict_matrix)){
      dict_matrix_reshape <- rbind(dict_matrix_reshape, c(rownames(dict_matrix)[i], colnames(dict_matrix)[j], dict_matrix[i,j]))
    }
  }
  dict_matrix_reshape <- dict_matrix_reshape[-1,]
  dict_matrix_reshape$term1 <- as.factor(dict_matrix_reshape$term1)
  dict_matrix_reshape$term2 <- as.factor(dict_matrix_reshape$term2)
  dict_matrix_reshape$freq <- as.numeric(dict_matrix_reshape$freq)
  
  p <- ggplot(dict_matrix_reshape, aes(x = term1, y = term2)) + 
    geom_raster(aes(fill=freq), hjust=0.5, vjust=0.5,interpolate = FALSE)+
    theme(plot.title = element_text(colour = "black", face = "bold", size = 25, vjust = 1),
          plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "inches"),
          axis.ticks = element_blank()) +
    xlab(NULL) + 
    ylab(NULL) + 
    ggtitle('交叉频次')
  return(p)
}