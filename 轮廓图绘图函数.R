outline<-function(data,amain,xlabel,ylabel){  #输入数据框，标题，和x,y变量
  data<-as.matrix(data);
  n<-ncol(data);m<-nrow(data);
  i<-1;j<-1;
  plot(data[i,1:n],type="b",pch=19,lwd=2,xaxt="n",col=i,xlab=xlabel,ylab=ylabel,
       main=amain)
  axis(1,at=1:n,labels=colnames(data))
  for(i in 2:m){
    lines(data[i,1:n],type="b",col=i,pch=19,lwd=2);
  }
  legend("topleft",legend=rownames(data),lty=1,lwd=2,pch=rep(19,m),col=c(1:m),
         ncol=1,bty="l",cex=1.2,text.col=c(i:m),inset=0.01)
}