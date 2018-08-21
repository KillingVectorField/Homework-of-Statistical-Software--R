update<-function(t,x1,x2,dt){
  c(t+dt,
          x1+dt*(0.35*(-9.7*sin((t+3)*pi/12)+8.3-x1)+0.46*(x2-x1)+11.1),
          x2+dt*(0.28*(-9.7*sin((t+3)*pi/12)+8.3-x2)+0.46*(x1-x2)))
}
  
t<-0;x1<-4;x2<-4;dt<-1.0/60/60;
record=matrix(c(t,x1,x2),nrow=1)
colnames(record)<-c('t','x1','x2')
i<-0
while(t<=24*7){
  result<-update(t,x1,x2,dt)
  t<-result[1];x1<-result[2];x2<-result[3];
  if(i%%600==0){record<-rbind(record,result)}#每10分钟记录一次数据
  i<-i+1
}

plot(record[,'t'],record[,'x1'],col='red',type='l')
lines(record[,'t'],record[,'x2'],col='blue',lty=2)
legend('topleft',col=c('red','blue'),lty=c(1,2),legend=c('x1','x2'))
title('24小时温度变化情况')

#研究周期
#首先试图用sort函数找到峰值
peaks<-head(sort(record[,'x1'],index.return=T,decreasing = T)$ix,n=7)
peaks
peaks[7]<-which.max(record[1:200,'x1'])
record[c(peaks),]
abline(v=c(record[c(peaks),'t']))
