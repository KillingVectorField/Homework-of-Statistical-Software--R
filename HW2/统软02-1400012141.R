d.class=read.csv('class.csv',header = TRUE)
#(1)
d.class[d.class[,'age']>=15,]
#(2)
d.class[d.class[,'age']>=15 & d.class[,'sex']=='M',c('name','age')]
#(3)
x<-d.class$age