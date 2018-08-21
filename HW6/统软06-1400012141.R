library(tidyverse)
library(lubridate)
d.patients<-read_csv("patients.csv",locale = locale(encoding = "GBK"),
                     col_types = cols(
                       `出生日期`=col_date(format = "%Y/%m/%d"),
                       `发病日期`=col_date(format = "%Y/%m/%d"),
                       `诊断时间`=col_datetime(format="%Y/%m/%d%.%R")
                     ))
#发病年龄已在第25章练习中给出
d.patients[['发病年龄']]<-as.numeric(
  difftime(d.patients[["发病日期"]], d.patients[["出生日期"]], units='days')/365.25)#发病年龄
head(year(d.patients$发病日期))#发病年
head(month(d.patients$发病日期))#发病月
head(as.character(d.patients$发病日期,format="%Y%m"))#发病年月
#
d.patients[["地址编码"]]=substr(as.character(d.patients$现住地址国标),1,6)
#
d.patients[["发病年月"]]=as.character(d.patients$发病日期,format="%Y%m")
d.pas1<-d.patients %>%
  count(`地址编码`,`发病年月`)
write.csv(d.pas1,"分区分年月统计.csv",row.names=FALSE)
#
d.patients[["发病月"]]=month(d.patients$发病日期)
d.pas2<-as.data.frame.matrix(table(unlist(d.patients[,'地址编码']),
                            unlist(d.patients[,'发病月'])))
write.csv(d.pas2,"分区分月统计.csv")
#
d.pas3<-as.data.frame.matrix(table(unlist(d.patients[,'发病年月']),
                                   unlist(d.patients[,'性别'])))
d.pas3[['总计']]=d.pas3$男+d.pas3$女
write.csv(d.pas3,"分年月分性别统计.csv")
#
d.pas4<-count(d.patients,d.patients$'职业')
names(d.pas4)<-c("职业","发病人数")
d.pas4[["百分比"]]<-round(100*d.pas4$发病人数/sum(d.pas4$发病人数),digits = 1)
write.csv(d.pas4,"职业构成.csv")
#
age_classes<-d.patients$发病年龄 %/% 10
age_classes<-ifelse(age_classes<=7,age_classes,7)
d.patients[["年龄段"]]<-as.factor(paste(age_classes*10, "-", ifelse(age_classes<7,10*(age_classes+1),"以上")))
d.pas5<-as.data.frame.matrix(table(unlist(d.patients[,'年龄段']),
                                   unlist(d.patients[,'性别'])))
names(d.pas5)<-c("男性发病人数","女性发病人数")
d.pas5[['发病率（男）']]<-round(100*d.pas5$男性发病人数/sum(d.pas5$男性发病人数),digits=1)
d.pas5[['发病率（女）']]<-round(100*d.pas5$女性发病人数/sum(d.pas5$女性发病人数),digits=1)
write.csv(d.pas5,"年龄性别分布.csv")