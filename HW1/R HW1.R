#第七章 下标运算
d.class <- read.csv('class.csv', header=TRUE, stringsAsFactors=FALSE)
name <- d.class[,'name']
age <- d.class[,'age']
#1
age[c(3,5,7)]
#2
age[age>=15]
#3
age[which(name=='Mary')]
age[which(name=='James')]
#4
agel=age[c(-which(name=='Mary'),-which(name=='James'))]
print(agel)
#5
reverse_map<-function(x){
  y=match(seq(length(x)),x)
}
test<-sample.int(8)
print(test);print(reverse_map(test))

#第九章 日期
dates.tab=read.csv("dates.csv",header=TRUE)
date1<-dates.tab[,"出生日期"]
date2<-dates.tab[,"发病日期"]
#1
date1<-as.POSIXct(date1);date2<-as.POSIXct(date2)
#2
year(date1)
#3
print((date2-date1)/dyears(1))
#4
Sys.setlocale('LC_TIME','C')
print(as.character(date2,format='%b%y'))
#5
f<-function(date_str){
  Sys.setlocale('LC_TIME','C')
  tmp<-as.POSIXct(paste('01',date_str,sep = ''),format='%d%b%y')
  y=as.numeric(substring(date_str,4))
  if (y <=20){
    update(tmp,year=2000+y)}
  else{
    update(tmp,year=1900+y)}
}
#6
g<-function(data_POSIXct){
  Sys.setlocale('LC_TIME','C')
  as.character(data_POSIXct,format='%b%y')
}
#7
Age<-function(birth,work){
  floor((work-birth)/dyears(1))
}

#第十章 factors
sex<-as.factor(d.class[,'sex'])
#age<-d.class[,'age']
#1
print(table(sex))
#2
print(tapply(age,sex,max))
#3
library(forcats)
fct_recode(sex,'Female'='F','Male'='M')
