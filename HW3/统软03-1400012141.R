#圆桌夫妇座位问题
is.couple<-function(i,j){
  #(1,2)(3,4)是对的，(2,3)(4,5)是不对的
  abs(i-j)==1&(i+j)%%4==3
}
library(e1071)

couples_around_table<- function(N){#N对夫妇，暴力验证
  permutations=permutations(2*N)
  count=0
  total=nrow(permutations)
  for (i in 1:total){
    P=permutations[i,]
    for (j in 1:(2*N)){
      if (is.couple(P[j],P[j%%(2*N)+1])){
        count=count+1
        break}
    }
  }
  1-count/total
} 

theory<-function(N){
  sum=1
  for (i in 1:N){
    sum<-sum+(-1)^{i}*choose(N,i)*2^{i}*factorial(2*N-i-1)/factorial(2*N-1)
  }
  sum
}

couples_around_table(2)
couples_around_table(3)
couples_around_table(4)
