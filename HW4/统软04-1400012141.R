h=c(0.482962913145,0.836516303738,0.224143868042, -0.129409522551)
N<-2
n<-8#最小刻度为2^(-n)
depth<-20#迭代次数

lattice<- seq(from=-0.5,to=2*N-1,by=1/2^n)

indicator<-function(x) ifelse(abs(x) <= 0.5, 1, 0)#[-0.5,0.5]上的示性函数

x2i<-function(x) round((x+0.5)*(2^n))+1#从x逆推下标i

eta.iter<-function(eta.old){
  eta.new<-numeric(length(eta.old))
  for (k in 0:(2*N-1)){
    tmp<-numeric(length(eta.old))
    for (i in x2i((k-0.5)/2):x2i((k+2*N-1)/2)){
      x<-lattice[i]
      tmp[i]<-eta.old[x2i(2*x-k)]
    }
    eta.new<-eta.new+tmp*h[k+1]
  }
  return(sqrt(2)*eta.new)
}

eta<-indicator(lattice)
for (i in 1:depth){eta<-eta.iter(eta)}

plot(lattice,eta)