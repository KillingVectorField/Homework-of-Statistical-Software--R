tri_cube<-function(t){
  abs.t=abs(t)
  ifelse(abs.t>1,0,(1-abs.t^3)^3)
}

kernelEstimate<-function(x0,x,y,kernel=tri_cube,
                         h=1.06*sd(x)*length(x)^(-1/5)){
  K<-kernel((x0-x)/h)
  sum(K*y)/sum(K)
}

kernelSmoother<-function(x,y,kernel=tri_cube,
                         n.points=max(100L,length(x)),
                         h=1.06*sd(x)*length(x)^(-1/5),
                         x.range=range(x),
                         x.points=seq(from=x.range[1],to=x.range[2],
                                      length.out = n.points)){
  y.points<-vapply(x.points,
                   function(x0) kernelEstimate(x0,x,y,kernel = kernel,h = h),
                   FUN.VALUE = 0.5)
  return(cbind(x.points,y.points))
}

set.seed(2)
X<-rnorm(20,5,5)
Y<-X^2
plot(X,Y)
points(kernelSmoother(X,Y,h = 2),type="l",col="red")
plot(X,Y)
points(kernelSmoother(X,Y),type="l",col="blue")
plot(X,Y)
points(kernelSmoother(X,Y,h = 7),type="l",col="green")
plot(X,Y)
points(ksmooth(X,Y,kernel = "normal",bandwidth = qnorm(0.75)/0.25),type="l",lty=2,col="red")
points(kernelSmoother(X,Y,kernel = dnorm,h = 1),type="l",col="blue")
# 分析：
# 与 R 的标准函数结果一致
# 当h取得较小，会使得某些区间所有K取值都为0，估计值未定义（NAN）,
# 此外，会出现很多“平台”，这是因为在数据点比较系数的区域，K的support仅为1个点，则在这个邻域内估计值不随x变化。


kernelDensityEstimate<-function(x0,x,kernel=dnorm,
                                h=1.06*sd(x)*length(x)^(-1/5)){
  mean(kernel((x0-x)/h))/h
}

kernelDensity<-function(x,kernel=dnorm, n.points=max(100L,length(x)),
                        h=1.06*sd(x)*length(x)^(-1/5),
                        x.range=range(x),
                        cut=3,
                        x.points=seq(from=x.range[1]-cut*h,to=x.range[2]+cut*h,
                                     length.out = n.points)){
  f.points<-vapply(x.points,
                   function(x0) kernelDensityEstimate(x0,x,kernel = kernel,h = h),
                   FUN.VALUE = 0.5)
  return(cbind(x.points,f.points))
}

hist(X)
plot(density(X,bw=1.06*sd(X)*length(X)^(-1/5),kernel = "gaussian"),col="blue")
points(kernelDensity(X),type='l',col="red",lty=2)

# 验证：与 R 标准函数结果相同
