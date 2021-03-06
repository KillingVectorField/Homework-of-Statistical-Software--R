negLog <- function(theta, x) {
    # theta[1]=a, theta[2]=b, theta[3]=log(sigma^2)
    N <- length(x)
    a<-theta[1]
    b<-theta[2]
    s2<-exp(theta[3])
    e_lag <- 0
    x_lag <- 0
    RSS <- 0
    e <- numeric()
    for (x_i in x) {
        e <- x_i - a * x_lag - b * e_lag
        RSS <- RSS + e ^ 2
        e_lag <- e
        x_lag <- x_i
    }
    RSS / (2 * s2) + N / 2 * log(s2)
}

ARMA11 <- function(seed=1, theta, N) {
    set.seed(seed)
    e <- rnorm(N, sd = sqrt(theta[3]))
    x <- e
    for (i in 2:N) {
        x[i] <- theta[1] * x[i - 1] + e[i] + theta[2] * e[i - 1]
    }
    x
}

theta.real<-c(0.5,0.7,1)
theta.MLE<-t(sapply(1:1000, function(seed){
  ARMAData<-ARMA11(seed=seed, theta=theta.real,N=100)
  ores<-optim(c(0,0,0),negLog,x=ARMAData)
  theta.MLE<-ores$par
}))
theta.MLE[,3]<-exp(theta.MLE[,3])
summary(theta.MLE)
cat('真实a=', theta.real[1], 'MLE估计=', mean(theta.MLE[,1]),
    '估计的标准误=', sd(theta.MLE[,1]), '\n')
cat('真实b=', theta.real[2], 'MLE估计=', mean(theta.MLE[,2]),
    '估计的标准误=', sd(theta.MLE[,2]), '\n')
cat('真实s2=', theta.real[3], 'MLE估计=', mean(theta.MLE[,3]),
    '估计的标准误=', sd(theta.MLE[,3]), '\n')
