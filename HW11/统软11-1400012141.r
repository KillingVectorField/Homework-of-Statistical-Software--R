setwd("C:/Users/billy/source/repos/统软11")
reliab <- read.csv("store-reliab-data.csv")

#as.vector(binaryLogic::fillUpToBit(as.binary(2^8-1),8))

Z1 <- reliab[reliab['testid'] == 1, c('times', 'delta')]
#Z1 <- reli ab[reliab['testid'] <= 2, c('times', 'delta')]
theta <- reliab[reliab['testid'] == 1, 'true_theta'][1]

P_theta_Z <- function(P, delta)
  prod(ifelse(delta, P, 1 - P))

if.noworse <- function(Z2, Z1) {
  #判断是否Z2不次于Z1
  if (sum(Z2[, 2]) > sum(Z1[, 2]))
    return (TRUE)
  if (sum(Z2[, 2]) == sum(Z1[, 2])) {
    if (sum(Z2[, 1] * Z2[, 2]) >= sum(Z1[, 1] * Z1[, 2]))
      return (TRUE)
    else
      return (FALSE)
  }
}

G.direct <- function(theta, Z0) {
  N <- nrow(Z0)
  P <- exp(-Z0[, 1] / theta)
  n_0 <- sum(Z0[, 2])
  G.sum <- 0
  # 满足条件1的Zn
  if (n_0 < N) {
    for (n in (n_0 + 1):N) {
      delta.combn <- combn(N, n)
      G.temp <- vapply(1:ncol(delta.combn), function(i) {
        delta1 <- rep(0, N)
        delta1[delta.combn[, i]] <- 1
        return (P_theta_Z(P, delta1))
      }, FUN.VALUE = 1)
      G.sum <- G.sum + sum(G.temp)
    }
  }
  # 满足条件2的Zn
  delta.combn <- combn(N, n_0)
  G.temp <- vapply(1:ncol(delta.combn), function(i) {
    delta1 <- rep(0, N)
    delta1[delta.combn[, i]] <- 1
    if (sum(Z0[, 1] * delta1) >= sum(Z0[, 1] * Z0[, 2]))
      return (P_theta_Z(P, delta1))
    else
      return (0)
  }, FUN.VALUE = 1)
  G.sum <- G.sum + sum(G.temp)
  G.sum
}
G.direct <- Vectorize(G.direct, vectorize.args = "theta")

# 以下是随机模拟求G函数值的方法
G.MC <- function(theta, Z0, N = 1e4) {
  n0 <- sum(Z0[, 2])
  G.MC.single <- function(theta, Z0) {
    # 返回I(Zn(i)>Zn)
    X <- rexp(nrow(Z0), 1 / theta)
    delta <- X > Z0[, 1]
    # 满足条件 1 的Zn
    if (sum(delta) > n0)
      return (TRUE)
    if (sum(delta) == n0) {
      if (sum(delta * Z0[, 1]) >= sum(Z0[, 2] * Z0[, 1]))
        return (TRUE)
    }
    return (FALSE)
  }
  x <- replicate(N,G.MC.single(theta, Z0))
  c(mean = mean(x), sd = sd(x))
}
G.MC <- Vectorize(G.MC, vectorize.args = "theta")

solve.theta <- function(alpha,
                        Z0,
                        G.method = "direct",
                        N = NULL, spline.plot =TRUE,
                        theta.min = 0,
                        theta.max = 50,
                        eps = 1e-4) {
  if (sum(Z0==1)==0) return(0) # 所有n次试验都失效的情形
  #二分法求根
  if (alpha >= 1) {
    print ("Illegal value of alpha!")
    return(FALSE)
  }
  if (G.method=="direct"){
    G<-function(theta) G.direct(theta, Z0)
  }
  else if (G.method=="MC"){
    G<-function(theta) G.MC(theta,Z0,ifelse(is.null(N),1e4,N))[1]
  }
  else {
    print("Illegal G.method!")
    return(FALSE)
  }
  if (G(theta.max) < alpha) {
    print("theta.max is not large enough!")
    return (FALSE)
  }
  while (abs(theta.max - theta.min) > eps) {
    theta.new <- (theta.min + theta.max) / 2
    if (G(theta.new) > alpha)
      theta.max <- theta.new
    else
      theta.min <- theta.new
  }
  if (G.method=="direct") (theta.min + theta.max) / 2
  else if (G.method=="MC"){
    theta.grid<-seq(theta.min-200*eps,theta.max+200*eps,length.out = 100)
    G.grid.MC<-vapply(theta.grid,G,FUN.VALUE = 1)
    require(splines)
    reg.spline.fitted<-lm(G.grid.MC~bs(theta.grid))$fitted.values
    if (spline.plot){
      plot(theta.grid,G.grid.MC)
      lines(theta.grid,reg.spline.fitted,lty="dashed")
    }
    theta.grid[which.min(abs(reg.spline.fitted-alpha))]
  }
  #(theta.min + theta.max) / 2
}


G.direct(10, Z1)
G.MC(10, Z1, 1e4)

theta.grid <- 1:20
G.grid.direct <- G.direct(theta.grid, Z0 = Z1)
plot(theta.grid, G.grid.direct, main = "G(theta)")
G.grid.MC <- G.MC(theta.grid, Z0 = Z1)
points(theta.grid, G.grid.MC[1, ], col = "red", pch = 2)
legend(
  "bottomright",
  c("直接求值法", "随机模拟法"),
  pch = c(1, 2),
  col = c("black", "red")
)


# 直接法求G(theta)
ptm=proc.time()
solve.theta(0.05, Z1)
proc.time()-ptm
# 随机模拟法求G(theta)
ptm=proc.time()
solve.theta(0.05, Z1, G.method = "MC")
proc.time()-ptm

ptm=proc.time()
result <- vapply(1:10, function(testid) {
  Z <- reliab[reliab['testid'] == testid, c('times', 'delta')]
  theta <- reliab[reliab['testid'] == testid, 'true_theta'][1]
  alpha <- reliab[reliab['testid'] == testid, 'alpha'][1]
  c(direct = solve.theta(alpha, Z),
    MC = solve.theta(alpha, Z, G.method = "MC", spline.plot = F))
}, FUN.VALUE = c(0.5, 0.5))
proc.time()-ptm
result