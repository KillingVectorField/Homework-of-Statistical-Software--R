f0 <- function(x, y) {
    exp(-45 * (x + 0.4) ^ 2 - 60 * (y - 0.5) ^ 2)+0.5*exp(-90*(x-0.5)^2-45*(y+0.1)^4)
}

MeanMC <- function(f=f0,N = 1e5, seed = 1) {
    set.seed(seed)
    x <- runif(N, -1, 1)
    y <- runif(N, -1, 1)
    4 * mean(f(x, y))
}

g0 <- function(x, y, p) {
    p * dnorm(x, -0.4, sqrt(1 / 90)) * dnorm(y, 0.5, sqrt(1 / 120))+(1 - p) * dnorm(x, 0.5, sqrt(1 / 180)) * dnorm(y, -0.1, sqrt(1 / 20))
}

WeightedMC <- function(f = f0, g = g0, N = 1e5, seed = 1) {
    set.seed(seed)
    p <- 0.5358984
    K <- rbinom(N, size = 1, prob = p)
    x <- ifelse(K, rnorm(N, -0.4, sqrt(1 / 90)), rnorm(N, 0.5, sqrt(1 / 120)))
    y <- ifelse(K, rnorm(N, 0.5, sqrt(1 / 180)), rnorm(N, -0.1, sqrt(1 / 20)))
    mean(f(x,y)/g(x,y,p))
}

B <- 100
start<-proc.time()
MeanMC.record <- vapply(1:B, function(k) MeanMC(seed = k), FUN.VALUE = 1)
MeanMC.time <- proc.time()
MeanMC.time-start
mean(MeanMC.record)
sd(MeanMC.record)

WeightedMC.record <- vapply(1:B, function(k) WeightedMC(seed = k), FUN.VALUE = 1)
WeightedMC.time <- proc.time()
WeightedMC.time-MeanMC.time
mean(WeightedMC.record)
sd(WeightedMC.record)