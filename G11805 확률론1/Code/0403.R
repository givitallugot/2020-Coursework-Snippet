x <- rgamma(10000, shape=3, scale=5) # 평균 15 분산 75
mean(x)
var(x)

rgamma2 <- function(nsim, mean, c){
  x <- rgamma(nsim, 1/c, mean*c)
}

x <- rgamma2(10000, 10, 3)
mean(x)
var(x)

sqrt(10)
sqrt(5)
200-(sqrt(5)*20)
200+(sqrt(5)*20)
