?dnbinom

# size=n and prob=p
dnbinom(2, size = 3, prob=1/3) #r = 3, p = 1/3일 때 2가 나올 확률 

(8*3)/(3^5)

nsim <- 10000
p <- 1/3
n <- rnbinom(nsim, size=3, prob=1/3)

mean(n)
3*(1-p)/p

var(n)
3*(1-p)/p^2

# Reparameterization
rnbinom2 <- function(nsim, u, a){
  rnbinom(nsim, size=1/a, prob=1/(1+a*u))
}

n2 <- rnbinom2(nsim, u=10, a=1/3)
# 평균은 10, 분산은 10 + 10^2*(1/3) 이어야 함
mean(n2)
var(n2)

dnbinom2 <- function(x, u, a){
  dnbinom(x, size=1/a, prob=1/(1+a*u))
}

dnbinom2(5, 10, 1/3)

pnbinom2 <- function(p, u, a){
  pnbinom(p, size=1/a, prob=1/(1+a*u))
}

pnbinom2(0.7, 10, 1/3)

qnbinom2 <- function(q, u, a){
  pnbinom(q, size=1/a, prob=1/(1+a*u))
}

qnbinom2(0.5, 10, 1/3)




# HW
factorial(15)/(factorial(8)*factorial(7))

choose(10,6)*(1/1024) + choose(10,7)*(1/1024) + choose(10,8)*(1/1024) + choose(10,9)*(1/1024) + choose(10,10)*(1/1024)
386/1024

(0.3)^6

20*19*18*17
2^12

(11*3*4)/4096

3/(13*11*10*7)

factorial(4)*factorial(13)*factorial(13)*factorial(13)*factorial(13)/factorial(52)

choose(16,2)/choose(20,2)
1-12/19
16/184
15/184
239+7+555
225+21+526+29
225+29
21+526
239/(239+555)
239/(239+7)
0.8/0.92
1-(0.94*0.9*0.8)
