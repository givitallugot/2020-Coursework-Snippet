library(knitr)
library(kableExtra)

# a. 

mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
head(mydata) 
# y - admit (합 - 1, 불합 - 0)
# rank 면접 점수 (숫자가 작을수록 잘본 것)

# logit(admit) = gre + gpa + rank

# Method 1: glm() function
mymodel <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial")
summary(mymodel)
coef <- mymodel$coef

# First Obs. [ 0 380 3.61 3 ]
z1 = -3.449548 + 0.002294*380 +0.777014 *3.61 -0.560031 *3
p1 = 1/(1+exp(-z1))
p1 # 모델로 예측한 첫 번째 사람이 합격할 확률

# Third Obs. [ 1 800 4.00 1 ]
z3 = -3.449548 + 0.002294*800 +0.777014 *4.00 -0.560031 *1
p3 = 1/(1+exp(-z3))
p3 # 모델로 예측한 세 번째 사람이 합격할 확률

(pi <- predict(mymodel, type="response")) # All pi hat
(admit.hat <- ifelse(pi > 0.5, 1, 0)) # All y hat

# log-likelihood
logLik(mymodel)

# AIC
-2*logLik(mymodel) + 2*4 # p = 4 = 설명변수 + 1

# Table
se.B <- summary(mymodel)$coef[,2] # se(Beta.hat)
z0.95 <- qnorm(0.975, 0, 1)

df1 <- data.frame(estimate = coef,
                  lwr = c(coef[1]-z0.95*se.B[1], coef[2]-z0.95*se.B[2], coef[3]-z0.95*se.B[3], coef[4]-z0.95*se.B[4]),
                  upr = c(coef[1]+z0.95*se.B[1], coef[2]+z0.95*se.B[2], coef[3]+z0.95*se.B[3], coef[4]+z0.95*se.B[4]))




# Method 2: Optim() function
library(optimx)

logit.mle <- function(par){
  z <- par[1] + par[2]*mydata$gre + par[3]*mydata$gpa + par[4]*mydata$rank
  G <- 1/(1+exp(-z))
  ll <- -sum(log(dbinom(mydata$admit, 1, G))) # 시도, 성공
  return(ll)
}
logit.rslt <- optim(par=c(coef[1], coef[2], coef[3], coef[4]), logit.mle, hessian=TRUE)
logit.2p <- 2*length(logit.rslt$par) # p = number of parameters
logit.ll <- -logit.rslt$value[1]
logit.AIC <- round(-2*logit.ll + logit.2p,4)

V <- solve(logit.rslt$hessian) # Variance-Covariance Matrix
se <- sqrt(diag(V)) # Standard Error
b <- logit.rslt$par # B hat

# Table
df2 <- data.frame(estimate. = c(b[1], b[2], b[3], b[4]),
                  lwr. = c(b[1]-z0.95*se[1], b[2]-z0.95*se[2], b[3]-z0.95*se[3], b[4]-z0.95*se[4]),
                  upr. = c(b[1]+z0.95*se[1], b[2]+z0.95*se[2], b[3]+z0.95*se[3], b[4]+z0.95*se[4]))
df <- cbind(df1, df2)
df <- round(df, 4)
kable(df) %>% kable_styling(full_width=F) %>% add_header_above(c(" " = 1, "Method1: glm()" = 3, "Method2: optim()" = 3)) %>% 
  add_footnote(c("Method1: loglikelihood = -229.7209 / AIC = 467.4418", "Method2: loglielihood = -229.7209 / AIC = 467.4418"), notation="number")


# b. Calculate log-likelihood function in the above problem using tensorflow
# c. Calculate log-likelihood function in the above problem using tensorflow and mini batch
# * b. c. 방법에 대해서는 설명변수 정규화가 필요
# * 정규화: x1 / max(x1), x2 / max(x2), x3 / max(x3)
# d. beta0, beta1, beta2, beta3 값들을 위 세 가지 방법에 대해서 비교
# * 값 & loglikelihood를 비교
# * 그런데, 변환된 변수에 대해서 beta값들 비교 어려움, 방법을 잘 생각해보기.. ?.. AIC? beta값 어떻게 비교?
# e. 