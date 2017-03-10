anomalie <- read.csv(file="../Data/Lab1/Anomalie.csv", header=TRUE, sep=";", stringsAsFactors = FALSE)
czynniki <- read.csv(file="../Data/Lab1/Czynniki.csv", header=TRUE, sep=";", stringsAsFactors = FALSE)

# CAPM --------------------------------------------------------------------

alfa <- rep(0,20)
rows <- nrow(czynniki)
eps <- matrix(0,rows,20)

for (i in 2:21)
{
  model1 <- lm(anomalie[,i]~czynniki[,2])
  alfa[i-1]<- coefficients(model1)[1]
  eps[,i-1]<- model1$residual
}

sigma <- cov(eps)
mean <- mean(czynniki[,2])
cov <- var(czynniki[,2])

T <- 216
M <- 20
K <- 1
scale <- T/M*(T-M-K)/(T-K-1)

GRS <- scale *(t(alfa)%*%solve(sigma)%*%alfa)/(1+t(mean)%*%solve(cov)%*%mean)
critical_value <- pf(GRS,M,T-M-K,lower.tail = FALSE)

# FF ----------------------------------------------------------------------

alfa <- rep(0,20)
rows <- nrow(czynniki)
eps <- matrix(0,rows,20)

for (i in 2:21)
{
  model1 <- lm(anomalie[,i]~czynniki[,2]+czynniki[,3]+czynniki[,4])
  alfa[i-1]<- coefficients(model1)[1]
  eps[,i-1]<- model1$residual
}

sigma <- cov(eps)
mean <- sapply(czynniki[,2:4],mean)
cov <- sapply(czynniki[,2:4],var)

T <- 216
M <- 20
K <- 3
scale <- T/M*(T-M-K)/(T-K-1)

GRS <- scale *(t(alfa)%*%solve(sigma)%*%alfa)/(1+mean%*%solve(cov)%*%mean)
critical_value <- pf(GRS,M,T-M-K,lower.tail = FALSE)

# model Cocharta ----------------------------------------------------------

alfa <- rep(0,20)
rows <- nrow(czynniki)
eps <- matrix(0,rows,20)

for (i in 2:22)
{
  model1 <- lm(anomalie[,i]~czynniki[,2]+czynniki[,3]+czynniki[,4]+czynniki[,5])
  alfa[i-1]<- coefficients(model1)[1]
  eps[,i-1]<- model1$residual
}

sigma <- cov(eps)
mean <- sapply(czynniki[,2:5],mean)
cov <- sapply(czynniki[,2:5],var)

T <- 216
M <- 20
K <- 4
scale <- T/M*(T-M-K)/(T-K-1)

GRS <- scale *(t(alfa)%*%solve(sigma)%*%alfa)/(1+mean%*%solve(cov)%*%mean)
critical_value <- pf(GRS,M,T-M-K,lower.tail = FALSE)

# 5 czynnikowy model  -----------------------------------------------------

alfa <- rep(0,20)
rows <- nrow(czynniki)
eps <- matrix(0,rows,20)

for (i in 2:22)
{
  model1 <- lm(anomalie[,i]~czynniki[,2]+czynniki[,3]+czynniki[,4]+czynniki[,6]+czynniki[,7])
  alfa[i-1]<- coefficients(model1)[1]
  eps[,i-1]<- model1$residual
}

sigma <- cov(eps)
mean <- sapply(czynniki[1,c(2,3,4,6,7)],mean)
cov <- sapply(czynniki[1,c(2,3,4,6,7)],var)

T <- 216
M <- 20
K <- 5
scale <- T/M*(T-M-K)/(T-K-1)

GRS <- scale *(t(alfa)%*%solve(sigma)%*%alfa)/(1+mean%*%solve(cov)%*%mean)
critical_value <- pf(GRS,M,T-M-K,lower.tail = FALSE)

# https://faculty.chicagobooth.edu/john.cochrane/teaching/35150_advanced_investments/Cochrane_asset_pricing_CH12_229-250.pdf (str 5)
