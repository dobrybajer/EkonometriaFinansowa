anomalie <- read.csv(file="Data/Lab1/Anomalie.csv", header=TRUE, sep=";", stringsAsFactors = FALSE)
czynniki <- read.csv(file="Data/Lab1/Czynniki.csv", header=TRUE, sep=";", stringsAsFactors = FALSE)

library("stats")
install.packages("sandwich")
library("sandwich")
install.packages("lmtest")
library("lmtest")


# 5 czynnikowy model ------------------------------------------------------

alfa <- rep(0,20)
rows <- nrow(czynniki)
eps <- matrix(0,rows,20)

bptest <- rep(0,20)
dwtest <- rep(0,20)
NeweyWest_t <- rep(0,20)

for (i in 2:21)
{
  model1 <- lm(anomalie[,i]~czynniki[,2]+czynniki[,3]+czynniki[,4]+czynniki[,6]+czynniki[,7])
  alfa[i-1]<- coefficients(model1)[1]
  eps[,i-1]<- model1$residual
  
  bptest[i-1] <- bptest(model1)$p.value
  dwtest[i-1] <- dwtest(model1)$p.value
  NeweyWest_t[i-1] <- alfa[i-1]/(sqrt(NeweyWest(model1)[1,1]))
}

p_value_newey <- 2*pnorm(abs(NeweyWest_t),0,1,lower.tail=FALSE)

bp <- length(which(bptest<0.05))
dw <- length(which(dwtest<0.05))
nt <- length(which(p_value_newey<0.05))

# model Cocharta ----------------------------------------------------------------------

alfa <- rep(0,20)
rows <- nrow(czynniki)
eps <- matrix(0,rows,20)

bptest <- rep(0,20)
dwtest <- rep(0,20)
NeweyWest_t <- rep(0,20)

for (i in 2:21)
{
  model1 <- lm(anomalie[,i]~czynniki[,2]+czynniki[,3]+czynniki[,4]+czynniki[,5])
  alfa[i-1]<- coefficients(model1)[1]
  eps[,i-1]<- model1$residual
  
  bptest[i-1] <- bptest(model1)$p.value
  dwtest[i-1] <- dwtest(model1)$p.value
  NeweyWest_t[i-1] <- alfa[i-1]/(sqrt(NeweyWest(model1)[1,1]))
}

p_value_newey <- 2*pnorm(abs(NeweyWest_t),0,1,lower.tail=FALSE)

bp <- length(which(bptest<0.05))
dw <- length(which(dwtest<0.05))
nt <- length(which(p_value_newey<0.05))

# FF ----------------------------------------------------------------------

alfa <- rep(0,20)
rows <- nrow(czynniki)
eps <- matrix(0,rows,20)

bptest <- rep(0,20)
dwtest <- rep(0,20)
NeweyWest_t <- rep(0,20)

for (i in 2:21)
{
  model1 <- lm(anomalie[,i]~czynniki[,2]+czynniki[,3]+czynniki[,4])
  alfa[i-1]<- coefficients(model1)[1]
  eps[,i-1]<- model1$residual
  
  bptest[i-1] <- bptest(model1)$p.value
  dwtest[i-1] <- dwtest(model1)$p.value
  NeweyWest_t[i-1] <- alfa[i-1]/(sqrt(NeweyWest(model1)[1,1]))
}

p_value_newey <- 2*pnorm(abs(NeweyWest_t),0,1,lower.tail=FALSE)

bp <- length(which(bptest<0.05))
dw <- length(which(dwtest<0.05))
nt <- length(which(p_value_newey<0.05))

# CAPM --------------------------------------------------------------------

alfa <- rep(0,20)
rows <- nrow(czynniki)
eps <- matrix(0,rows,20)

bptest <- rep(0,20)
dwtest <- rep(0,20)
NeweyWest_t <- rep(0,20)

for (i in 2:21)
{
  model1 <- lm(anomalie[,i]~czynniki[,2])
  alfa[i-1]<- coefficients(model1)[1]
  eps[,i-1]<- model1$residual
  
  bptest[i-1] <- bptest(model1)$p.value
  dwtest[i-1] <- dwtest(model1)$p.value
  NeweyWest_t[i-1] <- alfa[i-1]/(sqrt(NeweyWest(model1)[1,1]))
}

p_value_newey <- 2*pnorm(abs(NeweyWest_t),0,1,lower.tail=FALSE)

bp <- length(which(bptest<0.05))
dw <- length(which(dwtest<0.05))
nt <- length(which(p_value_newey<0.05))
