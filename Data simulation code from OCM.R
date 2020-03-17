rm(list = ls())

library(dummies)

options(scipen=999)

size=4205

#----- Gamma -----#
set.seed(45678)

x1<- rbinom (size,5,0.5) ####ordinal
x2<- rbinom (size,5,0.5) ####ordinal
x3 <- rbinom(size, 1,.5)  ###Binary
x4 <- rbinom(size, 1,.5)  ###Binary
x5 <- rnorm(size)  ##Continuous 
x6 <- rnorm(size)  ##Continuous
x7 <- rbinom (size,4,0.5)
z1 <- runif(size, 0, 1)  
z2 <- runif(size, -1, 1) 
x8 <- rnorm(size)    
yy<-exp(11.05+0.05*x1 -0.08*x2 + 0.25*x3 +0.15*x4 -0.02*x5 -0.002*x6+cbind(1, dummy(x7)[, -1]) %*% c(0,-2.3,-1.3,-0.4,-0.1)-0.75*z1^2 + 
          0.15*z2^3 + 0.35*(x8^2/(1+x8^2))+0.2*x3*x5)

y<-rgamma(size,shape=48,rate=48/yy)   

simdatanew <- data.frame(y,x1,x2,x3,x4,x5,x6,x7,x8,z1,z2) 
simdatanew$x7=factor(simdatanew$x7)
cutoff=round(quantile(simdatanew$y, 0.9), digits = 2)

#----- Weibull -----#
set.seed(45678)

x1<- rbinom (size,5,0.5) ####ordinal
x2<- rbinom (size,5,0.5) ####ordinal
x3 <- rbinom(size, 1,.5)  ###Binary
x4 <- rbinom(size, 1,.5)  ###Binary
x5 <- rnorm(size)  ##Continuous 
x6 <- rnorm(size)  ##Continuous
x7 <- rbinom (size,4,0.5)
z1 <- runif(size, 0, 1)  
z2 <- runif(size, -1, 1)  
x8 <- rnorm(size)    
yy=exp(11.05+0.05*x1 -0.08*x2 + 0.25*x3 +0.15*x4 -0.02*x5 -0.002*x6+cbind(1, dummy(x7)[, -1]) %*% c(0,-2.3,-1.3,-0.4,-0.1)-0.75*z1^2 + 
         0.15*z2^3 + 0.35*(x8^2/(1+x8^2))+0.2*x3*x5)
y<-rweibull(size,shape = 8, scale = 1.12*yy)  

simdatanew <- data.frame(y,x1,x2,x3,x4,x5,x6,x7,x8,z1,z2) 
simdatanew$x7=factor(simdatanew$x7)
cutoff=round(quantile(simdatanew$y, 0.9), digits = 2)

#----- Heteroscedastic lognormal (0.5+x1) -----#
set.seed(45678)

x1<- rbinom (size,5,0.5) ####ordinal
x2<- rbinom (size,5,0.5) ####ordinal
x3 <- rbinom(size, 1,.5)  ###Binary
x4 <- rbinom(size, 1,.5)  ###Binary
x5 <- rnorm(size)  ##Continuous 
x6 <- rnorm(size)  ##Continuous
x7 <- rbinom (size,4,0.5)
z1 <- runif(size, 0, 1)  
z2 <- runif(size, -1, 1)  
x8 <- rnorm(size)  
y=exp(11.05+0.05*x1 -0.08*x2 + 0.25*x3 +0.15*x4 -0.02*x5 -0.002*x6+cbind(1, dummy(x7)[, -1]) %*% c(0,-2.3,-1.3,-0.4,-0.1)-0.75*z1^2 + 
        0.15*z2^3 + 0.35*(x8^2/(1+x8^2))+0.2*x3*x5+rnorm(size,0,0.1)*(0.5+x1))

simdatanew <- data.frame(y,x1,x2,x3,x4,x5,x6,x7,x8,z1,z2) 
simdatanew$x7=factor(simdatanew$x7)
cutoff=round(quantile(simdatanew$y, 0.9), digits = 2)

#----- Heavy tail -----#
set.seed(45678)

x1<- rbinom (size,5,0.5) ####ordinal
x2<- rbinom (size,5,0.5) ####ordinal
x3 <- rbinom(size, 1,.5)  ###Binary
x4 <- rbinom(size, 1,.5)  ###Binary
x5 <- rnorm(size)  ##Continuous 
x6 <- rnorm(size)  ##Continuous
x7 <- rbinom (size,4,0.5)
z1 <- runif(size, 0, 1)  
z2 <- runif(size, -1, 1)  
x8 <- rnorm(size)  
y=exp(11.05+0.05*x1 -0.08*x2 + 0.25*x3 +0.15*x4 -0.02*x5 -0.002*x6+cbind(1, dummy(x7)[, -1]) %*% c(0,-2.3,-1.3,-0.4,-0.1)-0.75*z1^2 + 
        0.15*z2^3 + 0.35*(x8^2/(1+x8^2))+0.2*x3*x5+0.9*rnorm(size)+0.1*rnorm(size,0,2))

simdatanew <- data.frame(y,x1,x2,x3,x4,x5,x6,x7,x8,z1,z2) 
simdatanew$x7=factor(simdatanew$x7)
cutoff=round(quantile(simdatanew$y, 0.9), digits = 2)






