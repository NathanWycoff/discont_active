#!/usr/bin/Rscript
#  /home/nate/flee_surrogate/flee/theory.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 08.25.2023

library(hetGP)

N <- 10
x <- 1:N / (N+1)

l <- 0.1
#eps <- 1e-6
eps <- 1e-8

y <- 2*as.numeric(x>=0.5)-1
#y <- rep(0,N)

D2 <- as.matrix(dist(x))^2
K <- exp(-D2/l)

phi <- exp(-1/(N+1)^2/l)
phi**4
phi**9

exp(-1/(N+1)^2/l)
exp(-2/(N+1)^2/l)


Ki <- solve(K)

xk <- 0.5
kx <- exp(-(xk-x)^2/l)

kx %*% Ki %*% y

fit <- mleHomGP(as.matrix(x, ncol = 1), y, known = list('theta'=l, g=eps))
fit$Ki
summary(fit)
predict(fit, as.matrix(xk))

xx <- seq(0,1,length.out=1000)

pdf("temp.pdf")
plot(x,y)
points(xx, predict(fit, as.matrix(xx))$mean, type = 'l')
abline(v=0.5)
dev.off()

