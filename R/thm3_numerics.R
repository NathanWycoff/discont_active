library(hetGP)
library(activegp)

P <- 2

SIGMA1 <- matrix(c(1,0.5,0.5,1),ncol=2)
SIGMA1 <- SIGMA1 / norm(SIGMA1)
SIGMA2 <- matrix(c(1,-0.2,-0.2,0.5),ncol=2)
SIGMA2 <- SIGMA2 / norm(SIGMA2)
thresh1 <- 0.1
thresh2 <- 0.005

center1 <- c(0.1,0.1)
center2 <- c(0.5,0.5)
center3 <- c(0.9,0.9)

f <- function(x) {
    #xn <- x - 0.5
    xn <- x - center2
    ip <- c(t(xn) %*% SIGMA1 %*% xn)
    mid <- ip < thresh1

    xn <- x - center3
    ip <- c(t(xn) %*% SIGMA2 %*% xn)
    top <- ip < thresh2

    #xn <- x - 0.1
    xn <- x - center1
    ip <- c(t(xn) %*% SIGMA2 %*% xn)
    bot <- ip < thresh2
    return(bot+mid+top)
}

#ng <- 40
ng <- 100
N <- ng*ng
Xg <- matrix(NA, nrow=N, ncol=P)
xgrid <- seq(0,1,length.out=ng)
Y <- matrix(NA,nrow=ng,ncol=ng)
Yhat <- matrix(NA,nrow=ng,ncol=ng)
for (n1 in 1:ng) {
    for (n2 in 1:ng) {
        ind <- (n1-1)*ng+n2
        Xg[ind,1] <- xgrid[n1]
        Xg[ind,2] <- xgrid[n2]
        Y[n1,n2] <- f(Xg[ind,])
    }   
}   

pdf("./images/thm3_heatmap.pdf", height = 6, width = 10)
par(mfrow=c(1,2))
image(Y)
image(Yhat)
dev.off()

### direct extended ASM estimation.
runif_disc <- function(N, r, P) {
    dirs <- matrix(rnorm(N*P),ncol=P)
    dirs <- dirs / sqrt(rowSums(dirs^2))
    lengths <- runif(N, max = r)
    samp <- dirs * lengths
    return(samp)
}

r <- 0.001
M <- 50000
Nm <- 50
C_num <- matrix(0,nrow=P,ncol=P)
for (m in 1:M) {
    print(m)
    xm <- runif(P)
    Z <- runif_disc(Nm, r, P)
    Z <- rbind(Z,-Z) # Antithetical samples.
    Xz <- t(t(Z) + xm)
    yz <- apply(Xz, 1, f)
    beta_hat <- coef(lm(yz ~ Xz))[2:(P+1)]
    C_num <- C_num + 1/M * beta_hat %*% t(beta_hat)
}

### Analytic candidate solution ? 
a1 <- 2 * sqrt(thresh1)
b1 <- eigen(SIGMA1)$values[2]/eigen(SIGMA1)$values[1] * 2 * sqrt(thresh1)
e1 <- sqrt(a1^2-b1^2) / a1

a2 <- 2 * sqrt(thresh2)
b2 <- eigen(SIGMA2)$values[2]/eigen(SIGMA2)$values[1] * 2 * sqrt(thresh2)
e2 <- sqrt(a2^2-b2^2) / a2

big_perim <- integrate(function(x) 4*a1*sqrt(1-e1^2*sin(x)^2), 0, pi/2)$value
smol_perim <- integrate(function(x) 4*a2*sqrt(1-e2^2*sin(x)^2), 0, pi/2)$value

C_anlt <- big_perim * SIGMA1 + 2 * (smol_perim) * SIGMA2

ed_an <- eigen(C_anlt)
ed_num <- eigen(C_num) 

ed_an$values / ed_an$values[1]
ed_num$values / ed_num$values[1]

ed_an$vectors
ed_num$vectors

