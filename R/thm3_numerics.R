library(hetGP)
library(activegp)

P <- 2

#SIGMA <- matrix(c(1,0.9,0.9,1),ncol=2)
SIGMA1 <- matrix(c(1,0.5,0.5,1),ncol=2)
SIGMA1 <- SIGMA1 / norm(SIGMA1)
#SIGMA2 <- matrix(c(1,-0.5,-0.5,1),ncol=2)
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

N <- 2000
X <- matrix(runif(N*P), ncol = P)
y <- apply(X, 1, f)
fit <- mleHomGP(X, y, covtype = 'Matern3_2')

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

Yhat <- matrix(predict(fit, Xg)$mean, ncol = ng)

pdf("thm3_heatmap.pdf", height = 6, width = 10)
par(mfrow=c(1,2))
image(Y)
image(Yhat)
dev.off()

Chat <- activegp::C_GP(fit)
Cgp <- Chat

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
#M <- 2000
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
#C_anlt <- big_perim * SIGMA1/norm(SIGMA1) + 2 * (smol_perim) * SIGMA2/norm(SIGMA2)

library(xtable)
xtable(C_anlt)

#C_anlt1/norm(C_anlt1)
#C_anlt2/norm(C_anlt2)

#### Semi Analytic Monte Carlo estimator.
#Ma <- 10000
#curve_perims <- c(smol_perim, big_perim, smol_perim)
#curve_probs <- curve_perims / sum(curve_perims)
#curve_SIGMAs <- list(SIGMA2, SIGMA1, SIGMA2)
#curve_chols <- lapply(curve_SIGMAs, chol)
#curve_threshs <- list(thresh2, thresh1, thresh2)
#curve_centers <- list(center1, center2, center3)
##U <- matrix(NA,nrow=Ma,ncol=P)
#PHI <- matrix(NA,nrow=Ma,ncol=P)
#for (m in 1:Ma) {
#    # Sample a curve.
#    # 1 - bot left,
#    # 2 - mid
#    # 3 - top right.
#    curve <- sample(c(1,2,3),1,prob=curve_probs)
#
#    # Sample random direction.
#    u <- rnorm(2)
#    #us <- curve_SIGMAs[[curve]] %*% u
#    #us <- curve_chols[[curve]] %*% u
#    us <- solve(curve_chols[[curve]]) %*% u
#    #us <- solve(curve_SIGMAs[[curve]]) %*% u
#    us <- sqrt(c(curve_threshs[[curve]])) * us / sqrt(sum((curve_chols[[curve]]%*%us)^2))
#    PHI[m,] <- us + curve_centers[[curve]]
#}
#
#pdf("phi.pdf")
#plot(NA,NA,xlim=c(0,1),ylim=c(0,1))
#points(PHI[,1],PHI[,2], col = alpha('black', 0.02))
#dev.off()
#
#library(ggplot2)
#data <- as.data.frame(PHI)
#colnames(data) <- c("x","y")
#gp <- ggplot(data, aes(x=x, y=y) ) +
#  geom_bin2d() +
#  theme_bw()
#ggsave('hist.pdf', gp)

#eigen(Cgp)
ed_an <- eigen(C_anlt)
ed_num <- eigen(C_num) 

ed_an$values / ed_an$values[1]
ed_num$values / ed_num$values[1]

eigen(Cgp)

ed_an$vectors
ed_num$vectors

