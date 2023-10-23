library(activegp)
library(plot3D)

set.seed(123)

covtypes <- c('Matern3_2' ,'Matern5_2' ,'Gaussian')

f <- function(x) {
    nsp <- ifelse(x[1]>0.5,1,0)
    sp <- 6*(x[2] - 0.5)^2
    return(sp + nsp)
}

ng <- 100
N <- ng*ng
P <- 2
X <- matrix(NA, nrow=N, ncol=P)
xgrid <- seq(0,1,length.out=ng)
Y <- matrix(NA,nrow=ng,ncol=ng)
X1 <- matrix(NA,nrow=ng,ncol=ng)
X2 <- matrix(NA,nrow=ng,ncol=ng)
for (n1 in 1:ng) {
    for (n2 in 1:ng) {
        ind <- (n1-1)*ng+n2
        X[ind,1] <- xgrid[n1]
        X1[n1,n2] <- xgrid[n1]
        X[ind,2] <- xgrid[n2]
        X2[n1,n2] <- xgrid[n2]
        Y[n1,n2] <- f(X[ind,])
    }   
}   

## 2d image
pdf("images/mixed_function.pdf", width = 4.5, height = 3)
par(mar = c(2.2,2.2,0.5,0.5))
par(mgp = c(1.2,0.5,0))
image(Y, xlab = 'X1', ylab = 'X2')
dev.off()

pdf("images/mixed_3d.pdf")
par(mar=c(0,0,0,0))
surf3D(X1, X2, Y, colvar = Y, colkey = TRUE, box = TRUE, bty = "b", phi = 20, theta = -45, xlab = 'X1', ylab = 'X2', zlab = 'f(x)')
dev.off()

reps <- 1000

Ns <- seq(10,70,by=10)
nN <- length(Ns)
diags <- array(NA,dim=c(nN,reps,2))
for (ni in 1:nN) {
    N <- Ns[ni]
    print(N)
    for (rep in 1:reps) {
        X <- matrix(runif(N*P),ncol=P)
        y <- apply(X, 1, f)

        fit <- hetGP::mleHomGP(X, y, covtype='Gaussian')
        C_hat <- C_GP(fit, verbose = FALSE)
        C_hat$mat <- C_hat$mat / norm(C_hat$mat)

        diags[ni,rep,] <- diag(as.matrix(C_hat))
    }
}

med_diag <- matrix(NA,nrow=nN,ncol=2)
ub_diag <- matrix(NA,nrow=nN,ncol=2)
lb_diag <- matrix(NA,nrow=nN,ncol=2)
qu <- pmax(0,pmin(1,0.5+1.96*sqrt(0.25/reps)))
ql <- pmax(0,pmin(1,0.5-1.96*sqrt(0.25/reps)))
for (ni in 1:nN) {
    med_diag[ni,1] <- median(diags[ni,,1])
    ub_diag[ni,1] <- quantile(diags[ni,,1], qu)
    lb_diag[ni,1] <- quantile(diags[ni,,1], ql)
    med_diag[ni,2] <- median(diags[ni,,2])
    ub_diag[ni,2] <- quantile(diags[ni,,2], qu)
    lb_diag[ni,2] <- quantile(diags[ni,,2], ql)
}

pdf("images/disc_wins.pdf", width = 4.5, height = 3)
par(mar = c(2.5,1.5,0.1,0.1))
par(mgp=c(1.2,0.5,0))
yrange <- range(c(0.99*c(med_diag), 1.01*c(med_diag)))
plot(NA,NA,xlim=range(Ns),ylim=yrange, xlab = 'Sample Size', ylab = '')
points(Ns, med_diag[,1], col = 'red', type = 'l')
points(Ns, lb_diag[,1], col = 'red', type = 'l', lty = 'dashed')
points(Ns, ub_diag[,1], col = 'red', type = 'l', lty = 'dashed')
points(Ns, med_diag[,2], col = 'blue', type = 'l')
points(Ns, lb_diag[,2], col = 'blue', type = 'l', lty = 'dashed')
points(Ns, ub_diag[,2], col = 'blue', type = 'l', lty = 'dashed')
legend('bottomright',legend=c("X1 Importance","X2 Importance"), fill = c('red','blue'))
dev.off()
