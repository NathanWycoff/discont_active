
func <- commandArgs(trailingOnly=TRUE)[1]

P <- 2

set.seed(123)
a <- rnorm(P)
a <- a / sqrt(sum(a^2))

for (func in c("quad","gaussian","half_step","ripples")) {
    source("R/toy_common.R")

    ng <- 100
    N <- ng*ng
    P <- 2
    X <- matrix(NA, nrow=N, ncol=P)
    xgrid <- seq(0,1,length.out=ng)
    Y <- matrix(NA,nrow=ng,ncol=ng)
    for (n1 in 1:ng) {
        for (n2 in 1:ng) {
            ind <- (n1-1)*ng+n2
            X[ind,1] <- xgrid[n1]
            X[ind,2] <- xgrid[n2]
            Y[n1,n2] <- f(X[ind,])
        }   
    }   

    pdf(paste("images/",func,"_illust.pdf",sep=''), width = 5, height = 5)
    par(mar = c(2.2,2.2,0.5,0.5))
    par(mgp = c(1.2,0.5,0))
    image(Y, xlab = 'X1', ylab = 'X2')
    dev.off()
}
