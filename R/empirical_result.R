library(activegp)

# Using a grid, we can estimate C for exponential kernel, see if it seems to converge.
# use efficient method for K inversion.

f <- function(x) {
    if (x[1] > 0.5) {
        return(1)
    } else {
        return(0)
    }
}

#ng <- 10
ng <- round(sqrt(2000))
N <- ng*ng
P <- 2
X <- matrix(rnorm(N*P), ncol=P)
xgrid <- seq(0,1,length.out=ng)
for (n1 in 1:ng) {
    for (n2 in 1:ng) {
        ind <- (n1-1)*ng+n2
        X[ind,1] <- xgrid[n1]
        X[ind,2] <- xgrid[n2]
    }
}

y <- apply(X,1,f)


eigen(C_GP(X, y))
