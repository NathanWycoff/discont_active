library(activegp)
library(hetGP)

source("R/common.R")

reps <- 5

P <- 5
if (func=='quad') {
    f <- function(x) sum(a*(x-0.5))^2
} else if (func=='half_step') {
    f <- function(x) sum(a*(x-0.5))>0
} else {
    stop("Unknown Function")
}
#f <- function(x) log(1+sum(a*(x-0.5))^2)

#Ns <- seq(50, 400, by = 50)
Ns <- seq(50, 200, by = 50)
nN <- length(Ns)

# Build results storage
res <- list()
for (covtype in covtypes) {
    resmat <- matrix(NA,nrow=reps,ncol=nN)
    rownames(resmat) <- 1:reps
    colnames(resmat) <- Ns
    res[[covtype]] <- resmat
}

# Main loop.
for (ni in 1:nN) {
    N <- Ns[ni]


    for (rep in 1:reps) {
        a <- rnorm(P)
        a <- a / sqrt(sum(a^2))

        X <- matrix(runif(N*P), ncol = P)
        y <- apply(X, 1, f)


        for (covtype in covtypes) {
            fit <- hetGP::mleHomGP(X, y, covtype = covtype)
            C_hat <- C_GP(fit)
            a_hat <- eigen(C_hat)$vectors[,1]
            err <- subspace_dist(a, a_hat)
            res[[covtype]][rep,ni] <- err
        }

    }
}

save(res,file=paste(func,'_sav.RData',sep=''))
