
library(caret)
library(activegp)
library(hetGP)
library(Rdimtools)


dim_sizes <- list(ssudan=2,mali=3,syria=1,ethiopia=1,burundi=1,car=1)
set.seed(123)

source("flee/flee_common.R")

n_splits <- 10

trunc_asm <- TRUE

mse_vanil <- list()
mse_warp <- list()
mse_trunc <- list()
mse_sir <- list()

tt <- Sys.time()

N <- 500
cv_splits <- sample(1:n_splits,size=N,replace=TRUE)

for (dat in dats) {
    print(dat)
    mse_vanil[[dat]] <- rep(NA,n_splits)
    mse_warp[[dat]] <- rep(NA,n_splits)
    mse_trunc[[dat]] <- rep(NA,n_splits)
    mse_sir[[dat]] <- rep(NA,n_splits)
    R <- dim_sizes[[dat]]
    for (spl in 1:n_splits) {
        print(spl)
        #load(paste('flee/',dat,'.RData',sep=''))
        Xy <- as.matrix(read.csv(paste('./data/',dat,'_500.csv',sep='')))
        X <- Xy[,1:7]
        y <- Xy[,8]
        X <- X[1:N,]
        y <- y[1:N]

        ## Train-test spl
        #N <- length(y)
        #n_train <- ceiling(N*train_prop)
        #ind_train <- sample(N,n_train)
        ind_test <- which(cv_splits==spl)
        ind_train <- setdiff(1:N, ind_test)

        ## Vanil KNN
        knn_fit <- knnreg(X[ind_train,], y[ind_train])
        pred <- predict(knn_fit, X[-ind_train,])
        mse_vanil[[dat]][spl] <- sum((y[-ind_train] - pred)^2)

        # Est active subspace.
        yscale <- (y-mean(y)) / sd(y)
        gp_fit <- hetGP::mleHomGP(X[ind_train,], yscale[ind_train], covtype='Matern3_2')
        Ch <- C_GP(gp_fit)
        # TODO: debug why explicit = is necessary.
        Lt <- Lt_GP(C=Ch)
        for (tr in c("as","as_trunc","sir")) {
            if (substr(tr,1,2)=='as') {
                Xl <- X %*% Lt
                if (tr=='as_trunc') {
                    Xl <- Xl[,1:R,drop=F]
                }
            } else if (tr=='sir') {
                dr <- do.sir(X, y, ndim = R)
                Xl <- X %*% dr$projection
            } else stop("Unknown transform")

            # predict
            knn_fit <- knnreg(Xl[ind_train,,drop=F], y[ind_train])
            pred <- predict(knn_fit, Xl[-ind_train,])
            mse <- sum((y[-ind_train] - pred)^2)
            if (tr=='as') {
                mse_warp[[dat]][spl] <- mse
            } else if (tr=='as_trunc') {
                mse_trunc[[dat]][spl] <- mse
            } else if (tr=='sir') {
                mse_sir[[dat]][spl] <- mse
            } else stop("Unknown transform")
        }
    }
}

save(mse_vanil, mse_warp, mse_sir, mse_trunc, file = 'flee_bakeoff.RData')

tt1 <- Sys.time()
print(tt1-tt)
