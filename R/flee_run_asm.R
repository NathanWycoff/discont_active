library(activegp)

dats <- c('ssudan' , 'mali' , 'syria' , 'ethiopia' , 'burundi', 'car')

pretty_names <- list(car = 'Central African Republic', ssudan='South Sudan', mali='Mali', syria='Syria', ethiopia='Ethiopia', burundi = 'Burundi')

Cs <- list()
C <- list()
Xs <- list()
ys <- list()
N <- 500
for (dat in dats) {
    #load(paste(dat,'.RData',sep=''))
    Xy <- as.matrix(read.csv(paste('./data/',dat,'_500.csv',sep='')))
    X <- Xy[,1:7]
    y <- Xy[,8]
    X <- X[1:N,]
    y <- y[1:N]
    print(y[1:10])

    yscale <- (y-mean(y)) / sd(y)
    Cs[[dat]] <- eigen(C_GP(X, yscale))
    C[[dat]] <- eigen(C_GP(X, y))

    Xs[[dat]] <- X
    ys[[dat]] <- y
}


save(C, Cs, Xs, ys, file = './data/C.RData')