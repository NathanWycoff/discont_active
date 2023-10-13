library(activegp)

dats <- c('ssudan' , 'mali' , 'syria' , 'ethiopia' , 'burundi', 'car')

pretty_names <- list(car = 'Central African Republic', ssudan='South Sudan', mali='Mali', syria='Syria', ethiopia='Ethiopia', burundi = 'Burundi')

Cs <- list()
C <- list()
Xs <- list()
ys <- list()
for (dat in dats) {
    load(paste(dat,'.RData',sep=''))
    print(y[1:10])

    yscale <- (y-mean(y)) / sd(y)
    Cs[[dat]] <- eigen(C_GP(X, yscale))
    C[[dat]] <- eigen(C_GP(X, y))

    Xs[[dat]] <- X
    ys[[dat]] <- y
}


save(C, Cs, Xs, ys, file = 'C.RData')
