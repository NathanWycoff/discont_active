library(xtable)

load('C.RData')

dats <- c('ssudan' , 'mali' , 'syria' , 'ethiopia' , 'burundi', 'car')
pretty_names <- list(car = 'Central African Republic', ssudan='South Sudan', mali='Mali', syria='Syria', ethiopia='Ethiopia', burundi = 'Burundi')

library(rjson)
bounds <- fromJSON(file='param_ranges.json')
lb <- sapply(bounds, function(x) x$min)

eds <- C
#eds <- Cs

## Make biggest loading positive.
for (dat in dats) {
    edv <- eds[[dat]]$vectors
    for (p in 1:7) {
        eds[[dat]]$vectors[,p] <- sign(edv[which.max(abs(edv[,p])),p]) * edv[,p]
    }
}

# Dress the eigenvectors up into a nice table 
eigv1 <- sapply(dats, function(dat) round(eds[[dat]]$vectors[,1], 2))
rownames(eigv1) <- names(lb)
colnames(eigv1) <- sapply(colnames(eigv1), function(n) pretty_names[[n]])
eigv2 <- sapply(dats, function(dat) round(eds[[dat]]$vectors[,2], 2))
rownames(eigv2) <- names(lb)
colnames(eigv2) <- sapply(colnames(eigv2), function(n) pretty_names[[n]])
eigv3 <- sapply(dats, function(dat) round(eds[[dat]]$vectors[,3], 2))
rownames(eigv3) <- names(lb)
colnames(eigv3) <- sapply(colnames(eigv3), function(n) pretty_names[[n]])

xtable(eigv1)
xtable(eigv2)
