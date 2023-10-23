library(xtable)

load('flee/C.RData')
source("flee/flee_common.R")

varnames <- c("max_move_speed" , "max_walk_speed" , "camp_move_chance" , "conflict_move_chance" , "default_move_chance" , "camp_weight", "conflict_weight")

eds <- C

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

pdf("images/flee_evals.pdf", width = 10, height = 2)
par(mfrow=c(1,6))
par(mar=c(2.2,1.3,1,1))
par(mgp=c(1,0.3,0))
for (dat in dats) {
    ev <- eds[[dat]]$values
    name <- ifelse(dat=='car', 'CAR', pretty_names[[dat]])
    plot(ev/max(ev), xlab = 'Eigenvalue', ylab = '', main = name, pch = 20, cex = 2)
}
dev.off()
