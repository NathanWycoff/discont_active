## Creates cartoon image showing interpolant in 1D.
N <- 10
Np <- 1000

f <- function(x) as.numeric(x>0.5)

xp <- seq(0,1,length.out=Np)+rnorm(Np,sd=1e-8)

x <- seq(0,1,length.out=N)
y <- f(x)

yp <- rep(NA,Np)
D <- as.matrix(dist(c(x, xp)))[1:N,(N+1):(N+Np)]
neigh <- t(apply(D, 2, function(d) which(rank(d)<=2)))
yp <- sapply(1:nrow(neigh), function(i) {
    x1 <- x[neigh[i,1]]
    x2 <- x[neigh[i,2]]
    w <- abs(xp[i]-x1) / abs(x1-x2)
    w*y[neigh[i,2]] + (1-w) * y[neigh[i,1]]
})

#pdf("images/1d_interp.pdf", width = 5, height = 3)
pdf("images/1d_interp.pdf", width = 5, height = 2.5)
par(mar=c(2,2,0,0)+0.1)
par(mgp=c(0,0.5,0))
plot(xp, yp, type = 'l', col = 'darkgray', lwd = 3, cex.axis = 0.9, xlab = '', ylab = '')
points(x, y)
abline(v=0.5, lty = 'dashed')
dev.off()

## Divergence of estimates.
Ns <- seq(10,1000,by=10)

f <- function(x) as.numeric(x>0.5)

c_hats <- c()
for (N in Ns) {
    x <- seq(0,1,length.out=N)
    y <- f(x)

    rise <- diff(y)
    run <- diff(x)
    slopes <- rise / run
    c_hat <- mean(slopes^2)
    c_hats <- c(c_hats, c_hat)
}

#pdf("images/1d_div.pdf", width = 5, height = 3)
pdf("images/1d_div.pdf", width = 5, height = 2.5)
par(mar=c(2.5,2.5,0,0)+0.1)
par(mgp=c(1.5,0.5,0))
plot(c_hats, ylab = 'Local-Linear Esimate', xlab = 'Sample Size', type = 'l')
dev.off()