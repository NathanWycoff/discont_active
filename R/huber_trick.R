## It seems like I might be able to get an analytic solution using a "huberized" Gaussian kernel.
## but will that analysis tell us anything interesting?
library(activegp)
library(hetGP)

f <- function(x) as.numeric(x>0.5)


cs <- c()
#Ns <- 100 * (1:20)
N <- 6
xs <- seq(0,1,length.out = N)
X <- matrix(xs, ncol = 1)
ys <- sapply(xs, f)

eps <- 1/(N-1)
h <- eps/2
ell <- 0.1

k <- function(x1, x2, h, ell) {
    d <- abs(x1-x2)
    if (d>=h) {
        z <- d
    } else {
        z <- d^2/(2*h)-h/2
    }
    return(exp(-z / ell))
}

K <- matrix(NA, nrow=N, ncol=N)
for (n1 in 1:N) {
    for (n2 in n1:N) {
        K[n1,n2] <- K[n2,n1] <- k(X[n1,],X[n2,], h=h, ell = ell)
    }
}

K
K[1,2]^2
K[1,3]

x1 <- 0.5
x2 <- c(xs)
pdf("temp.pdf")
plot(x2, sapply(x2, function(x) k(x1,x, h = h, ell = ell)))
abline(v=x1+h)
abline(v=x1-h)
dev.off()
