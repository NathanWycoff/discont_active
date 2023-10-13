library(activegp)
library(hetGP)

f <- function(x) as.numeric(x>0.5)

cs <- c()
#Ns <- 100 * (1:20)
Ns <- c(10,20,30,40)
for (N in Ns) {
    xs <- seq(0,1,length.out = N)
    X <- matrix(xs, ncol = 1)
    ys <- sapply(xs, f)

    gp <- mleHomGP(X, ys, known = list(theta=1/10, g0=1e-5))

    ch <- C_GP(gp)
    print(ch)
    cs <- c(cs, c(ch$mat))
}

pdf("temp.pdf")
plot(Ns, cs)
abline(lm(cs~Ns))
dev.off()