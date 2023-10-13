library(activegp)

#dat <- 'ssudan'
#dat <- 'mali'
#dat <- 'syria'
#dat <- 'ethiopia'
dat <- 'burundi'

pretty_names <- list(car = 'Central African Republic', ssudan='South Sudan', mali='Mali', syria='Syria', ethiopia='Ethiopia', burundi = 'Burundi')

load(paste(dat,'.RData',sep=''))

ys <- (y-mean(y)) / sd(y)
Cs <- eigen(C_GP(X, ys))
C <- eigen(C_GP(X, y))

## Their active subspace is indistringuishable from random.
#Xr <- matrix(runif(N*P), nrow = N, ncol = P)
#yr <- rnorm(N)
#C <- eigen(C_GP(Xr, yr))

# And a GP fit sees no signal:
hetGP::mleHomGP(X, y)

# Project
U <- C$vectors[,1:2]
Z <- X %*% U
Us <- Cs$vectors[,1:2]
Zs <- X %*% Us

##Create a function to generate a continuous color palette
#rbPal <- colorRampPalette(c('red','blue'))
#nc <- 100
#ycol <- rank(y)
#cols <- rbPal(nc)[as.numeric(cut(ycol,breaks = nc))]
#
#pdf("neato.pdf")
#par(mfrow=c(1,2))
#plot(Z, col = cols)
#plot(Zs, col = cols)
#dev.off()

plot_asm <- function(X, y, ed, main = '', fname = 'asm.pdf', useranks = TRUE) {
    Z <- X %*% ed$vectors[,1:2]

    #fitZ <- mleHomGP(Z, y)
    fitZ <- mleHomTP(Z, y)
    smooth_y <- predict(fitZ, Z)$mean

    fld <- interp(x = Z[,1], y = Z[,2], z = smooth_y)

    #pal = colorRampPalette(c("red", "blue"))
    pal = colorRampPalette(c("red", "cadetblue1"))
    #palpoint = colorRampPalette(c("orangered", "cyan"))
    palpoint <- pal
    KK <- 10
    #cols <- pal(KK)[as.numeric(cut(y,breaks = KK))]
    #cols <- pal(KK)[as.numeric(cut(rank(y),breaks = KK))]
    if (useranks) {
        ycol <- rank(y)
    } else {
        print("Not using ranks!")
        ycol <- y
    }
    cols <- palpoint(KK)[as.numeric(cut(ycol,breaks = KK))]

    # Plot results
    #pdf("cholera_unif_asm.pdf", width = 5, height = 5)
    pdf(fname)
    filled.contour(x = fld$x,
                   y = fld$y,
                   z = fld$z,
                   color.palette = pal,
                   main =main,
                   key.title = title(main = "Error", cex.main = 1), plot.axes = points(Z, bg = cols, pch = 21))
    dev.off()
}

library(hetGP)
library(akima)
#plot_asm(X, y, Cs, main = pretty_names[[dat]], fname = paste(dat, '.pdf', sep = ''))
plot_asm(X, y, C, main = pretty_names[[dat]], fname = paste(dat, '.pdf', sep = ''), useranks = F)
