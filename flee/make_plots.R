library(hetGP)
library(akima)

source("flee_common.R")

dats <- c('ssudan' , 'mali' , 'syria' , 'ethiopia' , 'burundi', 'car')
pretty_names <- list(car = 'Central African Republic', ssudan='South Sudan', mali='Mali', syria='Syria', ethiopia='Ethiopia', burundi = 'Burundi')

plot_asm <- function(X, y, ed, main = '', xlab = '', ylab = '', fname = 'asm.pdf', useranks = TRUE) {
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
                   key.title = title(main = "Error", cex.main = 1), plot.axes = points(Z, bg = cols, pch = 21),
                   xlab = xlab,
                   ylab = ylab, 
                   cex.lab = 1.5, cex.main = 2)
    dev.off()
}


ev1s <- list(
            'ssudan' = 'Conflict Weight',
            'mali' = 'Camp Weight',
            'syria' = 'Camp Weight',
            'ethiopia' =  'Camp Weight',
            'burundi' =  'Camp Weight',
            'car' = 'Conflict Weight'
             )

ev2s <- list(
            'ssudan' = 'Camp Weight',
            'mali' = 'Conflict Move Chance',
            'syria' = 'Conflict Weight',
            'ethiopia' = 'Conflict Weight',
            'burundi' =  'Conflict Weight - Conflict Move Chance',
            'car' = 'Max Move Speed'
             )

load('C.RData')

for (dat in dats) {
    X <- Xs[[dat]]
    y <- ys[[dat]]
    #if (dat %in% c('mali','car')) {
    if (dat %in% c('mali')) {
        print("For visual puproses, decrease max Mali.")
        maxind <- which.max(y)
        yb <- y
        yb[which(y==max(y))] <- -Inf
        y[maxind] <- yb[which.max(yb)]
    }
    plot_asm(X, y, C[[dat]], main = pretty_names[[dat]], xlab = ev1s[[dat]], ylab = ev2s[[dat]], fname = paste(dat, '.pdf', sep = ''), useranks = F)
}
