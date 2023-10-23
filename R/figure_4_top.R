library(hetGP)
library(akima)

source("R/flee_common.R")

plot_asm <- function(X, y, ed, main = '', xlab = '', ylab = '', fname = 'asm.pdf', useranks = TRUE) {
    Z <- X %*% ed$vectors[,1:2]

    fitZ <- mleHomTP(Z, y)
    smooth_y <- predict(fitZ, Z)$mean

    fld <- interp(x = Z[,1], y = Z[,2], z = smooth_y)

    pal = colorRampPalette(c("cadetblue1","red"))
    palpoint <- pal
    KK <- 10
    if (useranks) {
        ycol <- rank(y)
    } else {
        print("Not using ranks!")
        ycol <- y
    }
    cols <- palpoint(KK)[as.numeric(cut(ycol,breaks = KK))]

    # Plot results
    pdf(fname)
    par(mar=c(1.7,2.1,2,2)+0.1) #BLTR
    par(mgp=c(0.4,0.5,0))
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

load('flee/C.RData')

#for (dat in c('ssudan')) {
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
    plot_asm(X, y, C[[dat]], main = pretty_names[[dat]], xlab = ev1s[[dat]], ylab = ev2s[[dat]], fname = paste('images/',dat, '.pdf', sep = ''), useranks = F)
}
