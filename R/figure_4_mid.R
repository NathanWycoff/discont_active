
source("flee/flee_common.R")

load('flee_bakeoff.RData')

#pdf("images/flee_bakeoff.pdf", width = 7, height = 5)
#par(mfrow=c(2,3))
pdf("images/flee_bakeoff.pdf", width = 10, height = 2)
par(mfrow=c(1,6))
par(mar=c(2.5,2,1,0.1))
par(mgp=c(1,0.3,0))
for (dat in dats) {
    pdf <- data.frame(vanil=mse_vanil[[dat]], warp=mse_warp[[dat]], trunc = mse_trunc[[dat]], sir = mse_sir[[dat]])
    print(pdf)
    name <- ifelse(dat=='car', 'CAR', pretty_names[[dat]])
    colnames(pdf) <- c("Identity", "ASM", "ASMt","SIR")
    bp <- boxplot(log10(pdf), main = name, ylab='Log 10 MSE', xaxt = 'n')
    tick <- seq_along(bp$names)
    axis(1, at = tick, labels = FALSE)
    #nn <- sapply(bp$names, function(i) as.expression(substitute(bold(i))))
    nn <- c(expression(bold("Ident")), expression(bold("ASM")), expression(bold("ASMt")), expression(bold("SIR")))
    text(tick, par("usr")[3] - 0.2, nn, srt = 45, xpd = TRUE)
    #text(tick, par("usr")[3] - 0.15, expression(bold(bp$names)), srt = 45, xpd = TRUE)
    #text(tick, par("usr")[3] - 0.3, bp$names, srt = 45, xpd = TRUE)
}
dev.off()