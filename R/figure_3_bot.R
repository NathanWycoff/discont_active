func <- commandArgs(trailingOnly=TRUE)[1]

source("R/toy_common.R")
load(file=paste(func,'_sav.RData',sep=''))


cols <- list('Gaussian'='red','Matern5_2'='blue','Matern3_2'='green')

for (P in Ps) {
    pdf(paste('images/',func,'_',P,'.pdf',sep=''), width = 5, height = 5)
    par(mar=c(2.8,2.7,2,1)+0.1) #BLTR
    par(mgp=c(1.6,0.5,0))
    all_errs <- c(do.call(rbind, res[[P]]))
    plot(NA,NA,xlim=range(Ns), ylim = range(log10(all_errs)), xlab = 'Sample Size', ylab = 'Log Cosine', main = pn_func[[func]], cex.main = 2, cex.lab = 1.5)
    for (covtype in covtypes) {
        med <- apply(log10(res[[P]][[covtype]]), 2, median)
        lb <- apply(log10(res[[P]][[covtype]]), 2, function(x) quantile(x,0.25))
        ub <- apply(log10(res[[P]][[covtype]]), 2, function(x) quantile(x,0.75))
        points(Ns, med, col = cols[[covtype]], type = 'l')
        points(Ns, lb, col = cols[[covtype]], type = 'l', lty='dashed')
        points(Ns, ub, col = cols[[covtype]], type = 'l', lty='dashed')
    }
    if (func=='quad') legend('bottomleft',legend=covtypes, fill = sapply(covtypes, function(i) cols[[i]]))
    dev.off()
}