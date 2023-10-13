source("R/common.R")
load(file=paste(func,'_sav.RData',sep=''))

cols <- list('Gaussian'='red','Matern5_2'='blue','Matern3_2'='green')

#Ns <- as.numeric(colnames(res[[covtypes[1]]]))

for (P in Ps) {
    pdf(paste(func,'_',P,'.pdf',sep=''), width = 5, height = 5)
    all_errs <- c(do.call(rbind, res[[P]]))
    plot(NA,NA,xlim=range(Ns), ylim = range(log10(all_errs)), xlab = 'Sample Size', ylab = 'Log Cosine', main = paste(pn_func[[func]],' dim=',P,sep=''))
    for (covtype in covtypes) {
        med <- apply(log10(res[[P]][[covtype]]), 2, median)
        points(Ns, med, col = cols[[covtype]], type = 'l')
    }
    dev.off()
}

pdf("legend.pdf", width = 3, height = 3)
plot(NA,NA,xlim=c(0,1), ylim = c(0,1), xlab = '', ylab = '', axes = FALSE)
legend('topright',legend=covtypes, fill = sapply(covtypes, function(i) cols[[i]]))
dev.off()