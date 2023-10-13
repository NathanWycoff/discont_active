library(rjson)
library(yaml)

#problem <- 'car'
#problem <- 'ssudan'
#problem <- 'mali'
#problem <- 'syria'
#problem <- 'ethiopia'
problem <- 'burundi'

datapaths <- list()
datapaths$car <-  'python runscripts/run.py conflict_input/car conflict_validation/car2013 '
datapaths$ssudan <-  'python runscripts/run.py conflict_input/ssudan conflict_validation/ssudan2014 '
datapaths$mali <-  'python runscripts/run.py conflict_input/mali conflict_validation/mali2012 '
datapaths$syria <-  'python runscripts/run.py conflict_input/syria conflict_validation/syria2013 '
datapaths$ethiopia <-  'python runscripts/run.py conflict_input/ethiopia conflict_validation/ethiopia2020 '
datapaths$burundi <-  'python runscripts/run.py conflict_input/burundi conflict_validation/burundi2015 '

flee_R <- function(x, problem, outind = 1, nlen = 10) {
    # Name some files
    outfile <- paste('out',outind,'.csv',sep='')
    outf <- paste('out',problem,'/', outfile, sep = '')
    #outf <- paste('outcar/', outfile, sep = '')
    setfile <- paste('simsetting',outind,'.yml',sep='')

    # Create parameter file.
    #GOTIL <- 10
    bounds <- fromJSON(file='param_ranges.json')
    lb <- sapply(bounds, function(x) x$min)
    ub <- sapply(bounds, function(x) x$max)
    #lb <- lb[1:GOTIL]
    #ub <- ub[1:GOTIL]
    m <- length(lb)# 7
    xab <- x * (ub-lb) + lb
    prms <- list()
    mvprms <- list()
    for (g in 1:m) {
        nn <- names(lb)[g]
        mvprms[[nn]] <- xab[g]
    }
    prms$move_rules <- mvprms
    prms$optimisations <- list(hasten=5)
    write_yaml(prms, setfile)

    # Create our command
    p1 <- datapaths[[problem]]
    #p2 <- paste(' simsetting.yml > ', outf, sep = '')
    p2 <- paste(' ',setfile,' > ', outf, sep = '')
    cmd <- paste(p1, nlen, p2, sep = '')
    print("R is about to call:")
    print(cmd)
    print("here's the output of that:")
    system(cmd)
    
    # Read and return the error
    df <- read.csv(outf)
    mean_error = mean(df[,'Total.error'])
    return(mean_error)
}

## Randomly sample a bunch, check the active subspace.
library(parallel)

N <- 500
P <- 7

#X <- matrix(NA, nrow = N, ncol = P)
X <- matrix(runif(N*P), nrow = N, ncol = P)
y <- rep(NA, N)

n.cores <- 25
clust <- makeCluster(n.cores)
clusterExport(clust, 'flee_R')
clusterExport(clust, 'problem')
clusterExport(clust, 'X')
clusterExport(clust, 'datapaths')
clusterEvalQ(clust, {library(rjson); library(yaml)})

tt <- Sys.time()
ylist <- parLapply(clust, 1:N, function(i) flee_R(X[i,], problem, outind = i, nlen = 0))
y <- as.numeric(ylist)
Sys.time() - tt 

save(X, y, file = paste(problem,'.RData',sep=''))
