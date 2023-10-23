
source("flee/flee_common.R")
varnames <- c("max_move_speed" , "max_walk_speed" , "camp_move_chance" , "conflict_move_chance" , "default_move_chance" , "camp_weight", "conflict_weight")

for (dat in dats) {
    load(paste('flee/',dat,'.RData',sep=''))
    out <- cbind(X, y)
    colnames(out) <- c(varnames, 'err')
    write.csv(out, paste('./data/',dat,'_500.csv',sep=''), row.names = FALSE)
}
