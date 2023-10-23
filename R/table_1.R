library(rjson)
library(xtable)

bounds <- fromJSON(file='param_ranges.json')

names(bounds)
sappbounds[['max_move_speed']]

df <- data.frame(min=sapply(bounds, function(i) i$min), max= sapply(bounds, function(i) i$max), default = sapply(bounds, function(i) i$default))
rownames(df) <- names(bounds)
df
xtable(df,digits=c(1))
