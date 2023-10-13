
func <- 'half_step'
#func <- 'quad'
covtypes <- c('Matern3_2' ,'Matern5_2' ,'Gaussian')
pn_func <- list("quad"="Rank 1 Quadratic", 'half_step'="HalfSpace Step Function")

reps <- 30

#Ps <- c(5, 10, 15)
#Ns <- seq(50, 200, by = 50)
Ps <- c(3,5,7)
Ns <- seq(10, 50, by = 10)

nP <- length(Ps)
nN <- length(Ns)