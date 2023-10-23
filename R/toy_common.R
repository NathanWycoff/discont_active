
#func <- 'half_step'
#func <- 'quad'
covtypes <- c('Matern3_2' ,'Matern5_2' ,'Gaussian')
pn_func <- list("quad"="Quadratic", 'gaussian' = 'Squared Exponential', 'half_step'="HalfSpace Step Function", 'ripples' = "'Ripple' Function")

reps <- 30
#reps <- 5
#reps <- 2

#Ps <- c(5, 10, 15)
Ns <- seq(50, 200, by = 50)
Ps <- c(3,5,7)

nP <- length(Ps)
nN <- length(Ns)

if (func=='quad') {
    f <- function(x) sum(a*(x-0.5))^2
#} else if (func=='ackley') {
#    f <- function(x) ackley(sum(a*(x-0.5))/sqrt(P))
} else if (func=='gaussian') {
    f <- function(x) exp(-sum(a*(x-0.5))^2)
} else if (func=='half_step') {
    f <- function(x) sum(a*(x-0.5))>0
} else if (func=='ripples') {
    f <- function(x) {
        #freq <- 5/sqrt(P) # this looked good up to P=5.
        freq <- 5/P
        #freq <- 10/sqrt(P)
        ip <- sin(2*pi*freq*sum(a*(x-0.5)))
        if (ip < 0) {
            return(1)
        } else {
            return(0)
        }   
    }
} else {
    stop("Unknown Function")
}