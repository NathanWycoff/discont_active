library(activegp)

# Using a grid, we can estimate C for exponential kernel, see if it seems to converge.
# use efficient method for K inversion.

f <- function(x) {
    if (x[1] > 0.5) {
        return(1)
    } else {
        return(0)
    }
}

ng <- round(sqrt(2000))
N <- 7
x <- seq(0,1,length.out=N)

ell <- 1.

K <- matrix(NA,nrow=N,ncol=N)
for (n1 in 1:N) {
    for (n2 in 1:N) {
        K[n1,n2] <- exp(-(x[n1]-x[n2])^2 / (2*ell))
    }
}

phi <- exp(-1/(2*ell*(N-1)^2))

K

#f <- c(1/phi^2, -phi)
a <- prod(sapply(1:(N-1), function(n) 1/(1-phi^(2*n))))
if (N==2) {
    #f <- c(1/(1-phi^2), -phi/(1-phi^2))
    #a <- 1/(1-phi^2)
    f <- a*c(1, -phi)
    #b <- c(-phi/(1-phi^2),1/(1-phi^2))
} else if (N==3) {
    #a <- 1/(1-phi^4)*1/(1-phi^2)
    #f <- a * c(1,phi^2-phi,-phi)
    #f <- a * c(phi^2,-phi+phi^3,phi^2)
    f <- a * c(1,-phi^3-phi,phi^2)
} else if (N==4) {
    #a <- prod(sapply(1:(N-1), function(n) 1/(1-phi^(2*n))))
    f <- a * c(1, -phi^5 - phi^3 - phi, phi^2 + phi^4 + phi^6, -phi^3)
} else if (N==5) {
    f <- a * c(1,
     -(phi^7+phi^5+phi^3+phi),
     phi^10+phi^8+phi^6+phi^6 + phi^4 + phi^2,
     - (phi^9 + phi^7 + phi^5 + phi^3),
     phi^4)
} else if (N==6) {
    f <- a * c(1,
    -(phi^9+phi^7+phi^5+phi^3+phi),
    phi^14+phi^12+2*phi^10+2*phi^8+2*phi^6+phi^4+phi^2,
    -(phi^15+phi^13+2*phi^11+2*phi^9+2*phi^7+phi^5+phi^3),
    phi^12+phi^10+phi^8+phi^6+phi^4,
    -phi^5)
} else if (N==7) {
    f <- a*c(1,
    -(phi^11+phi^9+phi^7+phi^5+phi^3+phi),
    phi^18+phi^16+2*phi^14+2*phi^12+3*phi^10+2*phi^8+2*phi^6+phi^4+phi^2,
    -(phi^21+phi^19+2*phi^17+3*phi^15+3*phi^13+3*phi^11+3*phi^9+2*phi^7+phi^5+phi^3),
    phi^20+phi^18+2*phi^16+2*phi^14+3*phi^12+2*phi^10+2*phi^8+phi^6+phi^4,
    -(phi^15+phi^13+phi^11+phi^9+phi^7+phi^5),
    phi^6)
} else {
    stop("NI")
}

#1/((1-phi^2)*(1-phi^4)*(1-phi^6))
t <- sapply((N):1, function(n) phi^(n^2))
sum(f*t)
phi^N

K %*% f
#K %*% b

f
f_tilde <- a*sapply(1:N, function(i) (-phi)^(i-1))

f/a
f_tilde/a

solve(K, c(1,rep(0,N-1)))
f
