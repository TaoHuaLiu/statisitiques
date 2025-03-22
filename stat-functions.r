varpop <- function(x) {
    sum((x - mean(x))^2) / length(x)
}

covpop <- function(x, y) {
    sum(
        (x - mean(x)) * (y - mean(y))
        ) / length(x)
}

coeff_a <- function(x,y) {
    covpop(x,y)/varpop(x)
}

coeff_b <- function(x,y) {
    mean(y) - coeff_a(x,y) * mean(x)
}

sdpop <- function(x) {
    sqrt(varpop(x))    
}

eqdroite <- function(x,y) {
    list(a = coeff_a(x,y), b = coeff_b(x,y))
}

preddroite <- function(x, fonction){
    fonction$a * x + fonction$b
}


corpop <- function(x,y) {
    covpop(x,y) / (sdpop(x) * sdpop(y)) 
} 