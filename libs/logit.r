logit <- function(x, ns = 23714) {
    x = ((ns - 1) * x + 0.5)/ns
    return(log((1/x) -1)* (-1))
}