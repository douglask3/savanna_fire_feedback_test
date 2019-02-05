lambda_dgvm <- function(c_start1, c_start0, c_1, c_0, BA_1, return_r) {
    
    delta = (c_start1 - c_start0)
    
    test = c_start1 > c_start0
    delta[test] = delta[test] * (1 - c_0[test])/(1 - c_start0[test])
    
    test = !test
    delta[test] = delta[test] * (c_0[test]/c_start0[test])
    
    gamma_burn = BA_1 * c_1
    
    rt = c_1 - c_0 - delta + gamma_burn
    
    if (return_r) return(rt)
    l = rt / (c_start0 - c_0)
    lambda = 1 - log(1 - l)
       
    #num = c_start1 - c_0
    
    #den = c_start1 - c_1 - gamma_burn + delta
    
    #lambda = log(num/den)
    
    return(lambda)
}