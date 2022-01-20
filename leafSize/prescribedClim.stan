functions{
  real customProb(real x, real mu, real sigma, real lmu, real lsigma){
    return(log(1/(sigma) * exp(-(0.5) * pow((mu - x)/sigma, 2)) * 1/(1+exp(-lsigma*(x-lmu)))));
//    return(log(1/sigma) - 0.5 * pow((mu - x)/sigma, 2) + log(1) - log(1 + exp(-lsigma*(x-lmu))));
  }
}
data{
int<lower = 0> n; // number of observations
vector[n] LS; // vector of observed leaf size
real cMu; // climate mean constraint
}

//transformed data{
//vector[n] logit_y; // log recruitment logit_y = log(y);
//}//

parameters{
real lsMu; //mean bio leaf size
real<lower = 0> lsSigma; // sd bio leaf size
real<lower = 0> climSigma; //sd climate constraint
//real<lower = 0> sigma; // overall error
}



model{
real nfact;
real x;

nfact = 0.0;
for (i in 1:1000) 
    nfact += customProb((i-500.0)/25.0, lsMu, lsSigma,  cMu, -climSigma);    

for (i in 1:n)
  target += customProb(LS[i], lsMu, lsSigma,  cMu, -climSigma) - nfact;
}

