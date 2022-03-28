functions{
  real customProb(real x, real mu, real sigma, real lmu, real lsigma){
    return(log(1/(sigma) * exp(-(0.5) * pow((mu - x)/sigma, 2)) * 1/(1+exp(-lsigma*(x-lmu)))));
//    return(log(1/sigma) - 0.5 * pow((mu - x)/sigma, 2) + log(1) - log(1 + exp(-lsigma*(x-lmu))));
  }
}
data{
int<lower = 0> nl; // number of observations
int<lower = 0> ns; // number of sites
vector[nl] LS; // vector of observed leaf size
int siteIDs[nl];
vector[ns] cMu; // climate mean constraint
real LSmean;
real LSsd;
real climsd;
}

//transformed data{
//vector[n] logit_y; // log recruitment logit_y = log(y);
//}//

parameters{
real lsMu; //mean bio leaf size
real<lower = 0.001> lsSigma; // sd bio leaf size
real<lower = 0.001> climSigma; //sd climate constraint
real<lower = 0.001> climLS0; // limiting amount on ls when at clim constraint.
//real<lower = 0> sigma; // overall error
}



model{
vector[ns] nfact;
real x;
vector[ns] x0;
int ID;

lsMu ~ normal(LSmean, 1);
lsSigma ~ lognormal(LSsd, 1);
climSigma ~ lognormal(climsd, 1);
climLS0 ~ uniform(0, 1);

for (i in 1:ns) {
    nfact[i] = 0.0;
    x0[i] = cMu[i] + log((1.0/climLS0) - 1.0)/ climSigma;
    for (j in 1:101) {
        nfact[i] += exp(customProb((j-51.0)/2.50, lsMu, lsSigma,  x0[i], -climSigma));  
    }
}

for (i in 1:nl) {   
    ID = siteIDs[i];
    target += customProb(LS[i], lsMu, lsSigma,  x0[ID], -climSigma) - log(nfact[ID]);
}
}

