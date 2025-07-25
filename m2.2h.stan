// hierarchical model with only radom effect of subject (repeated measure)

data {
  int<lower=0> N;  
  int<lower=0> Nsubject;
  int<lower=0> Nitem;
  int<lower=0> subject[N];
  int<lower=0> item[N];
  int<lower=0, upper=1> gender[N];  
  int<lower=0, upper=1> condition[N];
  vector[N] RT_z;
}

parameters {
  real beta_0;  
  real gammaz_0s [Nsubject];
  real gammaz_0i [Nitem];
  real<lower=0> sigma_0s;
  real<lower=0> sigma_0i;
  real<lower=0> sigma_0;
  real beta_gender;
  real beta_condition;
  real beta_gendercondition;

}

model {
  beta_0 ~ normal(0,1);  
  gammaz_0s ~ normal(0,1);
  gammaz_0i ~ normal(0,1);
  sigma_0s ~ gamma(2,0.1);//normal(0,0.01);
  sigma_0i ~ gamma(2,0.1);// normal(0,0.01);  
  sigma_0 ~ gamma(2,0.1);//normal(0,0.1);
  beta_gender ~ normal(0,1);
  beta_condition ~ normal(0,1);
  beta_gendercondition ~ normal(0,1);

  for (i in 1:N){
  RT_z[i] ~ normal(beta_0 + gammaz_0s[subject[i]]*sigma_0s + gammaz_0i[item[i]]*sigma_0i + beta_gender * gender[i] + beta_condition * condition[i] + beta_gendercondition * gender[i] * condition[i], sigma_0);
  }

}

generated quantities {
real delta_genderformasculine;
real delta_genderformasculineplus0;
real delta_genderformasculineminus0;
real delta_genderplusinteraction;

delta_genderformasculine = beta_condition-beta_gendercondition;
delta_genderformasculineplus0 = beta_0+beta_condition-beta_gendercondition;
delta_genderformasculineminus0 = -beta_0+beta_condition-beta_gendercondition;
delta_genderplusinteraction = beta_gender+beta_gendercondition;
}
