data {
 int<lower=0> m;                  //numero de combinacoes jogador x fase.
 int<lower=0> n;                  //numero de linhas.
 int<lower=0> e;                  //numero de niveis de escolaridade.
 int<lower=0, upper=1> acerto[n]; //indicadora de acerto.
 int<lower=0> escol[m];           //identificador de escolaridade.
 int<lower=0> fator_id[n];        //identificador de jogador x fase.
 vector[n] move;                  //numero da tentativa.
}

parameters {
 vector[e] alfa;					  //efeito da escolaridade.
 vector[m] beta;                      //efeito da tentativa.
 vector<lower=0.34,upper=1>[m] gamma; //maior probabilidade.
}

model {
 vector[n] mu;
 alfa ~ normal(0,1);
 for(ii in 1:m)
  beta[ii] ~ normal(alfa[escol[ii]], 1);
 for(ii in 1:n)
  mu[ii] = gamma[fator_id[ii]]*inv_logit(-log(3*gamma[fator_id[ii]]-1) + (move[ii]-1)*beta[fator_id[ii]]);
 acerto ~ bernoulli(mu);
}
