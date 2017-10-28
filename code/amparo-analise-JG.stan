data {
 int<lower=0> m;                  //numero de combinacoes jogador x fase.
 int<lower=0> n;                  //numero de linhas.
 int<lower=0> e;                  //numero de niveis de escolaridade.
 int<lower=0, upper=1> acerto[n]; //indicadora de acerto.
 int<lower=0> fator[n];           //identificador de jogador x fase.
 int<lower=0> escol[n];           //identificador de escolaridade.
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
 beta ~ normal(0,1);
 for(ii in 1:n)
  mu[ii] = gamma[fator[ii]]*inv_logit(-log(3*gamma[fator[ii]]-1)+ alfa[escol[ii]] + move[ii]*beta[fator[ii]]);
 acerto ~ bernoulli(mu);
}
