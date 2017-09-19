data {
 int<lower=0> m;                  //numero de combinacoes jogador x fase.
 int<lower=0> n;                  //numero de linhas.
 int<lower=0, upper=1> acerto[n]; //indicadora de acerto.
 int<lower=0> fator[n];           //identificador de jogador x fase.
 vector[n] move;                  //numero da tentativa.
 vector[n] movetime;              //tempo gasto na tentativa.
}

parameters {
 vector[m] alfa;                      //efeito do tempo pensando.
 vector[m] beta;                      //efeito da tentativa.
 vector<lower=0.34,upper=1>[m] gamma; //maior probabilidade.
}

model {
 vector[n] mu;
 alfa ~ normal(0,1);
 beta ~ normal(0,1);
 for(ii in 1:n)
  mu[ii] = gamma[fator[ii]]*inv_logit(-log(3*gamma[fator[ii]]-1)+ movetime[ii]*alfa[fator[ii]]+ move[ii]*beta[fator[ii]]);
 acerto ~ bernoulli(mu);
}
