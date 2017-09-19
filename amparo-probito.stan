data {
 int<lower=0> m;                  //numero de combinacoes jogador x fase.
 int<lower=0> n;                  //numero de linhas.
 int<lower=0, upper=1> acerto[n]; //indicadora de acerto.
 int<lower=0> fator[n];           //identificador de jogador x fase.
 vector[n] move;                  //numero da tentativa.
}

parameters {
 vector[m] beta;                      //efeito de tempo.
 vector<lower=0.34,upper=1>[m] gamma; //maior probabilidade.
}

model {
 vector[n] mu;
 beta ~ normal(0,1);
 for(ii in 1:n)
  mu[ii] = gamma[fator[ii]]*(Phi(move[ii]*beta[fator[ii]])-0.5)+0.33;
 acerto ~ bernoulli(mu);
}
