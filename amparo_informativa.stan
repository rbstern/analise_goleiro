data {
  int<lower=0> n;                 //numero de pacientes.
  int<lower=0> t;                  //numero de tratamentos.
  int<lower=0> m;                  //numero de linhas.
  int<lower=0, upper=1> acerto[m]; //indicadora de acerto.
  int<lower=0> alias[m];           //numero do jogador.
  int<lower=0> id[m];              //numero do nivel.
  vector[m] move;                  //numero da tentativa.
}

parameters {
  matrix[n,t] alfa;                    //interceptos
  matrix[n,t] beta;                    //efeito de tempo
  matrix<lower=0, upper=1>[n,t] gamma; //maior probabilidade
}

model {
  for(i in 1:n)
  {
    for(j in 1:t)
    {
        alfa[i,j] ~ normal(0,1);
        beta[i,j] ~ normal(0,1);
    }
  }
  for(i in 1:m)
  {
    acerto[i] ~ bernoulli(gamma[alias[i],id[i]]*inv_logit(alfa[alias[i],id[i]]+beta[alias[i],id[i]]*move[i]));
  }
}
