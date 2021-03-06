library(magrittr)
library(rstan)
library(tidyverse)
source("./libs/lib_fnames.R")

data <- fname_data("data.rds") %>% 
  read_rds()

estagios <- data %>%
  select(new_playid) %>%
  distinct() %>%
  filter((grepl("JG", new_playid) |
          grepl("JM", new_playid)))
data %<>% inner_join(estagios, by = "new_playid")

fatores <- data %>% 
  select(alias, escolaridade, fator, id, hy) %>%
  group_by(fator) %>%
  summarise_all(funs(first(na.omit(.)))) %>%
  arrange(fator) %>%
  rowid_to_column("fator_id")

data %<>% inner_join(fatores %>% select(fator, fator_id), 
                     by="fator")

#? necess?rio passar algumas informa??es
#explicitamente para o c?digo do stan.
stan_data <- data %>%
  select(acertou_lgl, fator_id, move) %>%
  mutate(acerto = acertou_lgl) %>%
  as.list()
stan_data$e <- fatores$escolaridade %>% max
stan_data$escol <- fatores$escolaridade
stan_data$m <- fatores %>% nrow
stan_data$n = stan_data$acertou %>% length

stan_fit <- stan(file   = fname_analysis("01-JG-stan.stan"), 
                 data   = stan_data, 
                 iter   = 2000, 
                 chains = 1)

#pdf("../plots/amparo-JG-stan-param.pdf")
#hist(stan_fit$summary[grep("gamma\\[[[:alnum:]]{1,},1\\]",rownames(stan_fit$summary)),1],xlab = expression(theta),main="Fase 1",cex.lab=1.5)
#hist(stan_fit$summary[grep("gamma\\[[[:alnum:]]{1,},2\\]",rownames(stan_fit$summary)),1],xlab = expression(theta),main="Fase 2",cex.lab=1.5)
#hist(stan_fit$summary[grep("gamma\\[[[:alnum:]]{1,},3\\]",rownames(stan_fit$summary)),1],xlab = expression(theta),main="Fase 3",cex.lab=1.5)

#hist(stan_fit$summary[grep("alfa\\[[[:alnum:]]{1,},1\\]",rownames(stan_fit$summary)),1],xlab = expression(alpha),main="Fase 1",cex.lab=1.5)
#hist(stan_fit$summary[grep("alfa\\[[[:alnum:]]{1,},2\\]",rownames(stan_fit$summary)),1],xlab = expression(alpha),main="Fase 2",cex.lab=1.5)
#hist(stan_fit$summary[grep("alfa\\[[[:alnum:]]{1,},3\\]",rownames(stan_fit$summary)),1],xlab = expression(alpha),main="Fase 3",cex.lab=1.5)

#hist(stan_fit$summary[grep("beta\\[[[:alnum:]]{1,},1\\]",rownames(stan_fit$summary)),1],xlab = expression(beta),main="Fase 1",cex.lab=1.5)
#hist(stan_fit$summary[grep("beta\\[[[:alnum:]]{1,},2\\]",rownames(stan_fit$summary)),1],xlab = expression(beta),main="Fase 2",cex.lab=1.5)
#hist(stan_fit$summary[grep("beta\\[[[:alnum:]]{1,},3\\]",rownames(stan_fit$summary)),1],xlab = expression(beta),main="Fase 3",cex.lab=1.5)
#dev.off()

alfa <- stan_fit %>% rstan::extract("alfa")
beta <- stan_fit %>% rstan::extract("beta")
gamma <- stan_fit %>% rstan::extract("gamma")
alfa_m <- alfa$alfa %>% colMeans
beta_m <- beta$beta %>% colMeans
gamma_m <- gamma$gamma %>% colMeans

fatores %<>% cbind(beta_m, gamma_m)
fatores_alfa <- fatores %>% 
  select(escolaridade) %>%
  distinct() %>%
  arrange(escolaridade) %>% 
  cbind(alfa_m)
fatores %<>% inner_join(fatores_alfa, by = "escolaridade")

list(estagios = estagios,
     param    = fatores) %>% 
  write_rds(fname_data("stan_data.rds"))
