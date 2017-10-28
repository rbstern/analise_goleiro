library(magrittr)
library(tidyverse)

#Carregar BD
#Em linux data.rds pode ser preferivel.
data <- read.csv("../data/amparo/data.csv")

#Variáveis explicativas
stan.data <- readRDS("../clean_data/amparo-JG-stan-data.rds")
estagios_modelados <- stan.data$estagios_modelados
fator_nobs <- stan.data$fator_nobs
param <- stan.data$param
data2 <- data %>% filter(new_playid %in% estagios_modelados)

aux1 <- param %>% select(alias, id, beta.m) %>% spread(id, beta.m)
colnames(aux1) <- c(colnames(aux1)[1],
                    "beta." %>% paste(colnames(aux1)[-1], sep=""))

aux2 <- param %>% select(alias, id, gamma.m) %>% spread(id, gamma.m)
colnames(aux2) <- c(colnames(aux2)[1],
                    "gamma." %>% paste(colnames(aux2)[-1], sep=""))

tempos <- data2 %>% filter(move != 1) %>%
  select(fator, alias, id, movementtime) %>%
  group_by(fator) %>%
  summarise(alias=mean(alias), id=mean(id), time.m=mean(movementtime)) %>%
  select(alias, id, time.m)
aux3 <- tempos %>% select(alias, id, time.m) %>% spread(id, time.m)
colnames(aux3) <- c(colnames(aux3)[1],
                    "time." %>% paste(colnames(aux3)[-1], sep=""))

expl <- aux1 %>% inner_join(aux2) %>% inner_join(aux3)
expl_has_na <- (expl %>% is.na %>% rowMeans > 0) %>% which
expl <- expl[-expl_has_na,]

#Variaveis resposta
resp <- data2 %>%
  select(alias,HY_,
         best_lim, best_marcha, best_orient, best_reat, best_rest, best_trans, best_tot,
         moca_abs, moca_ate, moca_evoc, moca_lin, moca_nom, moca_ori, moca_vis, moca_tot,
         updrs_post, updrs_rig, updrs_trem, updrs_tot) %>%
  group_by(alias) %>%
  summarise_all(funs(first(na.omit(.)))) %>%
  arrange(alias)
resp <- resp[-expl_has_na,]
resp$HY_[resp$HY_ %>% is.na %>% which] <- 0

#excluir saudaveis
#resp_has_na <- (resp %>% is.na %>% rowMeans > 0) %>% which
#resp <- resp[-resp_has_na,]
#expl <- expl[-resp_has_na,]

resp2 <- resp %>% mutate(moca_tot_scale = moca_tot) %>% 
                  mutate(best_lim= best_lim >= 18,
                         best_marcha= best_marcha >= 17,
                         best_orient= best_orient >= 13,
                         best_reat= best_reat >= 13,
                         best_rest= best_rest >= 13,
                         best_trans= best_trans >= 15,
                         best_tot= best_tot >= 87.5,
                         moca_abs = moca_abs >= 2,
                         moca_ate = moca_ate >= 5,
                         moca_evoc = moca_evoc >= 4,
                         moca_lin = moca_lin >= 2,
                         moca_nom = moca_nom >= 3,
                         moca_ori = moca_ori >= 6,
                         moca_vis = moca_vis >= 4,
                         moca_tot = moca_tot >= 26,
                         updrs_post = updrs_post >= 1,
                         updrs_rig = updrs_rig >= 4,
                         updrs_trem = updrs_trem >= 2,
                         updrs_tot = updrs_tot >= 18)

#BD final
dt.gol <- inner_join(expl,resp2)[,-1]
variaveis.resp <- (resp2 %>% names)[-(1:2)]
variaveis.resp <- variaveis.resp[variaveis.resp!="moca_tot_scale"]
variaveis.expl <- (expl %>% names)[-1]
dt.class <- list(dt.gol=dt.gol,
                 variaveis.expl=variaveis.expl,
                 variaveis.resp=variaveis.resp)
saveRDS(dt.class, "../clean_data/amparo-JG-data-classify.rds")
