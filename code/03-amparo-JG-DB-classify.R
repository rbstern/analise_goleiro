library(magrittr)
library(tidyverse)

#Carregar BD
#Em linux data.rds pode ser preferivel.
data <- read.csv("../data/amparo/data.csv")

#Variáveis explicativas
stan_info <- readRDS("../data/amparo/amparo-JG-stan-data.rds")
estagios <- stan_info$estagios
data %<>% inner_join(estagios, by = "new_playid")

param <- stan_info$param
aux1 <- param %>% select(alias, id, beta_m) %>% spread(id, beta_m)
colnames(aux1) <- c(colnames(aux1)[1],
                    "beta_" %>% paste(colnames(aux1)[-1], sep=""))

aux2 <- param %>% select(alias, id, gamma_m) %>% spread(id, gamma_m)
colnames(aux2) <- c(colnames(aux2)[1],
                    "gamma_" %>% paste(colnames(aux2)[-1], sep=""))

tempos <- data %>% 
  filter(move != 1) %>%
  select(fator, alias, id, movementtime) %>%
  group_by(fator) %>%
  summarise(alias = mean(alias), id = mean(id), time_m = mean(movementtime, na.rm=TRUE)) %>%
  select(alias, id, time_m)
aux3 <- tempos %>% select(alias, id, time_m) %>% spread(id, time_m)
colnames(aux3) <- c(colnames(aux3)[1],
                    "time_" %>% paste(colnames(aux3)[-1], sep=""))

extras <- data %>%
  select(alias, escolaridade) %>%
  group_by(alias) %>%
  summarise_all(funs(first(na.omit(.))))

expl <- aux1 %>% 
  inner_join(aux2, by = "alias") %>% 
  inner_join(aux3, by = "alias") %>%
  inner_join(extras, by = "alias")
expl_has_na <- (expl %>% is.na() %>% rowMeans() > 0) %>% which
expl <- expl[-expl_has_na,]

#Variaveis resposta
resp <- data %>%
  select(alias, hy,
         best_lim, best_marcha, best_orient, best_reat, best_rest, best_trans, best_tot,
         moca_abs, moca_ate, moca_evoc, moca_lin, moca_nom, moca_ori, moca_vis, moca_tot,
         updrs_post, updrs_rig, updrs_trem, updrs_tot) %>%
  group_by(alias) %>%
  summarise_all(funs(first(na.omit(.)))) %>%
  arrange(alias)
resp <- resp[-expl_has_na,]

#excluir saudaveis
#resp_has_na <- (resp %>% is.na %>% rowMeans > 0) %>% which
#resp <- resp[-resp_has_na,]
#expl <- expl[-resp_has_na,]

resp2 <- resp %>% 
  mutate(moca_tot_scale = moca_tot,
         best_lim           = best_lim >= 18,
         best_marcha        = best_marcha >= 17,
         best_orient        = best_orient >= 13,
         best_reat          = best_reat >= 13,
         best_rest          = best_rest >= 13,
         best_trans         = best_trans >= 15,
         best_tot= best_tot >= 87.5,
         moca_abs           = moca_abs >= 2,
         moca_ate           = moca_ate >= 5,
         moca_evoc          = moca_evoc >= 4,
         moca_lin           = moca_lin >= 2,
         moca_nom           = moca_nom >= 3,
         moca_ori           = moca_ori >= 6,
         moca_vis           = moca_vis >= 4,
         moca_tot           = moca_tot >= 26,
         updrs_post         = updrs_post >= 1,
         updrs_rig          = updrs_rig >= 4,
         updrs_trem         = updrs_trem >= 2,
         updrs_tot          = updrs_tot >= 18)

#BD final
db_gol <- inner_join(expl, resp2, by = "alias")[,-1]
variaveis_resp <- (resp2 %>% names)[-(1:2)]
variaveis_resp <- variaveis_resp[variaveis_resp != "moca_tot_scale"]
variaveis_expl <- (expl %>% names)[-1]
db_class <- list(db_gol         = db_gol,
                 variaveis_expl = variaveis_expl,
                 variaveis_resp = variaveis_resp)
saveRDS(db_class, "../data/amparo/amparo-JG-data-classify.rds")
