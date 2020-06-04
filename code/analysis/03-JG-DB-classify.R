library(magrittr)
library(tidyverse)
source("./libs/lib_fnames.R")

data <- fname_data("data.rds") %>% 
  read_rds()

#Vari?veis explicativas
stan_info <- fname_data("stan_data.rds") %>% 
  read_rds()
estagios <- stan_info$estagios
data %<>% inner_join(estagios, by = "new_playid")

param <- stan_info$param
aux1 <- param %>% 
  select(alias, id, beta_m) %>% 
  spread(id, beta_m)
colnames(aux1) <- c(colnames(aux1)[1],
                    "beta_" %>% paste(colnames(aux1)[-1], sep = ""))

aux2 <- param %>% 
  select(alias, id, gamma_m) %>% 
  spread(id, gamma_m)
colnames(aux2) <- c(colnames(aux2)[1],
                    "gamma_" %>% paste(colnames(aux2)[-1], sep=""))

tempos <- data %>% 
  filter(move != 1) %>%
  select(fator, alias, id, movement_time) %>%
  group_by(fator) %>%
  summarise(alias = mean(alias), 
            id = mean(id), 
            time_m = mean(movement_time, na.rm=TRUE)) %>%
  select(alias, id, time_m)
aux3 <- tempos %>% 
  select(alias, id, time_m) %>% 
  spread(id, time_m)
colnames(aux3) <- c(colnames(aux3)[1],
                    "time_" %>% paste(colnames(aux3)[-1], sep = ""))

extras <- data %>%
  select(alias, escolaridade) %>%
  group_by(alias) %>%
  summarise_all(funs(first(na.omit(.))))

expl <- aux1 %>% 
  inner_join(aux2, by = "alias") %>% 
  inner_join(aux3, by = "alias") %>%
  inner_join(extras, by = "alias") %>% 
  as_tibble()
expl_has_na <- (expl %>% is.na() %>% rowMeans() > 0) %>% which()
expl <- expl[-expl_has_na,]

#Variaveis resposta
resp <- data %>%
  select(alias, hy,
         best_lim, best_marcha, best_orient, 
         best_reat, best_rest, best_trans, best_tot,
         moca_abs, moca_ate, moca_evoc, 
         moca_lin, moca_nom, moca_ori, 
         moca_vis, moca_tot,
         dgi, tug_custo) %>%
  group_by(alias) %>%
  summarise_all(funs(first(na.omit(.)))) %>%
  arrange(alias)
resp <- resp[-expl_has_na,]

#excluir saudaveis
#resp_has_na <- (resp %>% is.na %>% rowMeans > 0) %>% which
#resp <- resp[-resp_has_na,]
#expl <- expl[-resp_has_na,]

balanced_div = function(x)
{
  a = x >= median(x)
  b = x >= median(x) + 1
  if(abs(mean(a)-0.5) <= abs(mean(b)-0.5))
    return(a)
  return(b)
}

resp2 <- resp %>% 
  mutate(
    hy_1               = hy >= 1,
    hy_2               = hy >= 2,
    hy_3               = hy >= 3,
    best_lim           = balanced_div(best_lim),
    best_marcha        = balanced_div(best_marcha),
    best_orient        = balanced_div(best_orient),
    best_reat          = balanced_div(best_reat),
    best_rest          = balanced_div(best_rest),
    best_trans         = balanced_div(best_trans),
    best_tot           = balanced_div(best_tot),
    moca_tot_scale     = moca_tot,
    moca_abs           = balanced_div(moca_abs),
    moca_ate           = balanced_div(moca_ate),
    moca_evoc          = balanced_div(moca_evoc),
    moca_lin           = balanced_div(moca_lin),
    moca_nom           = balanced_div(moca_nom),
    moca_ori           = balanced_div(moca_ori),
    moca_vis           = balanced_div(moca_vis),
    moca_tot           = balanced_div(moca_tot),
    dgi                = balanced_div(dgi),
    tug_custo          = balanced_div(tug_custo),
  ) %>% 
  select(-hy)

#BD final
db_gol <- inner_join(expl, resp2, by = "alias")[,-1]
variaveis_resp <- names(resp2)[-1]
variaveis_resp <- variaveis_resp[variaveis_resp != "moca_tot_scale"]
variaveis_expl <- names(expl)[-1]
db_class <- list(db_gol         = db_gol,
                 variaveis_expl = variaveis_expl,
                 variaveis_resp = variaveis_resp)

# Salvando BD
write_rds(db_class, fname_data("JG-classify.rds"))
