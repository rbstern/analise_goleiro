######################################################
## Esta seção ainda está extremamente experimental. ##
######################################################

library(glmnet)
library(ordinal)
library(randomForest)
library(tidyverse)
library(magrittr)

# Tarefas:
# 1. Entender o que é a reta entre PC1 e PC2.

#################################################
## Calculo de custos e verificacao de indices. ##
#################################################
dt = read.csv("./data/amparo/dt.csv") %>%
  as.tibble() %>%
  filter(moca >= 23) %>%
  mutate(hy = as.ordered(hy),
         custo_x30s_dist = (x30s_dist_ts - x30s_dist_td)/x30s_dist_ts,
         custo_x30s_fv = (x30s_fv_ts - x30s_fv_td)/x30s_fv_ts)

# Indice simples confirma ME.
dt %>%
  filter(hy <= 1) %>%
  ggplot(aes(x = custo_x30s_dist, y = custo_x30s_fv, color = hy)) +
  geom_point()

# Modelo ainda está péssimo.
dt %>%
  ggplot(aes(x = custo_x30s_dist, y = custo_x30s_fv, color = x30s_idx_2)) +
  geom_point()

#########################################################
### 1. Estudo de dupla tarefa com spectral clustering ###
#########################################################

# Selecionar apenas dupla tarefa
dt_pure = dt %>%
  select(td_m = x30s_dist_td,
         td_v = x30s_fv_td,
         ts_m = x30s_dist_ts,
         ts_v = x30s_fv_ts) %>%
  select(td_m, td_v, ts_m, ts_v)

###################
## a. BD inteiro ##
###################

# Componentes principais
dt_pca = dt_pure %>% 
  as.matrix() %>%
  prcomp(center = TRUE, scale = TRUE)
dt_pca
dt_pca$sdev

# Agrupamentos nos componentes principais
cluster_frame = as.tibble(dt_pca$x)
for(ii in 2:6)
{
  name = paste("cluster_", ii, sep="")
  cluster_frame[[name]] = kmeans(dt_pca$x, centers = ii)$cluster
  cluster_frame[[name]] = as.factor(cluster_frame[[name]])
}

# Gráficos dos agrupamentos obtidos
cluster_frame %>% 
  ggplot(aes(x = PC1, y = PC2, colour = cluster_2)) +
  geom_point()

cluster_frame %>% 
  ggplot(aes(x = PC1, y = PC2, colour = cluster_3)) +
  geom_point()

cluster_frame %>% 
  ggplot(aes(x = PC1, y = PC2, colour = cluster_4)) +
  geom_point()

cluster_frame %>% 
  ggplot(aes(x = PC1, y = PC2, colour = cluster_5)) +
  geom_point()

#######################
## b. Medidas usuais ##
#######################

dt_idx = dt_pure %>%
  mutate(idx_m = (ts_m-td_m)/ts_m,
         idx_v = (ts_v-td_v)/ts_v) %>%
  select(idx_m, idx_v)

# Comparar com agrupamento no BD inteiro
dt_idx %>% ggplot(aes(x = idx_m, y = idx_v, colour = cluster_frame$cluster_4))+
geom_point()

# Componentes principais
dt_pca_idx = dt_idx %>% 
  as.matrix() %>%
  prcomp(center = TRUE, scale = TRUE)
dt_pca_idx
dt_pca_idx$sdev

# Agrupamentos nos componentes principais
cluster_frame_idx = as.tibble(dt_pca_idx$x)
for(ii in 2:6)
{
  name = paste("cluster_", ii, sep="")
  cluster_frame_idx[[name]] = kmeans(dt_pca_idx$x, centers = ii)$cluster
  cluster_frame_idx[[name]] = as.factor(cluster_frame_idx[[name]])
}

# Gráficos dos agrupamentos obtidos
cluster_frame_idx %>% 
  ggplot(aes(x = PC1, y = PC2, colour = cluster_2)) +
  geom_point()

cluster_frame_idx %>% 
  ggplot(aes(x = PC1, y = PC2, colour = cluster_3)) +
  geom_point()

cluster_frame_idx %>% 
  ggplot(aes(x = PC1, y = PC2, colour = cluster_4)) +
  geom_point()

cluster_frame_idx %>% 
  ggplot(aes(x = PC1, y = PC2, colour = cluster_5)) +
  geom_point()

######################################
### Tentativas de modelagem usando ###
###  regressao logistica ordinal   ###
######################################

## BD inteiro

formula = hy ~ custo_x30s_dist*custo_x30s_fv
om_custos = clm(formula, data = dt)
preds = predict(om_custos, type="class")$fit
mean(as.numeric(dt$hy) == as.numeric(preds))

formula = hy ~ custo_x30s_dist
om_custos = clm(formula, data = dt)
preds = predict(om_custos, type="class")$fit
mean(as.numeric(dt$hy) == as.numeric(preds))

formula = hy ~ custo_x30s_fv
om_custos = clm(formula, data = dt)
preds = predict(om_custos, type="class")$fit
mean(as.numeric(dt$hy) == as.numeric(preds))

## HY <= 1

dt_mod = dt %>%
  filter(hy <= 1) %>%
  mutate(tot = (custo_x30s_fv+custo_x30s_dist)/2)

formula = hy ~ custo_x30s_dist*custo_x30s_fv
om_custos = clm(formula, data = dt_mod)
preds = predict(om_custos, type="class")$fit
mean(as.numeric(dt_mod$hy) == as.numeric(preds))

formula = hy ~ custo_x30s_dist
om_custos = clm(formula, data = dt_mod)
preds = predict(om_custos, type="class")$fit
mean(as.numeric(dt_mod$hy) == as.numeric(preds))

formula = hy ~ custo_x30s_fv
om_custos = clm(formula, data = dt_mod)
preds = predict(om_custos, type="class")$fit
mean(as.numeric(dt_mod$hy) == as.numeric(preds))

formula = hy ~ tot
om_custos = clm(formula, data = dt_mod)
preds = predict(om_custos, type="class")$fit
mean(as.numeric(dt_mod$hy) == as.numeric(preds))

new_dt = dt_pca$x %>% 
  as.tibble() %>%
  mutate(hy = dt$hy) %>%
  filter(hy <= 1)
formula = hy ~ PC1 + PC2 + PC3 + PC4
om_custos = clm(formula, data = new_dt)
preds = predict(om_custos, type="class")$fit
mean(as.numeric(new_dt$hy) == as.numeric(preds))

# Floresta aleatoria
dt_mod_2 = dt %>% 
  mutate(hy = as.factor(as.character(hy)),
         hy = as.factor(hy != 0)) %>%
  select(-X, -genero, -idade, -escolaridade, -x30s_idx_1, -x30s_idx_2)
formula = hy ~ .
randomForest(formula, data = dt_mod_2)

dt_mod_2 = dt %>% 
  mutate(hy = as.factor(as.character(hy)),
         hy = as.factor(hy != 0)) %>%
  select(-X, -genero, -idade, -escolaridade, -x30s_idx_1, -x30s_idx_2
         -x30s_fv_td, -x30s_fv_ts, -custo_x30s_fv)
formula = hy ~ .
randomForest(formula, data = dt_mod_2)

dt_mod_2 = dt %>% 
  mutate(hy = as.factor(as.character(hy)),
         hy = as.factor(hy != 0)) %>%
  select(hy, x30s_dist_td, x30s_dist_ts, x30s_fv_td, x30s_fv_ts, moca)
formula = hy ~ .
randomForest(formula, data = dt_mod_2)

#####################################
### Comparar JG e MoCA em relação ###
###      à previsão de DT         ###
#####################################

dt = read.csv("./data/amparo/completa.csv") %>%
  as.tibble() %>%
  select(custo_10m_fv = Cust_10M_FV_, 
         custo_10m_cr = Cust_10M_CR_, # Medidas de performance em DT 10m.
         custo_30s_cr = X30._CR_,
         custo_30s_fv = X30._FV_,     
         x10m_fv_ts = BL_30.,
         x10m_fv_dt = Pal_DT_30.,     # Medidas de performance em DT 30s.
         beta_3, beta_4, beta_7, 
         gamma_3, gamma_4, gamma_7,
         time_3, time_4, time_7,      # Medidas de performance em JG.
         moca_tot = MoCA_TOTAL) %>%   # MoCA total.
  mutate(custo_10m_fv = as.numeric(as.character(custo_10m_fv)), 
         custo_10m_cr = as.numeric(as.character(custo_10m_cr)),
         custo_30s_fv = as.numeric(as.character(custo_30s_fv)), 
         custo_30s_cr = as.numeric(as.character(custo_30s_cr)),
         custo_cog_30s_fv = (x10m_fv_ts-x10m_fv_dt)/x10m_fv_ts,
         custo_total = (custo_30s_fv+custo_cog_30s_fv)/2) %>%
  na.omit() %>%
  mutate(cat_custo_10m_fv = custo_10m_fv >= median(custo_10m_fv),
         cat_custo_10m_cr = custo_10m_cr >= median(custo_10m_cr),
         cat_custo_30s_fv = custo_30s_fv >= median(custo_10m_fv),
         cat_custo_30s_cr = custo_30s_cr >= median(custo_30s_cr))

resp_vars = c("custo_10m_cr", "custo_10m_fv", "custo_30s_cr", "custo_30s_fv",
              "custo_cog_30s_fv", "custo_total")
jg_vars = c("beta_3", "beta_4", "beta_7", 
            "gamma_3", "gamma_4", "gamma_7", 
            "time_3", "time_4", "time_7")
moca_vars = c("moca_tot")

errors = NULL

X = dt %>% select(jg_vars) %>% as.matrix()
for(resp_var in resp_vars)
{
  Y = dt[[resp_var]] %>% as.matrix()
  aux <- cv.glmnet(X, Y, keep = TRUE, nfolds = nrow(dt))
  i <- which(aux$lambda == aux$lambda.min)
  error = (aux$fit.preval[,i] - Y) %>% abs() %>% mean()
  errors %<>% rbind(tibble(method = paste(resp_var, "JG", sep="_"), 
                           error = error))
}

X = dt$moca_tot %>% as.matrix()
X %<>% cbind(rep(1, nrow(X)))
for(resp_var in resp_vars)
{
  Y = dt[[resp_var]] %>% as.matrix()
  aux <- cv.glmnet(X, Y, keep = TRUE, nfolds = nrow(dt))
  i <- which(aux$lambda == aux$lambda.min)
  error = (aux$fit.preval[,i]-Y) %>% abs() %>% mean()
  errors %<>% rbind(tibble(method = paste(resp_var, "MOCA", sep="_"), 
                           error = error))
}

X[,1] = rnorm(nrow(X))
for(resp_var in resp_vars)
{
  Y = dt[[resp_var]] %>% as.matrix()
  aux <- cv.glmnet(X, Y, keep = TRUE, nfolds = nrow(dt))
  i <- which(aux$lambda == aux$lambda.min)
  error = (aux$fit.preval[,i]-Y) %>% abs() %>% mean()
  errors %<>% rbind(tibble(method = paste(resp_var, "null", sep="_"), 
                           error = error))
}

errors




####

dt = read.csv("./data/amparo/custos.csv") %>%
  as.tibble() %>%
  select(HY, custo.total, custo.motor, custo.cognitivo) %>%
  filter(HY != 4) %>%
  filter(HY != "")

dt %>%
  ggplot(aes(y = custo.cognitivo, x = HY)) +
  geom_boxplot()
