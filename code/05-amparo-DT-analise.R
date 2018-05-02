######################################################
## Esta seção ainda está extremamente experimental. ##
######################################################

library(ordinal)
library(tidyverse)

#################################################
## Calculo de custos e verificacao de indices. ##
#################################################
dt = read.csv("./data/amparo/dt.csv") %>%
  as.tibble() %>%
  mutate(hy = as.ordered(hy),
         custo_x30s_dist = (x30s_dist_ts - x30s_dist_td)/x30s_dist_ts,
         custo_x30s_fv = (x30s_fv_ts - x30s_fv_td)/x30s_fv_ts)

# Indice simples confirma ME.
dt %>%
  ggplot(aes(x = custo_x30s_dist, y = custo_x30s_fv, color = x30s_idx_1)) +
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

formula = hy ~ custo_x30s_dist*custo_x30s_fv
om_custos = clm(formula, data = dt)
preds = predict(om_custos, type="class")$fit
mean(as.numeric(dt$hy) == as.numeric(preds))
mean(abs(as.numeric(dt$hy) - as.numeric(preds)) <= 1)

new_dt = dt_pca$x %>% 
  as.tibble() %>%
  mutate(hy = dt$hy)
formula = hy ~ PC1 + PC2 + PC3 + PC4
om_custos = clm(formula, data = new_dt)
preds = predict(om_custos, type="class")$fit
mean(as.numeric(dt$hy) == as.numeric(preds))
mean(abs(as.numeric(dt$hy) - as.numeric(preds)) <= 1)
