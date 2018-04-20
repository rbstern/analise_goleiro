######################################################
## Esta seção ainda está extremamente experimental. ##
######################################################

library(ordinal)
library(tidyverse)

dt = "./data/amparo/dt.csv" %>% 
  read.csv() %>%
  as.tibble() %>%
  filter(hy != 0) %>%
  mutate(hy = as.ordered(hy),
         custo_x30s_dist = (x30s_dist_ts - x30s_dist_td)/x30s_dist_ts,
         custo_x30s_fv = (x30s_fv_ts - x30s_fv_td)/x30s_fv_ts,
         custo_x10m_dist = (x10m_dist_ts - x10m_dist_td)/x10m_dist_ts,
         custo_x10m_fv = (x10m_fv_ts - x10m_fv_td)/x10m_fv_ts,
         idx_1 = ((x30s_dist_td*x30s_fv_td)-(x30s_dist_ts*x30s_fv_ts))/sqrt(x30s_dist_ts*x30s_fv_ts), 
         idx_2 = ((x10m_dist_td*x10m_fv_td)-(x10m_dist_ts*x10m_fv_ts))/sqrt(x10m_dist_ts*x10m_fv_ts)) %>%
  na.omit()

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

# a. Grafico de radar.
# b. Desenvolvimento do indice.

##########################################
### Tentativas falhas de modelagem     ###
### usando regressao logistica ordinal ###
##########################################
# dt %>% 
#   ggplot(aes(x = custo_x30s_dist, y = custo_x30s_fv, colour = idx_1)) +
#   geom_point()
# 
# dt %>% 
#   filter(custo_x10m_dist > -2) %>%
#   ggplot(aes(x=idx_2, y=custo_x10m_dist))+
#   geom_point()+
#   geom_smooth(method='lm',formula=y~x)
# 
# formula = hy ~ custo_x30s_dist*custo_x30s_fv
# formula = hy ~ custo_x10m_dist*custo_x10m_fv
# om_custos = clm(formula, data = dt)
# preds = predict(om_custos, type="class")$fit
# mean(as.numeric(dt$hy) == as.numeric(preds))
# 
# dt %>% ggplot(aes(x=custo_x30s_dist, y=custo_x30s_fv, color=idx_1))+
#   geom_point()
# 
# dt_1 = tibble(hy=dt$hy, idx=dt$idx_1, num=1) %>% na.omit()
# dt_2 = tibble(hy=dt$hy, idx=dt$idx_2, num=2) %>% na.omit()
# dt_ = rbind(dt_1, dt_2) %>% mutate(num = as.factor(num))
# 
# 
# om_idx_1 = clm(hy ~ idx_1, data = dt)
# preds = predict(om_idx_1, type="class")$fit
# mean(as.numeric(dt$hy) == as.numeric(preds))
# 
# dt_1 = dt %>% 
#   select(hy, idx_1,
#          x30s_dist_ts, x30s_dist_td, x30s_fv_ts, x30s_fv_td, 
#          x10m_dist_ts, x10m_dist_td, x10m_fv_ts, x10m_fv_td) %>%
#   na.omit()
# 
# om_x30s = clm(hy ~ x30s_dist_ts + x30s_dist_td + x30s_fv_ts + x30s_fv_td, data = dt_1)
# preds = predict(om_x30s, type="class")$fit
# mean(as.numeric(dt_1$hy) == as.numeric(preds))
# 
# om_x30s_2 = clm(hy ~ idx_1, data = dt_1)
# preds = predict(om_x30s_2, type="class")$fit
# mean(as.numeric(dt_1$hy) == as.numeric(preds))
# 
# om_x10m = clm(hy ~ x10m_dist_ts + x10m_dist_td + x10m_fv_ts + x10m_fv_td, data = dt_1)
# preds = predict(om_x10m, type="class")$fit
# mean(as.numeric(dt_1$hy) == as.numeric(preds))
