######################################################
## Esta seção ainda está extremamente experimental. ##
######################################################

library(ordinal)
library(tidyverse)

dt = "../data/amparo/dt.csv" %>% 
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

dt %>% 
  filter(custo_x10m_dist > -2) %>%
  ggplot(aes(x=idx_2, y=custo_x10m_dist))+
  geom_point()+
  geom_smooth(method='lm',formula=y~x)

formula = hy ~ custo_x30s_dist*custo_x30s_fv
formula = hy ~ custo_x10m_dist*custo_x10m_fv
om_custos = clm(formula, data = dt)
preds = predict(om_custos, type="class")$fit
mean(as.numeric(dt$hy) == as.numeric(preds))

dt %>% ggplot(aes(x=custo_x30s_dist, y=custo_x30s_fv, color=idx_1))+
  geom_point()

dt_1 = tibble(hy=dt$hy, idx=dt$idx_1, num=1) %>% na.omit()
dt_2 = tibble(hy=dt$hy, idx=dt$idx_2, num=2) %>% na.omit()
dt_ = rbind(dt_1, dt_2) %>% mutate(num = as.factor(num))


om_idx_1 = clm(hy ~ idx_1, data = dt)
preds = predict(om_idx_1, type="class")$fit
mean(as.numeric(dt$hy) == as.numeric(preds))

dt_1 = dt %>% 
  select(hy, idx_1,
         x30s_dist_ts, x30s_dist_td, x30s_fv_ts, x30s_fv_td, 
         x10m_dist_ts, x10m_dist_td, x10m_fv_ts, x10m_fv_td) %>%
  na.omit()

om_x30s = clm(hy ~ x30s_dist_ts + x30s_dist_td + x30s_fv_ts + x30s_fv_td, data = dt_1)
preds = predict(om_x30s, type="class")$fit
mean(as.numeric(dt_1$hy) == as.numeric(preds))

om_x30s_2 = clm(hy ~ idx_1, data = dt_1)
preds = predict(om_x30s_2, type="class")$fit
mean(as.numeric(dt_1$hy) == as.numeric(preds))

om_x10m = clm(hy ~ x10m_dist_ts + x10m_dist_td + x10m_fv_ts + x10m_fv_td, data = dt_1)
preds = predict(om_x10m, type="class")$fit
mean(as.numeric(dt_1$hy) == as.numeric(preds))
