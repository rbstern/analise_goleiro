library(caret)
library(ordinal)
library(randomForest)
library(stringr)
library(tidyverse)
library(magrittr)

dt = read.csv("./data/amparo/completa.csv")
dt = as.tibble(dt)
names(dt) = str_to_lower(names(dt))
names(dt) = gsub("x[.]", "", names(dt))

dt$hy_ = ifelse(is.na(dt$hy_), 0, dt$hy_)

vars = str_to_lower(c("Nx", "Ny", "duracao", "distancia", 
         "VelMedia", "VelMax", "VxMedio", "Vymedio", "alfa", "DurApoio",
         "RmX", "RmY", "VmX", "VmY", "StdRx", "DurPassada"))
tipos = str_to_lower(c("TS", "FV", "CR"))
testes = str_to_lower(c("3M", "6M", "8.40M"))

nomes = c()
for(var in vars)
{
  for(teste in testes)
  {
    nomes = c(nomes, paste(var, teste, sep = "_"))
  }
}

var_names = c()
new_dt = dt
for(nome in nomes)
{
  name_ts = paste(nome, "ts", sep = "_")
  name_cr = paste(nome, "cr", sep = "_")
  cost_cr = paste("custo", nome, "cr", sep = "_")
  name_fv = paste(nome, "fv", sep = "_")
  cost_fv = paste("custo", nome, "fv", sep = "_")
  var_names = c(var_names, cost_cr, cost_fv)
  new_dt[[cost_cr]] = (new_dt[[name_ts]]-new_dt[[name_cr]])/new_dt[[name_ts]]
  new_dt[[cost_fv]] = (new_dt[[name_ts]]-new_dt[[name_fv]])/new_dt[[name_ts]]
}

all_vars = c("hy_", "idad_", "moca_total", var_names)
new_dt %<>%
  select(all_vars) %>%
  mutate(hy_ = as.ordered(hy_)) %>%
  na.omit() %>% 
  filter(hy_ != 0)

all_vars_3m = var_names[grep("3m", var_names)]
all_vars_6m = var_names[grep("6m", var_names)]
all_vars_8m = var_names[grep("8[.]40m", var_names)]

pca_custos = new_dt %>% 
  select(all_vars_6m) %>% 
  prcomp()
plot(pca_custos$sdev/sum(pca_custos$sdev))

num_comps = 2:20
acertos = rep(0, 20)
for(num_comp in num_comps)
{
  print(num_comp)
  new_vars = pca_custos$x[,1:num_comp]
  bd_3m = as.data.frame(cbind(as.numeric(new_dt$hy_)-1,
                              new_dt$idad_,
                              new_dt$moca_total,
                              new_vars)) %>%
  mutate(V1 = as.ordered(V1)) %>%
  as.tibble()  
  for(ii in 1:nrow(new_dt))
  {
    new_bd_3m = bd_3m[-ii,]
    formula = V1 ~ .
    om_custos = clm(formula, data = new_bd_3m)
    new_x = bd_3m[ii,]
    acertos[num_comp] = acertos[num_comp] +
      (as.numeric(predict(om_custos, newdata = bd_3m, type="class")$fit[ii]) ==
         as.numeric(bd_3m$V1[ii]))
  }
}
acertos 

###

bd_6m = as.data.frame(cbind(as.numeric(new_dt$hy_)-1,
                            new_dt$idad_,
                            new_dt$moca_total,
                            pca_custos$x[,1:5])) %>%
  mutate(V1 = as.factor(V1)) %>%
  as.tibble()
formula = V1 ~ .
randomForest(formula, data = bd_6m)

for(num_comp in num_comps)
{
  print(num_comp)
  new_vars = pca_custos$x[,1:num_comp]
  bd_3m = as.data.frame(cbind(as.numeric(new_dt$hy_)-1,
                              new_dt$idad_,
                              new_dt$moca_total,
                              new_vars)) %>%
    mutate(V1 = as.ordered(V1)) %>%
    as.tibble()  
  for(ii in 1:nrow(new_dt))
  {
    new_bd_3m = bd_3m[-ii,]
    formula = V1 ~ .
    om_custos = clm(formula, data = new_bd_3m)
    new_x = bd_3m[ii,]
    acertos[num_comp] = acertos[num_comp] +
      (as.numeric(predict(om_custos, newdata = bd_3m, type="class")$fit[ii]) ==
         as.numeric(bd_3m$V1[ii]))
  }
}
acertos



###

new_dt_2 = dt %>%
  select(hy_, idad_, moca_total,
         x10m_ts_, x10m_fv_, x10m_cr_,
         subtração_10m, pal_dt_10m) %>%
  na.omit() %>%
  mutate(hy_ = as.ordered(hy_))
  
formula = hy_ ~ .
om_custos = clm(formula, data = new_dt_2)
preds = predict(om_custos, type = "class")$fit
confusionMatrix(preds, as.factor(new_dt_2$hy_))

###

new_dt_2 = dt %>%
  select(hy_, idad_, moca_total,
         x10m_ts_, x10m_fv_, x10m_cr_,
         subtração_10m, pal_dt_10m) %>%
  na.omit() %>%
  mutate(hy_ = as.ordered(hy_))

formula = hy_ ~ .
om_custos = clm(formula, data = new_dt_2)
preds = predict(om_custos, type = "class")$fit
confusionMatrix(preds, as.factor(new_dt_2$hy_))








###





