library(abjutils)
library(boot)
library(glmnet)
library(magrittr)
library(rpart)
library(ROCR)
library(rstan)
library(tidyverse)

db_class <- readRDS("./data/amparo/amparo-JG-data-classify.rds")
db_gol <- db_class$db_gol
db_gol <- as.tibble(db_gol)
variaveis_expl <- db_class$variaveis_expl
variaveis_resp <- db_class$variaveis_resp
db_gol2 <- db_gol %>% na.omit

expl_form <- paste(variaveis_expl, collapse="+")

#Regressao logistica (glm)
theme = theme_set(theme_minimal(base_size = 26))
theme = theme_update(legend.position="top")

data_roc_all <- NULL
for(tvar in variaveis_resp)
{
  db_gol_u <- db_gol
  if(db_gol[[tvar]] %>% is.na %>% sum) { db_gol_u <- db_gol2 }
  formula <- tvar %>% paste("~", expl_form, sep = "") %>% as.formula()
  k <- nrow(db_gol_u)
  predictions <- rep(NA, k)
  for(i in 1:k) {
    model <- glm(formula, family = binomial, data = db_gol_u[-i,])
    predictions[i] <- predict(model, newdata = db_gol_u[i, , drop = F])
  }
  roc <- prediction(predictions, db_gol_u[[tvar]])
  roc <- performance(roc, measure = "tpr", x.measure = "fpr") 
  data_roc <- tibble("espec"    = roc@x.values[[1]],
                     "sens"     = roc@y.values[[1]],
                     "variable" = tvar,
                     "method"   = "GLM")
  data_roc_all %<>% rbind(data_roc)
}

# Regressao logistica usando glmnet
for(tvar in variaveis_resp)
{
  db_gol_u <- db_gol
  if(db_gol[[tvar]] %>% is.na() %>% sum()) { db_gol_u <- db_gol2 }
  x= db_gol_u %>% select(variaveis_expl) %>% as.matrix()
  y= db_gol_u %>% select(tvar) %>% as.matrix() %>% as.factor()
  aux <- cv.glmnet(x, y, family = "binomial",
                   keep = TRUE, nfolds=nrow(db_gol_u))
  i <- which(aux$lambda == aux$lambda.min)
  # coeficientes:
  coefficients(aux, s = aux$lambda.min)

  roc <- prediction(aux$fit.preval[,i], db_gol_u[[tvar]])
  roc <- performance(roc, measure = "tpr", x.measure = "fpr") 
  data_roc <- tibble("espec"    = roc@x.values[[1]],
                     "sens"     = roc@y.values[[1]],
                     "variable" = tvar,
                     "method"   = "GLMNET")
  data_roc_all %<>% rbind(data_roc)
}

# MOCA
for(tvar in variaveis_resp)
{
  db_gol_u <- db_gol
  if(db_gol[[tvar]] %>% is.na() %>% sum()) { db_gol_u <- db_gol2 }
  
  roc <- prediction(db_gol_u$moca_tot_scale, db_gol_u[[tvar]])
  roc <- performance(roc, measure = "tpr", x.measure = "fpr") 
  data_roc <- tibble("espec"    = roc@x.values[[1]],
                     "sens"     = roc@y.values[[1]],
                     "variable" = tvar,
                     "method"   = "MOCA")
  data_roc_all %<>% rbind(data_roc)
}

#Arvores de decisao
for(tvar in variaveis_resp)
{
  formula <- tvar %>% paste("~", expl_form, sep = "") %>% as.formula()
  db_gol_u <- db_gol
  if(db_gol[[tvar]] %>% is.na() %>% sum()) { db_gol_u <- db_gol2 }
  k <- nrow(db_gol_u)
  predictions <- rep(NA, k)
  for(i in 1:k) {
    model <- rpart(formula, data = db_gol_u[-i,], method = "class")
    model %<>% prune(cp = model$cptable[which.min(aux$cptable[,"xerror"]), "CP"])
    predictions[i] <- predict(model, newdata = db_gol_u[i, , drop=F])[2]
  }
  roc <- prediction(predictions, db_gol_u[[tvar]])
  roc <- performance(roc, measure = "tpr", x.measure = "fpr") 
  data_roc <- tibble("espec"    = roc@x.values[[1]],
                     "sens"     = roc@y.values[[1]],
                     "variable" = tvar,
                     "method"   = "RPART")
  data_roc_all %<>% rbind(data_roc)
  }

#write_rds(data_roc_all, "../data/amparo/amparo-JG-classify.rds")
data_roc_all = read_rds("../data/amparo/amparo-JG-classify.rds")

#Curva ROC
g <- data_roc_all %>%
  ggplot()+
  geom_line(aes(x = espec, y = sens, color = method), size = 1.2)+
  xlab("1-Specificity")+
  ylab("Sensitivity")+
  geom_abline(size = 1.2)+
  facet_wrap( ~ variable, ncol = 4)
ggsave("../plots/amparo-analise-JG-classify.pdf", height = 17, width = 14)

#Curva ROC artigo
data_roc_all %<>%
  filter(variable=="best_lim") %>%
  filter(method=="GLMNET" | method=="MOCA")
data_roc_all$method[data_roc_all$method == "GLMNET"] <- "JG"
g <- data_roc_all %>%
  ggplot()+
  geom_line(aes(x = espec, y = sens, color = method), size = 1.2)+
  xlab("1-Especificidade")+
  ylab("Sensitividade")+
  geom_abline(size = 1.2)+
  facet_wrap( ~ variable, ncol = 4)
ggsave("../plots/amparo-analise-JG-classify-art.jpg", height = 17, width = 14)

## Zona em testes
#Floresta aleat?ria
#performance terrivel
#expl2 <- dt.gol2 %>% select((expl %>% names)[-1])
#aux <- randomForest(x=expl2, y=dt.gol2$best_marcha %>% as.factor)
#rfcv(expl[,-1], resp2$best_tot %>% as.factor)$error.cv

#?rvore de decis?o
formula <- paste("moca_abs~",
                 variaveis.expl %>% paste(collapse="+"),sep="")

### Nova rodada de classificacao
dt = read.csv("./data-raw/amparo/completa.csv")
names(dt) = str_to_lower(names(dt))
dt %<>% 
  mutate(tug_custo = (tug_st-tug_dt)/tug_st) %>%
  mutate(escol_       = rm_accent(escol_),
         escolaridade = 1+grepl("Medio", escol_) +
           2*grepl("Superior", escol_),
         custo_fv_30s = (bl_fv_30.-pal_dt_30.)/bl_fv_30.) %>%
  as.tibble()

variaveis_resp = c("dgi_", "tug_custo") #"tug_st", "tug_dt", )

all_vars = c("moca_total", variaveis_resp, variaveis_expl)
dt %<>% 
  select(all_vars) %>%
  na.omit()

for(tvar in variaveis_resp)
{
  t_median = median(dt[[tvar]])
  prop_1 = abs(mean(dt[[tvar]] >= t_median) - 0.5)
  prop_2 = abs(mean(dt[[tvar]] > t_median) - 0.5)
  if(prop_1 < prop_2) dt[[tvar]] = dt[[tvar]] >= t_median
  else dt[[tvar]] = dt[[tvar]] > t_median
}

variaveis_expl = variaveis_expl[1:9]
data_roc_all = NULL
data_acc_all = NULL
# Regressao logistica usando glmnet
for(tvar in variaveis_resp)
{
  db_gol = dt
  x = db_gol %>% select(variaveis_expl) %>% as.matrix()
  y = db_gol %>% select(tvar) %>% as.matrix() %>% as.factor()
  aux <- cv.glmnet(x, y, family = "binomial",
                   keep = TRUE, nfolds=nrow(db_gol))
  i <- which(aux$lambda == aux$lambda.min)
  # coeficientes:
  coefficients(aux, s = aux$lambda.min)
  
  roc <- prediction(aux$fit.preval[,i], db_gol[[tvar]])
  t_acc = max(roc@tp[[1]] + roc@tn[[1]])
  t_n = max(roc@tp[[1]] + roc@tn[[1]] + roc@fp[[1]] + roc@fn[[1]])
  
  roc <- performance(roc, measure = "tpr", x.measure = "fpr") 
  data_roc <- tibble("espec"    = roc@x.values[[1]],
                     "sens"     = roc@y.values[[1]],
                     "variable" = tvar,
                     "covariate"   = "GG")
  data_roc_all %<>% rbind(data_roc)
  data_acc = tibble(
    acc = t_acc/t_n,
    variable = tvar,
    covariate = "GG"
  )
  data_acc_all %<>% rbind(data_acc)
}

for(tvar in variaveis_resp)
{
  db_gol = dt
  roc <- prediction(db_gol$moca_total, db_gol[[tvar]])
  t_acc = max(roc@tp[[1]]+roc@tn[[1]])
  t_n = max(roc@tp[[1]]+roc@tn[[1]]+roc@fp[[1]]+roc@fn[[1]])
  roc <- performance(roc, measure = "tpr", x.measure = "fpr") 
  data_roc <- tibble("espec"    = roc@x.values[[1]],
                     "sens"     = roc@y.values[[1]],
                     "variable" = tvar,
                     "covariate"   = "MOCA")
  data_roc_all %<>% rbind(data_roc)
  data_acc = tibble(
    acc = t_acc/t_n,
    variable = tvar,
    covariate = "MOCA"
  )
  data_acc_all %<>% rbind(data_acc)
}

#Curva ROC
g <- data_roc_all %>%
  filter(variable == "dgi_") %>%
  ggplot()+
  geom_line(aes(x = espec, y = sens, color = covariate), size = 1.2)+
  xlab("1-Specificity")+
  ylab("Sensitivity")+
  geom_abline(size = 1.2)#+
  #ggtitle("ROC (receiver operator characteristic) curves for 
  #        predicting DGI using the classifiers
  #        adjusted with either GG variables or MoCA score")
ggsave("./plots/amparo-analise-JG-classify-dgi.pdf", height = 17, width = 14)

g <- data_roc_all %>%
  filter(variable == "tug_custo") %>%
  ggplot()+
  geom_line(aes(x = espec, y = sens, color = covariate), size = 1.2)+
  xlab("1-Specificity")+
  ylab("Sensitivity")+
  geom_abline(size = 1.2)#+
  #ggtitle("ROC (receiver operator characteristic) curves for 
  #        predicting TUG cost using the classifiers
  #        adjusted with either GG variables or MoCA score")
ggsave("./plots/amparo-analise-JG-classify-tug-cost.pdf", height = 17, width = 14)

