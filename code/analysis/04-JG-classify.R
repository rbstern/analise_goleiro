# Usar como covariaveis numero de tentativas
# se chegou ao fim da fase ou nao.
# Analise apenas para hy > 0.

library(abjutils)
library(boot)
library(glmnet)
library(magrittr)
library(rpart)
library(ROCR)
library(rstan)
library(tidyverse)
source("./libs/lib_fnames.R")

db_class <- fname_data("JG-classify.rds") %>%  
  read_rds()
db_gol <- db_class$db_gol
db_gol <- as_tibble(db_gol)
variaveis_expl <- db_class$variaveis_expl
variaveis_resp <- db_class$variaveis_resp
db_gol2 <- db_gol %>% na.omit

expl_form <- paste(variaveis_expl, collapse = "+")

theme = theme_set(theme_minimal(base_size = 26))
theme = theme_update(legend.position="top")
data_roc_all <- NULL

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

  preds <- prediction(aux$fit.preval[,i], db_gol_u[[tvar]])
  roc <- performance(preds, measure = "tpr", x.measure = "fpr")
  auc <- performance(preds, measure = "auc")
  data_roc <- tibble("espec"    = roc@x.values[[1]],
                     "sens"     = roc@y.values[[1]],
                     "auc"      = auc@y.values[[1]],
                     "variable" = tvar,
                     "method"   = "GLMNET")
  data_roc_all %<>% rbind(data_roc)
}

# MOCA
for(tvar in variaveis_resp)
{
  db_gol_u <- db_gol
  if(db_gol[[tvar]] %>% is.na() %>% sum()) { db_gol_u <- db_gol2 }
  
  preds <- prediction(db_gol_u$moca_tot_scale, db_gol_u[[tvar]])
  roc <- performance(preds, measure = "tpr", x.measure = "fpr")
  auc <- performance(preds, measure = "auc")
  data_roc <- tibble("espec"    = roc@x.values[[1]],
                     "sens"     = roc@y.values[[1]],
                     "auc"      = auc@y.values[[1]],
                     "variable" = tvar,
                     "method"   = "MOCA")
  data_roc_all %<>% rbind(data_roc)
}

#Arvores de decisao
# for(tvar in variaveis_resp)
# {
#   formula <- tvar %>% paste("~", expl_form, sep = "") %>% as.formula()
#   db_gol_u <- db_gol
#   if(db_gol[[tvar]] %>% is.na() %>% sum()) { db_gol_u <- db_gol2 }
#   k <- nrow(db_gol_u)
#   predictions <- rep(NA, k)
#   for(i in 1:k) {
#     model <- rpart(formula, data = db_gol_u[-i,], method = "class")
#     model %<>% prune(cp = model$cptable[which.min(aux$cptable[,"xerror"]), "CP"])
#     predictions[i] <- predict(model, newdata = db_gol_u[i, , drop=F])[2]
#   }
#   roc <- prediction(predictions, db_gol_u[[tvar]])
#   roc <- performance(roc, measure = "tpr", x.measure = "fpr") 
#   data_roc <- tibble("espec"    = roc@x.values[[1]],
#                      "sens"     = roc@y.values[[1]],
#                      "variable" = tvar,
#                      "method"   = "RPART")
#   data_roc_all %<>% rbind(data_roc)
#   }

write_rds(data_roc_all, 
          fname_data("JG-roc.rds"))
data_roc_all = read_rds(fname_data("JG-roc.rds"))

#Curva ROC
g <- data_roc_all %>%
  ggplot()+
  geom_line(aes(x = espec, y = sens, color = method), size = 1.2)+
  xlab("1-Specificity")+
  ylab("Sensitivity")+
  geom_abline(size = 1.2)+
  facet_wrap( ~ variable, ncol = 4)
fname_plot("JG-classify.pdf") %>% 
  ggsave(height = 17, width = 14)

#Curva ROC artigo
data_roc_all %<>%
  filter(variable == "best_lim" | variable == "hy_3") %>%
  filter(method == "GLMNET" | method == "MOCA")
data_roc_all$method[data_roc_all$method == "GLMNET"] <- "JG"

g <- data_roc_all %>%
  ggplot()+
  geom_line(aes(x = espec, y = sens, color = method), size = 1.2)+
  xlab("1-Especificidade")+
  ylab("Sensitividade")+
  geom_abline(size = 1.2)+
  facet_wrap( ~ variable, ncol = 4)
fname_plot("JG-classify-art.pdf") %>% 
ggsave(height = 17, width = 14)

## Zona em testes
#Floresta aleatoria
#performance terrivel
#expl2 <- dt.gol2 %>% select((expl %>% names)[-1])
#aux <- randomForest(x=expl2, y=dt.gol2$best_marcha %>% as.factor)
#rfcv(expl[,-1], resp2$best_tot %>% as.factor)$error.cv

#Arvore de decisao
# formula <- paste("moca_abs~",
#                  variaveis.expl %>% paste(collapse="+"),sep="")
