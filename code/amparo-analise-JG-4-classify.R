library(boot)
library(glmnet)
library(magrittr)
library(ROCR)
library(rstan)
library(tidyverse)

dt.class <- readRDS("../clean_data/amparo-JG-data-classify.rds")
dt.gol <- dt.class$dt.gol
variaveis.expl <- dt.class$variaveis.expl
variaveis.resp <- dt.class$variaveis.resp
dt.gol2 <- dt.gol %>% na.omit

expl.form <- paste(variaveis.expl, collapse="+")

#Regressao logistica (glm)
theme = theme_set(theme_minimal(base_size = 26))
theme = theme_update(legend.position="top")

data.roc.all <- NULL
for(tvar in variaveis.resp[-1])
{
  dt.gol.u <- dt.gol
  if(dt.gol[[tvar]] %>% is.na %>% sum) { dt.gol.u <- dt.gol2 }
  formula <- tvar %>% paste("~",expl.form,sep="") %>% as.formula
  k <- nrow(dt.gol.u)
  predictions <- rep(NA,k)
  for (i in 1:k) {
    model <- glm(formula, family=binomial, data=dt.gol.u[-i,])
    predictions[i] <-  predict(model, newdata=dt.gol.u[i,,drop=F])
  }
  roc <- prediction(predictions, dt.gol.u[[tvar]])
  roc <- performance(roc, measure = "tpr", x.measure = "fpr") 
  data.roc <- data.frame("espec"=roc@x.values[[1]],
                         "sens"=roc@y.values[[1]],"variable"=tvar,
                         "method"="GLM")
  data.roc.all <- rbind(data.roc.all,data.roc)
}
ggplot(data=data.roc.all)+geom_line(aes(x=espec,y=sens),size=1.2)+
  xlab("1-Especificidade")+ylab("Sensibilidade")+
  geom_abline(color="red",size=1.2)+facet_wrap( ~ variable, ncol=4)

# Regressao logistica usando glmnet
for(tvar in variaveis.resp[-1])
{
  dt.gol.u <- dt.gol
  if(dt.gol[[tvar]] %>% is.na %>% sum) { dt.gol.u <- dt.gol2 }
  x= dt.gol.u %>% select(variaveis.expl) %>% as.matrix
  y= dt.gol.u %>% select(tvar) %>% as.matrix %>% as.factor
  aux <- cv.glmnet(x,y,family="binomial",
                   keep=TRUE, nfolds=nrow(dt.gol.u))
  i <- which(aux$lambda == aux$lambda.min)
  # coeficientes:
  coefficients(aux,s=aux$lambda.min)

  roc <- prediction(aux$fit.preval[,i], dt.gol.u[[tvar]])
  roc <- performance(roc, measure = "tpr", x.measure = "fpr") 
  data.roc <- data.frame("espec"=roc@x.values[[1]],
                         "sens"=roc@y.values[[1]],"variable"=tvar,
                         "method"="GLMNET")
  data.roc.all <- rbind(data.roc.all,data.roc)
}

# MOCA
for(tvar in variaveis.resp[-1])
{
  dt.gol.u <- dt.gol
  if(dt.gol[[tvar]] %>% is.na %>% sum) { dt.gol.u <- dt.gol2 }
  
  roc <- prediction(dt.gol.u$moca_tot_scale, dt.gol.u[[tvar]])
  roc <- performance(roc, measure = "tpr", x.measure = "fpr") 
  data.roc <- data.frame("espec"=roc@x.values[[1]],
                         "sens"=roc@y.values[[1]],"variable"=tvar,
                         "method"="MOCA")
  data.roc.all <- rbind(data.roc.all,data.roc)
}

g <- ggplot(data=data.roc.all)+geom_line(aes(x=espec,y=sens,color=method),
                                         size=1.2)+
  xlab("1-Especificidade")+ylab("Sensibilidade")+
  geom_abline(size=1.2)+facet_wrap( ~ variable, ncol=4)
ggsave("../plots/amparo-analise-JG-classify.pdf",height = 17,width = 14)

## Zona em testes
#Floresta aleatória
#performance terrivel
#expl2 <- dt.gol2 %>% select((expl %>% names)[-1])
#aux <- randomForest(x=expl2, y=dt.gol2$best_marcha %>% as.factor)
#rfcv(expl[,-1], resp2$best_tot %>% as.factor)$error.cv

aux %>% printcp
glm(formula, family=binomial, data=dt.gol.u)

#Árvore de decisão
formula <- paste("moca_abs~",
                 variaveis.expl %>% paste(collapse="+"),sep="")
results.dt <- list()
for(tvar in variaveis.resp)
{
  formula <- update.formula(formula, as.formula(paste(tvar, '~ .')))
  dt.gol.u <- dt.gol
  if(dt.gol[[tvar]] %>% is.na %>% sum) { dt.gol.u <- dt.gol2 }
  base.1 <- dt.gol.u[[tvar]] %>% mean
  base.1 <- max(base.1,1-base.1)
  aux <- rpart(formula, data=dt.gol.u, method="class")
  aux %<>% prune(cp=aux$cptable[which.min(aux$cptable[,"xerror"]),"CP"])
  results.dt[[tvar]] <- aux
}
saveRDS(results.dt, "./results/results-dt.rds")
results.dt <- readRDS("./results/results-dt.rds")
#results.dt %<>% as.tibble
#results.dt %<>% mutate(incr=acc-base) %>%
#  arrange(incr)


