library(boot)
library(broom)
library(ellipse)
library(ggplot2)
library(glmnet)
library(magrittr)
library(mcmc)
library(purrrlyr)
library(randomForest)
library(rpart)
library(rstan)
library(tidyverse)
library(tree)
library(ROCR)


#Carregar BD
#Em linux data.rds pode ser preferivel.
data <- readRDS("../data.rds")
#data <- read.csv("./data/amparo/data.csv")
#labels <- read.csv("./data/amparo/labels.csv")

#An?lise descritiva de taxa de acerto.
#aux <- data %>% 
#  group_by(id_alias_fct) %>%
#  do(plots=ggplot(data=.,aes(x=move,y=cum_mean))+
#       geom_point()+
#       ggtitle(unique(.$playeralias)))
#pdf("./plots/amparo-cum_mean.pdf")
#aux$plots
#dev.off()

#An?lise descritiva de taxa de tempo at? decis?o.
#aux <- data %>% 
#       filter(move != 1) %>%
#       group_by(id_alias_fct) %>%
#       do(plots=ggplot(data=.,aes(x=move,y=movementtime,
#                                  colour=acertou_lgl))+
#       geom_point()+
#       ggtitle(unique(.$playeralias)))
#pdf("./plots/amparo-movementtime.pdf")
#aux$plots
#dev.off()

##Grazie mile, Fernando Correa.
##Elipses de confian?a para par?metros.
#desenha_ellipse <- function(df, ...){
# ok_stages <- coefs %>% group_by(playid) %>%
# summarise(n=n()) %>%
# filter(n>5) %>%
# select(playid) %>%
# unlist
# df %>% filter(playid %in% ok_stages) %>%
# group_by(...) %>%
# by_slice(function(df){ ellipse(x =cov(df)/nrow(df),
#                                centre=colMeans(df),
#                                level = 0.95, 
#                                npoints = 100) %>%
#          as.data.frame()},
#          .collate = 'rows')
#}

##########################################################
## Extra??es de covari?veis a partir do Jogo do Goleiro ##
##########################################################
estagios <- data$new_playid %>% unique
estagios_modelados <- estagios[c(3,4,7)]
data2 <- data %>% filter(new_playid %in% estagios_modelados)

#? necess?rio passar algumas informa??es
#explicitamente para o c?digo do stan.
acerto <- data2$acertou_lgl
alias <- data2$alias
escol <- data2$escolaridade +1
fator <- data2$fator
id <- data2$id
move <- data2$move
e <- escol %>% max               #numero de escolaridades.
p <- alias %>% max               #numero de pacientes. 
f <- id %>% max                  #numero de fases.
m <- p*f                         #numero de combinacoes paciente x fase.
n <- alias %>% length            #numero total de jogadas (linhas).
fator_obs <- fator %>% unique %>% sort
fator_nobs <- which(!(1:m %in% fator_obs))

#amostra <- stan(file="amparo.stan", 
#                data=c("m","n", "e", "acerto", 
#                        "fator", "escol", "move"), 
#                iter=1000, chains=1)
#saveRDS(amostra, "./clean_data/glm-amparo.rds")

#Covariaveis do jogo do goleiro
amostra <- readRDS("./clean_data/glm-amparo.rds")
alfa <- amostra %>% rstan::extract("alfa")
beta <- amostra %>% rstan::extract("beta")
gamma <- amostra %>% rstan::extract("gamma")
alfa.m <- (alfa$alfa %>% colMeans)
beta.m <- (beta$beta %>% colMeans)[-fator_nobs]
gamma.m <- (gamma$gamma %>% colMeans)[-fator_nobs]

####################################
##Analise tempo~move por jogador  ##
##Regress?o Linear                ##
##Poss?vel cov extra?             ##
####################################
#coefs <- data %>% 
#  filter(move != 1) %>%
#  group_by(id.alias) %>%
#  do(tidy(glm(movementtime~move, 
#              data=.))) %>%
#  group_by(id.alias) %>%
#  select(term, estimate)
#coefs <- split(coefs, coefs$term)
#coefs$"(Intercept)" %<>% select(id.alias, estimate)
#colnames(coefs$"(Intercept)") <- c("id.alias", "alfa")
#coefs$"move" <- coefs$"move"
#coefs$"move" %<>% select(id.alias, estimate)
#colnames(coefs$"move") <- c("id.alias", "beta")
#coefs <- merge(coefs$"(Intercept)", coefs$"move", by="id.alias")
#coefs <- merge(data %>% select(id.alias,playid) %>% unique, by="id.alias", coefs) 
#saveRDS(coefs, "./clean_data/projecoes-amparo-2.rds")
#coefs <- readRDS("./clean_data/projecoes-amparo-2.rds")
#data_ellipses <- coefs %>%
#  select(alfa, beta, playid) %>%
#  desenha_ellipse(playid)
#coefs %>% filter(alfa<30,beta<3) %>%
#  ggplot(aes(x=alfa, y=beta, col=playid))+
#  geom_point()+
#  geom_path(data=data_ellipses, 
#            aes(x=alfa, y=beta, col=playid), 
#            inherit.aes = T)
#ggsave("./plots/scatter-hab-grupo-amparo-2.pdf")

##############################################
# An?lise descritiva das covari?veis obtidas #
##############################################



########################################################
#Construir classificadores baseados no jogo do goleiro #
########################################################

param <- data_frame(fator=fator,alias=alias,id=id) %>%
  group_by(fator) %>%
  summarise_all(funs(first(na.omit(.)))) %>%
  arrange(fator) %>%
  cbind(beta.m,gamma.m)
aux1 <- param %>% select(alias, id, beta.m) %>% spread(id, beta.m)
colnames(aux1) <- c(colnames(aux1)[1],
                    "beta." %>% paste(colnames(aux1)[-1], sep=""))
aux2 <- param %>% select(alias, id, gamma.m) %>% spread(id, gamma.m)
colnames(aux2) <- c(colnames(aux2)[1],
                    "gamma." %>% paste(colnames(aux2)[-1], sep=""))

tempos <- data2 %>% filter(move != 1) %>%
  select(fator, alias, id, movementtime) %>%
  group_by(fator) %>%
  summarise(alias=mean(alias), id=mean(id), time.m=mean(movementtime)) %>%
  select(alias, id, time.m)
aux3 <- tempos %>% select(alias, id, time.m) %>% spread(id, time.m)
colnames(aux3) <- c(colnames(aux3)[1],
                    "time." %>% paste(colnames(aux3)[-1], sep=""))

expl <- aux1 %>% inner_join(aux2) %>% inner_join(aux3)
expl_has_na <- (expl %>% is.na %>% rowMeans > 0) %>% which
expl <- expl[-expl_has_na,]

#Variaveis resposta
resp <- data2 %>%
  select(alias,HY_,
         best_lim, best_marcha, best_orient, best_reat, best_rest, best_trans, best_tot,
         moca_abs, moca_ate, moca_evoc, moca_lin, moca_nom, moca_ori, moca_vis, moca_tot,
         updrs_post, updrs_rig, updrs_trem, updrs_tot) %>%
  group_by(alias) %>%
  summarise_all(funs(first(na.omit(.)))) %>%
  arrange(alias)
resp <- resp[-expl_has_na,]
resp$HY_[resp$HY_ %>% is.na %>% which] <- 0

#expl$moca_tot_x <- resp$moca_tot

#excluir saudaveis
#resp_has_na <- (resp %>% is.na %>% rowMeans > 0) %>% which
#resp <- resp[-resp_has_na,]
#expl <- expl[-resp_has_na,]

resp2 <- resp %>% 
  mutate(moca_tot_scale = moca_tot) %>% 
  mutate(best_lim= best_lim >= 18,
         best_marcha= best_marcha >= 17,
         best_orient= best_orient >= 13,
         best_reat= best_reat >= 13,
         best_rest= best_rest >= 13,
         best_trans= best_trans >= 15,
         best_tot= best_tot >= 87.5,
         moca_abs = moca_abs >= 2,
         moca_ate = moca_ate >= 5,
         moca_evoc = moca_evoc >= 4,
         moca_lin = moca_lin >= 2,
         moca_nom = moca_nom >= 3,
         moca_ori = moca_ori >= 6,
         moca_vis = moca_vis >= 4,
         moca_tot = moca_tot >= 26,
         updrs_post = updrs_post >= 1,
         updrs_rig = updrs_rig >= 4,
         updrs_trem = updrs_trem >= 2,
         updrs_tot = updrs_tot >= 18)

#BD final
dt.gol <- inner_join(expl,resp2)[,-1]
dt.gol2 <- dt.gol %>% na.omit
variaveis.resp <- (resp2 %>% names)[-(1:2)]
variaveis.resp <- variaveis.resp[variaveis.resp!="moca_tot_scale"]
variaveis.expl <- (expl %>% names)[-1]

###################
# Classificadores #
###################

#Regress?o log?stica

theme = theme_set(theme_minimal(base_size = 26))
theme = theme_update(legend.position="top")


formula <- paste("moca_abs~",
                 variaveis.expl %>% paste(collapse="+"),sep="")
results <- NULL
data.roc.all <- NULL
### NAO CONSEGUI FAZER A PRIMEIRA, POIS A VARIAVEL NAO FOI DEFINIDA
for(tvar in variaveis.resp[-1])
{
  formula <- update.formula(formula, as.formula(paste(tvar, '~ .')))
  dt.gol.u <- dt.gol
  if(dt.gol[[tvar]] %>% is.na %>% sum) { dt.gol.u <- dt.gol2 }
  base.1 <- dt.gol.u[[tvar]] %>% mean
  base.1 <- max(base.1,1-base.1)
  aux <- glm(formula, family=binomial, data=dt.gol.u)
  cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)
  err <- (cv.glm(dt.gol.u, aux,cost)$delta)[1]
  
  pos <- (dt.gol.u[[tvar]] == 1) %>% which
  
  # leave-one-out and 11-fold cross-validation prediction error for 
  # the nodal data set.  Since the response is a binary variable an
  # appropriate cost function is
  
  
  k <- nrow(dt.gol.u)
  predictions <- rep(NA,k)
  for (i in 1:k) {
    model <- glm(formula, family=binomial, data=dt.gol.u[-i,])
    predictions[i] <-  predict(model, newdata=dt.gol.u[i,,drop=F])
  }
  
  
  preds <- (predictions >= 0)
  
  
  roc <- prediction(predictions, dt.gol.u[[tvar]])
  roc <- performance(roc, measure = "tpr", x.measure = "fpr") 
  data.roc <- data.frame("espec"=roc@x.values[[1]],
                         "sens"=roc@y.values[[1]],"variable"=tvar,
                         "method"="GLM")
  data.roc.all <- rbind(data.roc.all,data.roc)
  
  
  sens <- mean(preds[pos]==1)
  spec <- mean(preds[-pos]==0)
  
  results <- rbind(results,
                   data.frame(var=tvar,
                              base=base.1,
                              acc=1-err,
                              method="binomial",
                              sens=sens,
                              spec=spec))
}
ggplot(data=data.roc.all)+geom_line(aes(x=espec,y=sens),size=1.2)+
  xlab("1-Especificidade")+ylab("Sensibilidade")+
  geom_abline(color="red",size=1.2)+facet_wrap( ~ variable, ncol=4)

results %<>% as.tibble
results %<>% mutate(incr=acc-base) %>%
  arrange(incr)
results
#saveRDS(results, "./results/JG-predict.rds")
#results <- readRDS("./results/JG-predict.rds")

# Regress?o logistica usando glmnet
results <- NULL
for(tvar in variaveis.resp[-1])
{
  dt.gol.u <- dt.gol
  if(dt.gol[[tvar]] %>% is.na %>% sum) { dt.gol.u <- dt.gol2 }
  base.1 <- dt.gol.u[[tvar]] %>% mean
  base.1 <- max(base.1,1-base.1)
  x= dt.gol.u %>% select(variaveis.expl) %>% as.matrix
  y= dt.gol.u %>% select(tvar) %>% as.matrix %>% as.factor
  aux <- cv.glmnet(x,y,family="binomial",
                   keep = TRUE,nfolds = nrow(dt.gol.u))
  i <- which(aux$lambda == aux$lambda.min)
  # coeficientes:
  coefficients(aux,s=aux$lambda.min)
  pos <- (dt.gol.u[[tvar]] == 1) %>% which
  
  preds <- aux$fit.preval[,i]>1/2
  mse.min <- mean(preds!=dt.gol.u[[tvar]])
  
  roc <- prediction(aux$fit.preval[,i], dt.gol.u[[tvar]])
  roc <- performance(roc, measure = "tpr", x.measure = "fpr") 
  data.roc <- data.frame("espec"=roc@x.values[[1]],
                         "sens"=roc@y.values[[1]],"variable"=tvar,
                         "method"="GLMNET")
  data.roc.all <- rbind(data.roc.all,data.roc)
  
  
  sens <- mean(preds[pos]==1)
  spec <- mean(preds[-pos]==0)
  results <- rbind(results,
                   data.frame(var=tvar,
                              base=base.1,
                              acc=1-mse.min))
}
results %<>% as.tibble
results %<>% mutate(incr=acc-base) %>%
  arrange(incr)
results

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
ggsave("roc.pdf",height = 17,width = 14)