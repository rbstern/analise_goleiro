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

#Carregar BD
#Em linux data.rds pode ser preferivel.
data <- read.csv("./data/amparo/data.csv")
#labels <- read.csv("./data/amparo/labels.csv")
  
#Análise descritiva de taxa de acerto.
#aux <- data %>% 
#  group_by(id_alias_fct) %>%
#  do(plots=ggplot(data=.,aes(x=move,y=cum_mean))+
#       geom_point()+
#       ggtitle(unique(.$playeralias)))
#pdf("./plots/amparo-cum_mean.pdf")
#aux$plots
#dev.off()

#Análise descritiva de taxa de tempo até decisão.
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
##Elipses de confiança para parâmetros.
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
## Extrações de covariáveis a partir do Jogo do Goleiro ##
##########################################################
estagios <- data$new_playid %>% unique
estagios_modelados <- estagios[c(3,4,7)]
data2 <- data %>% filter(new_playid %in% estagios_modelados)

#É necessário passar algumas informações
#explicitamente para o código do stan.
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
##Regressão Linear                ##
##Possível cov extra?             ##
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
# Análise descritiva das covariáveis obtidas #
##############################################

#Desempenho no JG por escolaridade e HY.
player_idx <- data$playeralias %>% unique %>% match(data$playeralias)
players <- data$playeralias[player_idx] %>% as.numeric
players_sort <- players %>% sort %>% match(players)
players_states <- data$HY_[player_idx]
players_states <- players_states[players_sort]
players_escol <- data$escolaridade[player_idx]
players_escol <- players_escol[players_sort]
hy_ <- players_states %>% rep(each=f) %>% as.factor
escol <- players_escol %>% rep(each=f) %>% as.character
bloco <- data$playid %>% unique %>% sort %>% rep(p)
param <- data.frame(beta.m=beta.m,
                    gamma.m=gamma.m,
                    hy_=hy_,
                    escol=escol,
                    bloco=bloco)

#param %>% ggplot(aes(x=bloco, y=gamma.m, fill=hy_))+
#  geom_boxplot()
#ggsave("./plots/logit-amparo-hy-f.pdf")

#param %>% ggplot(aes(x=bloco, y=gamma.m, fill=escol))+
#  geom_boxplot()
#ggsave("./plots/logit-amparo-escol-f.pdf")

#Curvas de acerto estimadas
aux <- data %>%
  group_by(fator) %>%
  do(plots=ggplot(data=.,aes(x=move,y=cum_mean))+
       geom_point()+
       ggtitle(unique(.$playeralias)))
curvas_param <- inner_join(aux, param)
final_plot <- function(pp, b, g)
{
  glm_mod <- function(move) g*inv.logit(-log(3*g-1)+move*b)
  pp+stat_function(fun=glm_mod)
}
this_list <- list(pp=curvas_param$plots,
                  b=curvas_param$beta.m,
                  g=curvas_param$gamma.m)
aux2 <- pmap(this_list, final_plot)
#pdf("./plots/amparo-cum_mean-glm.pdf")
#aux2
#dev.off()

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

resp2 <- resp %>% mutate(best_lim= best_lim >= 18,
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
variaveis.expl <- (expl %>% names)[-1]

###################
# Classificadores #
###################

#Regressão logística
#Izbicki
formula <- paste("moca_abs~",
                 variaveis.expl %>% paste(collapse="+"),sep="")
results <- NULL
for(tvar in variaveis.resp)
{
  formula <- update.formula(formula, as.formula(paste(tvar, '~ .')))
  dt.gol.u <- dt.gol
  if(dt.gol[[tvar]] %>% is.na %>% sum) { dt.gol.u <- dt.gol2 }
  base.1 <- dt.gol.u[[tvar]] %>% mean
  base.1 <- max(base.1,1-base.1)
  aux <- glm(formula, family=binomial, data=dt.gol.u)
  err <- (cv.glm(dt.gol.u, aux)$delta)[1]
  #Izbicki ROC
  pos <- (dt.gol.u[[tvar]] == 1) %>% which
  preds <- (predict(aux, dt.gol.u) >= 0)
  sens <- mean(preds[pos]==1)
  spec <- mean(preds[-pos]==0)
  #Izbicki ROC
  results <- rbind(results,
                   data.frame(var=tvar,
                              base=base.1,
                              acc=1-err,
                              method="binomial",
                              sens=sens,
                              spec=spec))
}
results %<>% as.tibble
results %<>% mutate(incr=acc-base) %>%
             arrange(incr)
results
#saveRDS(results, "./results/JG-predict.rds")
#results <- readRDS("./results/JG-predict.rds")

# Regressão logistica usando glmnet
results <- NULL
for(tvar in variaveis.resp)
{
  dt.gol.u <- dt.gol
  if(dt.gol[[tvar]] %>% is.na %>% sum) { dt.gol.u <- dt.gol2 }
  base.1 <- dt.gol.u[[tvar]] %>% mean
  base.1 <- max(base.1,1-base.1)
  x= dt.gol.u %>% select(variaveis.expl) %>% as.matrix
  y= dt.gol.u %>% select(tvar) %>% as.matrix %>% as.factor
  aux <- cv.glmnet(x,y,family="binomial",type.measure="class")
  i <- which(aux$lambda == aux$lambda.min)
  mse.min <- aux$cvm[i]
  pos <- (dt.gol.u[[tvar]] == 1) %>% which
  preds <- (predict(aux, dt.gol.u) >= 0)
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
#saveRDS(results, "./results/JG-predict.rds")
#results <- readRDS("./results/JG-predict.rds")

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

#######################################
#Classificadores baseados em moca_tot #
#######################################
expl2 <- data.frame(alias=resp$alias, moca_tot_x=resp$moca_tot)
dt.gol <- inner_join(expl2,resp2)[,-1]
dt.gol2 <- dt.gol %>% na.omit
variaveis.resp <- (resp2 %>% names)[-(1:2)]
variaveis.expl <- (expl2 %>% names)[-1]

#Classificadores
formula <- paste("moca_abs~",
                 variaveis.expl %>% paste(collapse="+"),sep="") %>%
           as.formula
results2 <- NULL
for(tvar in variaveis.resp)
{
  formula <- update.formula(formula, as.formula(paste(tvar, '~ .')))
  dt.gol.u <- dt.gol
  if(dt.gol[[tvar]] %>% is.na %>% sum) { dt.gol.u <- dt.gol2 }
  base.1 <- dt.gol.u[[tvar]] %>% mean
  base.1 <- max(base.1,1-base.1)
  aux <- glm(formula, family=binomial, data=dt.gol.u)
  err <- (cv.glm(dt.gol.u, aux)$delta)[1]
  
  pos <- (dt.gol.u[[tvar]] == 1) %>% which
  preds <- (predict(aux, dt.gol.u) >= 0)
  sens <- mean(preds[pos]==1)
  spec <- mean(preds[-pos]==0)
  
  results2 <- rbind(results2,
                    data.frame(var=tvar,
                               base=base.1,
                               acc=1-err,
                               method="binomial",
                               sens=sens,
                               spec=spec))
}
results2 %<>% as.tibble
results2 %<>% mutate(incr=acc-base) %>%
              arrange(incr)
#saveRDS(results2, "./results/MOCA-predict.rds")
#results2 <- readRDS("./results/MOCA-predict.rds")

#Diferenças entre classificadores
results %<>% arrange(var)
results2 %<>% arrange(var)
comp <- data.frame(var=results$var, 
                   diff=(results$acc-results2$acc) %>% round(2))
comp %<>% arrange(diff)
#saveRDS(comp, "./results/comp-JG-MOCA-predict.rds")
#comp <- readRDS("./results/comp-JG-MOCA-predict.rds")

#começando glmnet se algum dia sobrar tempo para isso...
cvfit = cv.glmnet(expl[,-1] %>% as.matrix, 
                  dt.gol[["moca_tot"]],
                  family = "binomial",
                  type.measure = "class")
