library(boot)
library(broom)
library(dplyr)
library(ellipse)
library(ggplot2)
library(magrittr)
library(mcmc)
library(purrr)
library(purrrlyr)
library(rstan)
library(tibble)

#Carregar BD
data <- read.csv("../data.csv")
labels <- read.csv("../labels.csv")
  
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
desenha_ellipse <- function(df, ...){
 ok_stages <- coefs %>% group_by(playid) %>%
 summarise(n=n()) %>%
 filter(n>5) %>%
 select(playid) %>%
 unlist
 df %>% filter(playid %in% ok_stages) %>%
 group_by(...) %>%
 by_slice(function(df){ ellipse(x =cov(df)/nrow(df),
                                centre=colMeans(df),
                                level = 0.95, 
                                npoints = 100) %>%
          as.data.frame()},
          .collate = 'rows')
}

######################################
##Analise acertos~move por jogador  ##
##Regressão Logística               ##
##Deprecated.                       ##
######################################
#coefs <- data %>% group_by(id_alias_fct) %>%
#                  do(tidy(glm(acertou_lgl~move,
#                              data=.,
#                              family=binomial(link="logit")))) %>%
#                  group_by(id_alias_fct) %>% 
#                  select(term, estimate)
#coefs <- split(coefs, coefs$term)
#coefs$"(Intercept)" %<>% select(id_alias_fct, estimate)
#colnames(coefs$"(Intercept)") <- c("id_alias_fct", "alfa")
#coefs$"move" <- coefs$"move"
#coefs$"move" %<>% select(id_alias_fct, estimate)
#colnames(coefs$"move") <- c("id_alias_fct", "beta")
#coefs <- merge(coefs$"(Intercept)", coefs$"move", by="id_alias_fct")
#coefs <- merge(data %>% select(id_alias_fct,playid) %>% unique, 
#               by="id_alias_fct", coefs) 
#saveRDS(coefs, "./clean_data/projecoes-amparo.rds")

#coefs <- readRDS("./clean_data/projecoes-amparo.rds")
#data_ellipses <- coefs %>% 
#                 filter(abs(alfa)<20) %>%
#                 select(alfa, beta, playid) %>%
#                 desenha_ellipse(playid)

#coefs %>% filter(abs(alfa)<5) %>%
#          ggplot(aes(x=alfa, y=beta, col=playid))+
#          geom_point()+
#          geom_path(data=data_ellipses, 
#                    aes(x=alfa, y=beta, col=playid), 
#                    inherit.aes = T)
#ggsave("./plots/scatter-hab-grupo-amparo.pdf")

##############################################
## modificações na logística (em andamento) ##
##############################################
estagios <- data$new_playid %>% unique
estagios_modelados <- estagios[c(1,4,7)]
data2 <- data %>% filter(new_playid %in% estagios_modelados)

#É necessário passar algumas informações
#explicitamente para o código do stan.
acerto <- data2$acertou_lgl
alias <- data2$alias
fator <- data2$fator
id <- data2$id
move <- data2$move
movetime <- data2$movementtime
movetime[movetime %>% is.na %>% which] <- median(movetime, na.rm=TRUE)
p <- alias %>% max    #numero de pacientes. 
f <- id %>% max       #numero de fases.
m <- p*f              #numero de combinacoes paciente x fase.
n <- alias %>% length #numero total de jogadas (linhas).

amostra <- stan(file="amparo.stan", 
                data=c("m","n", "acerto", "fator", "move", "movetime"), 
                iter=1000, chains=1)
saveRDS(amostra, "./clean_data/glm-amparo.rds")

amostra <- readRDS("./clean_data/glm-amparo.rds")
beta <- amostra %>% extract("beta")
gamma <- amostra %>% extract("gamma")
beta.m <- beta$beta %>% colMeans
gamma.m <- gamma$gamma %>% colMeans

fatores <- 1:m
ids <- fatores %% f
aliass <- (fatores-1) %/% f +1

fatores_obs <- fatores %>% unique %>% sort
param <- data_frame(fator=fatores_obs,
                    beta=beta.m[fatores_obs], 
                    gamma=gamma.m[fatores_obs])

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
                  b=curvas_param$beta,
                  g=curvas_param$gamma)
aux2 <- pmap(this_list, final_plot)

 
pdf("./plots/amparo-cum_mean-glm.pdf")
aux$plots
dev.off()

player_idx <- data$playeralias %>% unique %>% match(data$playeralias)
players <- data$playeralias[player_idx] %>% as.numeric
players_sort <- players %>% sort %>% match(players)
players_states <- data$HY_[player_idx]
players_states <- players_states[players_sort]
players_escol <- data$escolaridade[player_idx]
players_escol <- players_escol[players_sort]

hy_ <- players_states %>% rep(each=f) %>% as.factor
escol <- players_escol %>% rep(each=f) %>% as.character
escol[escol=="0"] <- "Fundamental"
escol[escol=="1"] <- "Médio"
escol[escol=="2"] <- "Superior"

bloco <- data$playid %>% unique %>% sort %>% rep(p) 
param <- data.frame(beta.m=beta.m,
                    gamma.m=gamma.m,
                    hy_=hy_,
                    escol=escol,
                    bloco=bloco)

param %>% ggplot(aes(x=bloco, y=gamma.m, fill=hy_))+
          geom_boxplot()
ggsave("./plots/logit-amparo-hy-f.pdf")

param %>% ggplot(aes(x=bloco, y=gamma.m, fill=escol))+
          geom_boxplot()
ggsave("./plots/logit-amparo-escol-f.pdf")

##Amparo probito

amostra <- stan(file="amparo-probito.stan", 
                data=c("m","n", "acerto", "fator", "move"), 
                iter=1000, chains=1)
saveRDS(amostra, "./clean_data/glm-amparo-probito.rds")

##Analise tempo~move por jogador
##Regressão Linear
coefs <- data %>% 
         filter(move != 1) %>%
         group_by(id.alias) %>%
         do(tidy(glm(movementtime~move, 
                    data=.))) %>%
         group_by(id.alias) %>%
         select(term, estimate)
coefs <- split(coefs, coefs$term)
coefs$"(Intercept)" %<>% select(id.alias, estimate)
colnames(coefs$"(Intercept)") <- c("id.alias", "alfa")
coefs$"move" <- coefs$"move"
coefs$"move" %<>% select(id.alias, estimate)
colnames(coefs$"move") <- c("id.alias", "beta")
coefs <- merge(coefs$"(Intercept)", coefs$"move", by="id.alias")
coefs <- merge(data %>% select(id.alias,playid) %>% unique, by="id.alias", coefs) 
saveRDS(coefs, "./clean_data/projecoes-amparo-2.rds")

coefs <- readRDS("./clean_data/projecoes-amparo-2.rds")

data_ellipses <- coefs %>%
  select(alfa, beta, playid) %>%
  desenha_ellipse(playid)

coefs %>% filter(alfa<30,beta<3) %>%
 ggplot(aes(x=alfa, y=beta, col=playid))+
 geom_point()+
 geom_path(data=data_ellipses, 
           aes(x=alfa, y=beta, col=playid), 
           inherit.aes = T)
ggsave("./plots/scatter-hab-grupo-amparo-2.pdf")
