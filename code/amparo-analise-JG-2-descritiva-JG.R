library(magrittr)
library(tidyverse)

#Carregar BD
#Em linux data.rds pode ser preferivel.
data <- read.csv("../data/amparo/data.csv")

#Análise descritiva de taxa de acerto.
aux <- data %>% 
  group_by(id_alias_fct) %>%
  do(plots=ggplot(data=.,aes(x=move,y=cum_mean))+
       geom_point()+
       ggtitle(unique(.$playeralias)))
pdf("../plots/amparo-cum_mean.pdf")
aux$plots
dev.off()

#Análise descritiva de taxa de tempo até decisão.
aux <- data %>% 
       filter(move != 1) %>%
       group_by(id_alias_fct) %>%
       do(plots=ggplot(data=.,aes(x=move,y=movementtime,
                                  colour=acertou_lgl))+
       geom_point()+
       ggtitle(unique(.$playeralias)))
pdf("../plots/amparo-movementtime.pdf")
aux$plots
dev.off()

####################################################
# Análise descritiva das covariáveis obtidas no JG #
####################################################

stan.data <- readRDS("../clean_data/amparo-JG-stan-data.rds")
estagios_modelados <- stan.data$estagios_modelados
data2 <- data %>% filter(new_playid %in% estagios_modelados)
fator_nobs <- stan.data$fator_nobs
stan.param <- stan.data$param

stan.param %<>% mutate(escol=as.factor(escol),
                       hy=ifelse(is.na(hy), 0, hy) %>% as.factor,
                       id=as.factor(id))

stan.param %>% ggplot(aes(x=id, y=gamma.m, fill=hy))+
               geom_boxplot()
ggsave("../plots/amparo-stan-hy.pdf")
dev.off()

stan.param %>% ggplot(aes(x=id, y=gamma.m, fill=escol))+
               geom_boxplot()
ggsave("../plots/amparo-stan-escol.pdf")
dev.off()

#Curvas de acerto estimadas
#Precisam ser ajustadas para o novo modelo
aux <- data2 %>%
       group_by(fator) %>%
       do(plots=ggplot(data=.,aes(x=move,y=cum_mean))+
          geom_point()+
          ggtitle(unique(.$playeralias)))
curvas_param <- inner_join(aux, stan.param)
final_plot <- function(pp, b, g)
{
  glm_mod <- function(move) g*inv.logit(-log(3*g-1)+move*b) #Ajuste
  pp+stat_function(fun=glm_mod)
}
this_list <- list(pp=curvas_param$plots,
                  b=curvas_param$beta.m,
                  g=curvas_param$gamma.m)
aux2 <- pmap(this_list, final_plot)
pdf("../plots/amparo-cum_mean-stan.pdf")
aux2
dev.off()

###################################
# Ainda não finalizado            #
###################################
##Analise tempo~move por jogador  #
##Regressão Linear                #
##Possível cov extra?             #
###################################
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
