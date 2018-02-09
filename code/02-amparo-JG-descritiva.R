library(boot)
library(magrittr)
library(tidyverse)

#Carregar BD
#Em linux data.rds pode ser preferivel.
data <- read.csv("../data/amparo/data.csv")

#Análise descritiva de taxa de acerto.
aux <- data %>% 
  group_by(id_alias_fct) %>%
  do(plots = ggplot(data=., aes(x = move, y = cum_mean))+
       geom_point()+
       ggtitle(unique(.$playeralias)))
pdf("../plots/amparo-cum_mean.pdf")
aux$plots
dev.off()

#Análise descritiva de taxa de tempo até decisão.
aux <- data %>% 
       filter(move != 1) %>%
       group_by(id_alias_fct) %>%
       do(plots = ggplot(data = ., aes(x = move, 
                                       y = movementtime,
                                       colour = acertou_lgl))+
       geom_point()+
       ggtitle(unique(.$playeralias)))
pdf("../plots/amparo-movementtime.pdf")
aux$plots
dev.off()

####################################################
# Análise descritiva das covariáveis obtidas no JG #
####################################################

stan_info <- readRDS("../data/amparo/amparo-JG-stan-data.rds")
estagios <- stan_info$estagios
data %<>% inner_join(estagios, by = "new_playid")

stan_param <- stan_info$param %>%
  mutate(escolaridade = as.factor(escolaridade),
         id           = as.factor(id),
         hy           = as.factor(hy)) %>% 
  as.tibble()

##########################################################
##Curiosidade se os parametros tem distribuicao bimodal ##
##########################################################
#stan_param %>% 
#  ggplot(aes(x = beta_m, fill = id))+
#  geom_density()

#stan_param %>% 
#  ggplot(aes(x = beta_m, y=gamma_m, colour=id))+
#  geom_point()+
#  geom_smooth(method='lm', formula = gamma_m~stan_param$beta_m)

stan_param %>% 
  ggplot(aes(x = id, y = gamma_m, fill = hy))+
  geom_boxplot()
ggsave("../plots/amparo-stan-hy.pdf")
dev.off()

stan_param %>% ggplot(aes(x = id, y = gamma_m, fill = escolaridade))+
               geom_boxplot()
ggsave("../plots/amparo-stan-escol.pdf")
dev.off()

#Curvas de acerto estimadas
#Precisam ser ajustadas para o novo modelo
aux <- data %>%
       group_by(fator) %>%
       do(plots = ggplot(data = ., aes(x = move, y = cum_mean))+
          geom_point()+
          ggtitle(unique(.$playeralias)))
curvas_param <- inner_join(aux, stan_param, by="fator")
final_plot <- function(pp, b, g)
{
  glm_mod <- function(move) g*inv.logit(-log(3*g - 1) + (move-1)*b) #Ajuste
  pp+stat_function(fun=glm_mod)
}
this_list <- list(pp = curvas_param$plots,
                  b  = curvas_param$beta_m,
                  g  = curvas_param$gamma_m)
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
