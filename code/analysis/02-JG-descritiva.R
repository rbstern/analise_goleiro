library(boot)
library(magrittr)
library(tidyverse)
source("./libs/lib_fnames.R")

data <- fname_data("data.rds") %>% 
  read_rds()

#Analise descritiva de taxa de acerto.
aux <- data %>% 
  group_by(id_alias_fct) %>%
  do(plots = ggplot(data=., aes(x = move, y = cum_mean))+
       geom_point()+
       ggtitle(unique(.$playeralias)))
pdf(fname_plot("correct_cum_mean.pdf"))
aux$plots
dev.off()

#Analise descritiva de taxa de tempo ate decisao.
aux <- data %>% 
       filter(move != 1) %>%
       group_by(id_alias_fct) %>%
       do(plots = ggplot(data = ., aes(x = move, 
                                       y = movement_time,
                                       colour = acertou_lgl))+
       geom_point()+
       ggtitle(unique(.$playeralias)))
pdf(fname_plot("movement_time.pdf"))
aux$plots
dev.off()

####################################################
# Analise descritiva das covariaveis obtidas no JG #
####################################################

stan_info <- fname_data("stan_data.rds") %>% 
  read_rds()
estagios <- stan_info$estagios
data %<>% inner_join(estagios, by = "new_playid")

stan_param <- stan_info$param %>%
  mutate(escolaridade = as.factor(escolaridade),
         id           = as.factor(id),
         hy           = as.factor(hy)) %>% 
  as_tibble()

##########################################################
##Curiosidade se os parametros tem distribuicao bimodal ##
##########################################################
#stan_param %>% 
#  ggplot(aes(x = beta_m, fill = id))+
#  geom_density()

#stan_param %>% 
#  ggplot(aes(x = beta_m, y = gamma_m, colour = id)) +
#  geom_point()

stan_param %>% 
  ggplot(aes(x = id, y = beta_m, fill = hy))+
  geom_boxplot()
ggsave(fname_plot("stan_beta_hy.pdf"))
dev.off()

stan_param %>% 
  ggplot(aes(x = id, y = gamma_m, fill = hy))+
  geom_boxplot()
ggsave(fname_plot("stan_gamma_hy.pdf"))
dev.off()

stan_param %>% ggplot(aes(x = id, 
                          y = beta_m, 
                          fill = escolaridade))+
  geom_boxplot()
ggsave(fname_plot("stan_beta_escol.pdf"))
dev.off()

stan_param %>% ggplot(aes(x = id, 
                          y = gamma_m, 
                          fill = escolaridade))+
               geom_boxplot()
ggsave(fname_plot("stan_gamma_escol.pdf"))
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
pdf(fname_plot("correct_cum_mean_stan.pdf"))
aux2
dev.off()

###################################
# Ainda n?o finalizado            #
###################################
##Analise tempo~move por jogador  #
##Regress?o Linear                #
##Poss?vel cov extra?             #
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
