library(broom)
library(dplyr)
library(ellipse)
library(ggplot2)
library(magrittr)
library(tibble)

##Analise acertos~tempo por jogador
data <- readRDS("./clean_data/data.R")
data %<>% group_by(id) %>%
          filter(n() > 5)
coefs <- data %>% do(tidy(glm(acerto~tempo, 
                              data=., 
                              family=binomial(link="logit")))) %>%
         group_by(id) %>% 
         select(term, estimate)
coefs <- split(coefs, coefs$term)
coefs$"(Intercept)" %<>% select(id, estimate)
colnames(coefs$"(Intercept)") <- c("id", "alfa")
coefs$"tempo" %<>% select(id, estimate)
colnames(coefs$"tempo") <- c("id", "beta")
coefs <- merge(coefs$"(Intercept)", coefs$"tempo", by="id")
coefs <- merge(data %>% select(id,stage) %>% unique, by="id", coefs) %>%
         as.tibble
saveRDS(coefs, "./clean_data/projecoes.R")

coefs <- readRDS("./clean_data/projecoes.R")
coefs %>% filter(abs(alfa)<20) %>%
  ggplot(aes(alfa,beta,col=stage))+
  geom_point()
ggsave("./plots/scatter-hab-grupo.pdf")

coefs %>% filter(abs(alfa)<20) %>%
          group_by(stage) %>% 
          summarise(alfa=mean(alfa), beta=mean(beta)) %>%
          ggplot(aes(alfa,beta,col=stage))+geom_point()
ggsave("./plots/scatter-mean-hab-grupo.pdf")


##Analise acertos~log(tempo) por jogador
data2 <- data %>% mutate(tempo=log(tempo))
coefs <- data2 %>% do(tidy(glm(acerto~tempo, 
                              data=., 
                              family=binomial(link="logit")))) %>%
                  group_by(id) %>% 
                  select(term, estimate)
coefs <- split(coefs, coefs$term)
coefs$"(Intercept)" %<>% select(id, estimate)
colnames(coefs$"(Intercept)") <- c("id", "alfa")
coefs$"tempo" %<>% select(id, estimate)
colnames(coefs$"tempo") <- c("id", "beta")
coefs <- merge(coefs$"(Intercept)", coefs$"tempo", by="id")
coefs <- merge(data %>% select(id,stage) %>% unique, by="id", coefs) %>%
         as.tibble
saveRDS(coefs, "./clean_data/projecoes2.R")

coefs <- readRDS("./clean_data/projecoes2.R")
coefs %>% filter(abs(alfa)<20) %>%
          ggplot(aes(alfa,beta,col=stage))+
          geom_point()+
          stat_ellipse()
ggsave("./plots/scatter-hab-grupo-2.pdf")

coefs %>% filter(abs(alfa)<20) %>%
  group_by(stage) %>% 
  summarise(alfa=mean(alfa), beta=mean(beta)) %>%
  ggplot(aes(alfa,beta,col=stage))+geom_point()
ggsave("./plots/scatter-mean-hab-grupo-2.pdf")


##Analise acertos~tempo+contexto
data <- readRDS("./clean_data/data.R")
data %>% group_by(stage) %>% 
         do(tidy(glm(acerto~tempo, 
                     data=., 
                     family=binomial(link="logit")))) %>%
         group_by(stage) %>% 
         select(term, estimate)
         