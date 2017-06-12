library(broom)
library(dplyr)
library(ellipse)
library(ggplot2)
library(magrittr)
library(purrr)
library(purrrlyr)
library(tibble)
##Analise acertos~tempo por jogador
data <- readRDS("./clean_data/data.rds")
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
saveRDS(coefs, "./clean_data/projecoes.rds")

coefs <- readRDS("./clean_data/projecoes.rds")
desenha_ellipse <- function(df, ...){
  ok_stages <- coefs %>% group_by(stage) %>% 
                         summarise(n=n()) %>% 
                         filter(n>5) %>% 
                         select(stage) %>% 
                         unlist
  df %>% filter(stage %in% ok_stages) %>%
         group_by(...) %>% 
         by_slice(function(df){ ellipse(x = cov(df)/nrow(df),
                                centre=colMeans(df),
                                level = 0.95, 
                                npoints = 100) %>% 
                                as.data.frame()}, 
                  .collate = 'rows')
}
data_ellipses <- coefs %>% 
                 filter(abs(alfa)<20) %>%
                 select(alfa, beta, stage) %>%
                 desenha_ellipse(stage)

coefs %>% filter(abs(alfa)<20) %>%
          ggplot(aes(x=alfa, y=beta, col=stage))+
          geom_point()+
          geom_path(data=data_ellipses, 
                    aes(x=alfa, y=beta, col=stage), 
                    inherit.aes = T)
ggsave("./plots/scatter-hab-grupo.pdf")

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
saveRDS(coefs, "./clean_data/projecoes2.rds")

coefs <- readRDS("./clean_data/projecoes2.rds")
data_ellipses <- coefs %>% 
  filter(abs(alfa)<20) %>%
  select(alfa, beta, stage) %>%
  desenha_ellipse(stage)

coefs %>% filter(abs(alfa)<20) %>%
  ggplot(aes(x=alfa, y=beta, col=stage))+
  geom_point()+
  geom_path(data=data_ellipses, 
            aes(x=alfa, y=beta, col=stage), 
            inherit.aes = T)
ggsave("./plots/scatter-hab-grupo-2.pdf")

##Analise acertos~tempo+contexto
data <- readRDS("./clean_data/data.rds")
data %>% group_by(stage) %>% 
         do(tidy(glm(acerto~tempo, 
                       data=., 
                     family=binomial(link="logit")))) %>%
         group_by(stage) %>% 
         select(term, estimate)
