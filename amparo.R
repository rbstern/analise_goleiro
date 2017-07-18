library(broom)
library(dplyr)
library(ellipse)
library(ggplot2)
library(magrittr)
library(purrr)
library(purrrlyr)
library(tibble)

##Analise acertos~tempo por jogador
data <- read.csv("../amparo.csv")
data %<>% mutate(id.alias=paste(playid,playeralias,sep="")) %>%
          group_by(id.alias)
coefs <- data %>% do(tidy(glm(correct~move, 
                              data=., 
                              family=binomial(link="logit")))) %>%
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
saveRDS(coefs, "./clean_data/projecoes-amparo.rds")

coefs <- readRDS("./clean_data/projecoes-amparo.rds")
desenha_ellipse <- function(df, ...){
  ok_stages <- coefs %>% group_by(playid) %>% 
                         summarise(n=n()) %>% 
                         filter(n>5) %>% 
                         select(playid) %>% 
                         unlist
  df %>% filter(playid %in% ok_stages) %>%
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
                 select(alfa, beta, playid) %>%
                 desenha_ellipse(playid)

coefs %>% filter(abs(alfa)<5) %>%
          ggplot(aes(x=alfa, y=beta, col=playid))+
          geom_point()+
          geom_path(data=data_ellipses, 
                    aes(x=alfa, y=beta, col=playid), 
                    inherit.aes = T)
ggsave("./plots/scatter-hab-grupo-amparo.pdf")

