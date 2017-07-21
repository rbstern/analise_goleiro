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
data <- read.csv("../amparo.csv")
data %<>% filter(game=="JG") %>%
          mutate(id.alias=paste(playid,playeralias,sep=""))

##Analise acertos~tempo por jogador
##Regressão Logística
coefs <- data %>% group_by(id.alias) %>%
                  do(tidy(glm(correct~move, 
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

##############################################
## modificações na logística (em andamento) ##
##############################################
data2 <- data %>% mutate(acerto=(correct=="TRUE") %>% as.numeric,
                         alias=playeralias %>% as.numeric,
                         id=playid %>% as.numeric) %>%
                  select(acerto, alias, id, move) 
#É necessário passar algumas informações
#explicitamente para o código do stan.
p <- data2$alias %>% max #numero de pacientes. 
f <- data2$id %>% max    #numero de fases.
m <- p*f                 #numero de combinacoes paciente x fase.
n <- data2 %>% nrow      #numero total de jogadas (linhas).
acerto <- data2$acerto
alias <- data2$alias
id <- data2$id
fator <- t*(alias-1)+id
move <- data2$move

amostra <- stan(file="amparo.stan", 
                data=c("m","n", "acerto", "fator", "move"), 
                iter=1000, chains=1)
saveRDS(amostra, "./clean_data/glm-amparo.rds")

amostra <- readRDS("./clean_data/glm-amparo.rds")
beta <- amostra %>% extract("beta")
gamma <- amostra %>% extract("gamma")
beta.m <- beta$beta %>% colMeans
gamma.m <- gamma$gamma %>% colMeans
cols <- nivel %% 4
cbind(beta.m,gamma.m) %>% plot(col=cols)
