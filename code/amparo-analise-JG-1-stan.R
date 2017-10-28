library(dplyr)
library(magrittr)
library(rstan)

#Carregar BD
#Em linux data.rds pode ser preferivel.
data <- read.csv("../data/amparo/data.csv")

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
stan.fit <- stan(file="amparo-analise-JG.stan", 
                 data=c("m","n", "e", "acerto", 
                 "fator", "escol", "move"), 
                 iter=1000, chains=1)

pdf("../plots/parametros_amparo_informativa.pdf")
hist(stan.fit$summary[grep("gamma\\[[[:alnum:]]{1,},1\\]",rownames(stan.fit$summary)),1],xlab = expression(theta),main="Fase 1",cex.lab=1.5)
hist(stan.fit$summary[grep("gamma\\[[[:alnum:]]{1,},2\\]",rownames(stan.fit$summary)),1],xlab = expression(theta),main="Fase 2",cex.lab=1.5)
hist(stan.fit$summary[grep("gamma\\[[[:alnum:]]{1,},3\\]",rownames(stan.fit$summary)),1],xlab = expression(theta),main="Fase 3",cex.lab=1.5)

hist(stan.fit$summary[grep("alfa\\[[[:alnum:]]{1,},1\\]",rownames(stan.fit$summary)),1],xlab = expression(alpha),main="Fase 1",cex.lab=1.5)
hist(stan.fit$summary[grep("alfa\\[[[:alnum:]]{1,},2\\]",rownames(stan.fit$summary)),1],xlab = expression(alpha),main="Fase 2",cex.lab=1.5)
hist(stan.fit$summary[grep("alfa\\[[[:alnum:]]{1,},3\\]",rownames(stan.fit$summary)),1],xlab = expression(alpha),main="Fase 3",cex.lab=1.5)

hist(stan.fit$summary[grep("beta\\[[[:alnum:]]{1,},1\\]",rownames(stan.fit$summary)),1],xlab = expression(beta),main="Fase 1",cex.lab=1.5)
hist(stan.fit$summary[grep("beta\\[[[:alnum:]]{1,},2\\]",rownames(stan.fit$summary)),1],xlab = expression(beta),main="Fase 2",cex.lab=1.5)
hist(stan.fit$summary[grep("beta\\[[[:alnum:]]{1,},3\\]",rownames(stan.fit$summary)),1],xlab = expression(beta),main="Fase 3",cex.lab=1.5)
dev.off()

alfa <- stan.fit %>% rstan::extract("alfa")
beta <- stan.fit %>% rstan::extract("beta")
gamma <- stan.fit %>% rstan::extract("gamma")

fator_obs <- fator %>% unique %>% sort
fator_nobs <- which(!(1:m %in% fator_obs))
alfa.m <- (alfa$alfa %>% colMeans)[-fator_nobs]
beta.m <- (beta$beta %>% colMeans)[-fator_nobs]
gamma.m <- (gamma$gamma %>% colMeans)[-fator_nobs]

stan.data <- data_frame(alias=data2$alias,
                        escol=data2$escol,
                        fator=data2$fator,
                        hy=data2$HY_,
                        id=data2$id) %>%
  group_by(fator) %>%
  summarise_all(funs(first(na.omit(.)))) %>%
  arrange(fator) %>%
  cbind(beta.m,gamma.m)

stan.data <- list(estagios_modelados=estagios_modelados,
                  fator_nobs=fator_nobs,
                  param=stan.data)
saveRDS(stan.data, "../clean_data/amparo-JG-stan-data.rds")
