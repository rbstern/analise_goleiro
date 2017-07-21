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
data=read.csv("../../Data/170619exportAmparoDe170511a170619.csv")
data=data %>% filter(game=="JG") 
data %<>% mutate(id.alias=paste(playid,playeralias,sep=""))


data2 <- data %>% mutate(alias=playeralias %>% as.numeric,
                         id=playid %>% as.numeric) %>%
                  select(alias, correct, id, move) 
#? necess?rio passar algumas informa??es
#explicitamente para o c?digo do stan.
n <- data2$alias %>% max #numero de pacientes. 
t <- data2$id %>% max    #n?mero de tratamentos.
m <- data2 %>% nrow      #numero total de jogadas (linhas).
acerto <- as.numeric(data2$correct=="TRUE")
alias <- data2$alias
id <- data2$id
move <- data2$move

#amostra <- stan(file="amparo.stan", 
#                data=c("n", "t", "m", "acerto", 
#                       "alias", "id", "move"), 
#                iter=10^4, chains=1)



amostra <- stan(file="amparo_informativa.stan", 
                data=c("n", "t", "m", "acerto", 
                       "alias", "id", "move"), 
                iter=10^4, chains=1)
fit_summary <- summary(amostra)

pdf("plots/parametros_amparo_informativa.pdf")
hist(fit_summary$summary[grep("gamma\\[[[:alnum:]]{1,},1\\]",rownames(fit_summary$summary)),1],xlab = expression(theta),main="Fase 1",cex.lab=1.5)
hist(fit_summary$summary[grep("gamma\\[[[:alnum:]]{1,},2\\]",rownames(fit_summary$summary)),1],xlab = expression(theta),main="Fase 2",cex.lab=1.5)
hist(fit_summary$summary[grep("gamma\\[[[:alnum:]]{1,},3\\]",rownames(fit_summary$summary)),1],xlab = expression(theta),main="Fase 3",cex.lab=1.5)

hist(fit_summary$summary[grep("alfa\\[[[:alnum:]]{1,},1\\]",rownames(fit_summary$summary)),1],xlab = expression(alpha),main="Fase 1",cex.lab=1.5)
hist(fit_summary$summary[grep("alfa\\[[[:alnum:]]{1,},2\\]",rownames(fit_summary$summary)),1],xlab = expression(alpha),main="Fase 2",cex.lab=1.5)
hist(fit_summary$summary[grep("alfa\\[[[:alnum:]]{1,},3\\]",rownames(fit_summary$summary)),1],xlab = expression(alpha),main="Fase 3",cex.lab=1.5)

hist(fit_summary$summary[grep("beta\\[[[:alnum:]]{1,},1\\]",rownames(fit_summary$summary)),1],xlab = expression(beta),main="Fase 1",cex.lab=1.5)
hist(fit_summary$summary[grep("beta\\[[[:alnum:]]{1,},2\\]",rownames(fit_summary$summary)),1],xlab = expression(beta),main="Fase 2",cex.lab=1.5)
hist(fit_summary$summary[grep("beta\\[[[:alnum:]]{1,},3\\]",rownames(fit_summary$summary)),1],xlab = expression(beta),main="Fase 3",cex.lab=1.5)
dev.off()

alias.unique=unique(data2$alias)
for(ii in 1:max(data2$alias))
{
  dados.filtrado= data2 %>% filter(alias==alias.unique[ii])
  plot()
}
