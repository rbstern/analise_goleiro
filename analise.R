library(dplyr)
library(ggplot2)

data <- readRDS("./clean_data/data.R")
stages <- data[,1] %>% unique
projections <- NULL
for(stage in stages)
{
 s_data <- data %>% filter(stages==stage)
 s_data <- s_data[,-1]
 max.seq <- dim(s_data)[2]/2
 coefs <- matrix(NA, nrow=nrow(s_data), ncol=2)
 for(ii in 1:nrow(s_data))
 {
  p_data <- s_data[ii,]
  x <- p_data[,1:max.seq] %>% as.numeric %>% log
  y <- p_data[,-(1:max.seq)] %>% as.numeric
  if(sum(!is.na(y)) < 10)
  {
   coefs[ii, ] <- c(NA, NA)
  }
  else
  {
   coefs[ii,] <- glm(y~x, family=binomial(link="logit"))$coefficients
  }
 }
 
 coefs <- data.frame(rep(stage, nrow(coefs)), coefs)
 projections <- rbind(projections, coefs)
}
names(projections) <- c("stages", "alfa", "beta")
saveRDS(projections, "./clean_data/projecoes.R")

proj <- readRDS("./clean_data/projecoes.R")
proj[abs(proj[,2]) < 20,] %>% ggplot(aes(alfa,beta,col=stages))+
                              geom_point()
ggsave("./plots/scatter-hab-grupo.pdf")

means <- proj[abs(proj[,2]) < 20,] %>% group_by(stages) %>% 
  summarise(alfa=mean(alfa), beta=mean(beta))
means %>% ggplot(aes(alfa,beta,col=stages))+geom_point()
ggsave("./plots/scatter-mean-hab-grupo.pdf")
