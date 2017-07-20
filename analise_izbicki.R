library(dplyr)

dados=read.csv("../../Data/170619exportAmparoDe170511a170619.csv")

dados=dados %>% filter(game=="JG") 
dados$correct=ifelse(dados$correct=="TRUE",1,0)

binomial_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
}

aux=dados %>% group_by(playeralias) %>% 
  do(plots=ggplot(data=.,aes(x=move,y=correct))+
       geom_point()+
       facet_wrap(~ playid)+binomial_smooth()+ 
       ggtitle(unique(.$playeralias))
)

library(ggplot2) 
theme = theme_set(theme_minimal(base_size = 26))
theme = theme_update(legend.position="top", legend.title=element_blank(),panel.grid.major.x=element_blank())

pdf("plots/individual.profiles.pdf")
  aux$plots
dev.off()

