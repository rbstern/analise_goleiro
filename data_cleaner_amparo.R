library(dplyr)
library(magrittr)
library(tibble)

data <- read.csv("../amparo.csv")
dt <- data %>% filter(game == "AQ", total.Correct >= 3)
base <- dt$player.Alias %>% unique 

data %<>% filter(player.Alias %in% base) %>%
  mutate(playeralias=player.Alias, playid=playID) %>%
  mutate(new_playid=paste(game,playid,sep="-")) %>%
  mutate(alias=playeralias %>% as.numeric) %>%
  mutate(id=new_playid %>% as.factor %>% as.numeric) %>%
  mutate(fator=max(id)*(alias-1)+id) %>%
  mutate(id_alias_fct=paste(new_playid,playeralias,sep="")) %>%
  arrange(id_alias_fct, move) %>%
  mutate(acertou_lgl=correct.TRUE.FALSE,
         movementtime=movementTime.s.) %>%
  group_by(id_alias_fct) %>%
  mutate(cum_mean = cumsum(acertou_lgl)/(1:n()))

labels <- read.csv("../labels.csv")
labels2 <- data.frame(playeralias=labels$CÓDIGO, 
                      HY_=labels$HY_,
                      idade=labels$Idade,
                      escolaridade=labels$Escolaridade)
labels2 %<>% mutate(escolaridade=2*grepl("Superior", labels$Escolaridade)+
                                1*grepl("Médio", labels$Escolaridade),
                    HY_=HY_ %>% as.numeric %% 4)

data <- inner_join(data,labels2)
write.csv(data, "../data.csv")
