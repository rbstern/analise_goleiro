library(magrittr)
library(tidyverse)

data <- read.csv("../data/amparo/amparo.csv")
dt <- data %>% filter(game == "AQ", total.Correct >= 3)
base <- dt$player.Alias %>% unique 

data %<>% filter(player.Alias %in% base) %>%
  mutate(playeralias=player.Alias %>% as.factor, playid=playID) %>%
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

labels <- read.csv("../data/amparo/labels.csv")
labels %<>% filter(labels$COD_ %in% base)
             
labels2 <- data.frame(playeralias=labels$COD_ %>% as.factor,
                      idade=labels$IDAD_,
                      sexo=labels$SEXO_,
                      HY_=labels$HY_,
                      escolaridade=labels$ESCOL_,
                      moca_abs= labels$MoCA_abstração,
                      moca_ate= labels$MoCA_atenção,
                      moca_evoc= labels$MoCA_evoc_tardia,
                      moca_lin= labels$MoCA_linguagem,
                      moca_nom= labels$MoCA_nomeação,
                      moca_ori= labels$MoCA_orientação,
                      moca_vis= labels$MoCA_visuoesp_exec,
                      moca_tot= labels$MoCA_TOTAL,
                      best_lim= labels$BEST_Lim_Estab_Vert_ %>% as.numeric,
                      best_marcha= labels$BEST_Estab_Marcha_ %>% as.numeric,
                      best_orient= labels$BEST_Orient_Sens_,
                      best_reat= labels$BEST_Reat_,
                      best_rest= labels$BEST_Rest_Biom_ %>% as.numeric,
                      best_trans= labels$BEST_Trans_Antec_ %>% as.numeric,
                      best_tot= labels$BEST_ %>% as.numeric,
                      updrs_post= labels$Est_Post_UPDRS_III %>% as.numeric ,
                      updrs_rig= labels$Rigidez_UPDRS_III %>% as.numeric ,
                      updrs_trem= labels$Tremor_UPDRS_III %>% as.numeric ,
                      updrs_tot= labels$UPDRS_III %>% as.numeric)

labels2 %<>% mutate(escolaridade=2*grepl("Superior", labels$ESCOL_)+
                                1*grepl("Médio", labels$ESCOL_),
                    HY_=HY_ %>% as.numeric %% 4)

data <- inner_join(data,labels2)
write.csv(data, "../data/amparo/data.csv")
#não rodar por sigilo do DB
#saveRDS(data, "../data/amparo/data.rds")
