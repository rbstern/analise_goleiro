library(abjutils)
library(janitor)
library(magrittr)
library(tidyverse)

rm_accent_from_names <- function(dt)
{
  dt %<>% set_names(rm_accent(names(.)))
  invisible(dt)
}

data <- read.csv("./data-raw/amparo/amparo.csv") %>%
  as.tibble() %>%
  rm_accent_from_names() %>%
  clean_names()

players <- data %>%
  filter(game == "AQ", total_correct >= 3) %>%
  select(player_alias) %>%
  distinct()

data %<>% 
  inner_join(players, by="player_alias") %>%
  mutate(playeralias  = as.factor(player_alias), 
         playid       = playid,
         new_playid   = paste(game, playid, sep="-"),
         alias        = as.numeric(playeralias),
         id           = new_playid %>% as.factor() %>% as.numeric(),
         fator        = max(id)*(alias-1) + id,
         id_alias_fct = as.factor(paste(new_playid, playeralias, sep = "")),
         acertou_lgl  = correct_true_false %>% as.numeric() %>% as.logical(),
         movementtime = as.numeric(movementtime_s),
         move         = as.numeric(move)) %>%
  arrange(id_alias_fct, move) %>%
  group_by(id_alias_fct) %>%
  mutate(cum_mean = cumsum(acertou_lgl)/(1:n()))

labels <- read_csv("./data-raw/amparo/labels.csv") %>%
  as.tibble() %>%
  rm_accent_from_names() %>%
  clean_names() %>%
  mutate(player_alias = cod) %>%
  inner_join(players, by="player_alias")

labels_clean <- labels %>%
  mutate(playeralias  = as.factor(cod),
         idade        = idad,
         sexo         = sexo,
         hy           = hy %>% as.numeric %% 4,
         hy           = ifelse(is.na(hy), 0, hy),
         escol        = rm_accent(escol),
         escolaridade = 1+grepl("Medio", escol)+
                        2*grepl("Superior", escol),
         moca_abs     = moca_abstracao,
         moca_ate     = moca_atencao,
         moca_evoc    = moca_evoc_tardia,
         moca_lin     = moca_linguagem,
         moca_nom     = moca_nomeacao,
         moca_ori     = moca_orientacao,
         moca_vis     = moca_visuoesp_exec,
         moca_tot     = moca_total,
         best_lim     = as.numeric(best_lim_estab_vert),
         best_marcha  = as.numeric(best_estab_marcha),
         best_orient  = best_orient_sens,
         best_reat    = best_reat,
         best_rest    = as.numeric(best_rest_biom),
         best_trans   = as.numeric(best_trans_antec),
         best_tot     = as.numeric(best),
         updrs_post   = as.numeric(est_post_updrs_iii),
         updrs_rig    = as.numeric(rigidez_updrs_iii),
         updrs_trem   = as.numeric(tremor_updrs_iii),
         updrs_tot    = as.numeric(updrs_iii)) %>%
  select(playeralias, idade, sexo, hy, escolaridade,
         moca_abs, moca_ate, moca_evoc, moca_lin, moca_nom, moca_ori, moca_vis, moca_tot,
         best_lim, best_marcha, best_orient, best_reat, best_rest, best_trans, best_tot,
         updrs_post, updrs_rig, updrs_trem, updrs_tot)

table_hy <- labels_clean$hy %>% 
  table()
table_hy[-1] %>% sum()

data <- inner_join(data, labels_clean, by="playeralias")
write.csv(data, "./data/amparo/data.csv")
#nao rodar por sigilo do DB
#saveRDS(data, "../data/amparo/data.rds")


#Dupla tarefa
dt <- read_csv("./data-raw/amparo/dt.csv") %>%
  as.tibble() %>%
  rm_accent_from_names() %>%
  clean_names() %>%
  mutate(idad         = as.numeric(idad),
         sexo         = as.factor(sexo),
         hy           = hy %>% as.numeric %% 4,
         hy           = ifelse(is.na(hy), 0, hy),
         escol        = rm_accent(escol),
         escolaridade = 1+grepl("Medio", escol)+
           2*grepl("Superior", escol),
         moca_tot     = as.numeric(moca_total),
         x30s_dist_td = as.numeric(x30_fv),
         x30s_dist_ts = as.numeric(x30_ts),
         x30s_fv_td   = as.numeric(pal_dt_30),
         x30s_fv_ts   = as.numeric(bl_30),
         x10m_dist_td = as.numeric(x10m_fv),
         x10m_dist_ts = as.numeric(x10m_ts),
         x10m_fv_td   = as.numeric(pal_dt_10m),
         x10m_fv_ts   = (x10m_dist_td * x30s_fv_ts)/30,
         idx_1_x30s   = ((x30s_dist_td*x30s_fv_td)-(x30s_dist_ts*x30s_fv_ts))/sqrt(x30s_dist_ts*x30s_fv_ts),
         idx_1_x10m   = ((x10m_dist_td*x10m_fv_td)-(x10m_dist_ts*x10m_fv_ts))/sqrt(x10m_dist_ts*x10m_fv_ts)
         )

write.csv(dt, "./data/amparo/dt.csv")
