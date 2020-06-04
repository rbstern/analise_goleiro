library(abjutils)
library(janitor)
library(magrittr)
library(tidyverse)
source("./libs/lib_fnames.R")

rm_accent_from_names <- function(dt)
{
  dt %<>% set_names(rm_accent(names(.)))
  invisible(dt)
}

data <- fname_data("amparo.rds") %>% 
  read_rds() %>%
  rm_accent_from_names() %>%
  clean_names()

# Filtrar apenas jogadores que
# acertaram 3 ou mais questoes no aquecimento.
players <- data %>%
  filter(game == "AQ", total_correct >= 3) %>%
  select(player_alias) %>%
  distinct()

data %<>% 
  select(-x1) %>% 
  inner_join(players, by = "player_alias") %>%
  mutate(playid       = phase,
         new_playid   = paste(game, playid, sep = "-"),
         alias        = as.numeric(as.factor(player_alias)),
         id           = new_playid %>% as.factor() %>% as.numeric(),
         fator        = max(id)*(alias-1) + id,
         id_alias_fct = as.factor(paste(new_playid, player_alias, sep = "")),
         acertou_lgl  = correct %>% as.numeric() %>% as.logical(),
         movement_time = as.numeric(movement_time),
         move         = as.numeric(move)) %>%
  arrange(id_alias_fct, move) %>%
  group_by(id_alias_fct) %>%
  mutate(cum_mean = cumsum(acertou_lgl)/(1:n())) %>% 
  ungroup()

data = fname_data("clinico.csv") %>% 
  read_csv() %>% 
  as_tibble() %>%
  rm_accent_from_names() %>%
  clean_names() %>%
  mutate(
    player_alias = as.character(cod),
    escol        = rm_accent(escol),
    escolaridade = 1 + grepl("Medio", escol)+
      2*grepl("Superior", escol),
    escolaridade_anos = anos_escol,
    hy           = ifelse(hy == "(-)", "0", hy) %>% as.numeric(),
    best_lim     = best_lim_estab_vert,
    best_marcha  = best_estab_marcha,
    best_orient  = best_orient_sens,
    best_reat    = best_reat,
    best_rest    = best_rest_biom,
    best_trans   = best_trans_antec,
    best_tot     = best,
    moca_abs     = mo_ca_abstracao, 
    moca_ate     = mo_ca_atencao, 
    moca_evoc    = mo_ca_evoc_tardia, 
    moca_lin     = mo_ca_linguagem, 
    moca_nom     = mo_ca_nomeacao, 
    moca_ori     = mo_ca_orientacao, 
    moca_vis     = mo_ca_visuoesp_exec, 
    moca_tot     = mo_ca_total,
    dgi          = dgi,
    tug_custo    = (tug_st - tug_dt)/tug_st
  ) %>% 
  select(player_alias, hy, escolaridade,
         best_lim, best_marcha, best_orient,
         best_reat, best_rest, best_trans, best_tot,
         moca_abs, moca_ate, moca_evoc, moca_lin, 
         moca_nom, moca_ori, moca_vis, moca_tot,
         dgi, tug_custo) %>% 
  inner_join(data, by = "player_alias") %>% 
  mutate(player_alias = as.factor(player_alias)) 

data %>% 
  write_rds(fname_data("data.rds"))

data %>% 
  write_csv(fname_data("data.csv"))
