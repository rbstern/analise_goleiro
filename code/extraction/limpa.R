library(tidyverse)
source("./libs/lib_fnames.R")

read_amparo = function(x)
{
  fname = fname_data_raw(x)
  read_csv(fname, col_types = list(col_double(), 
                                   col_double(),
                                   col_logical(),
                                   col_double(),
                                   col_character(),
                                   col_character(),
                                   col_double(),
                                   col_character(),
                                   col_double(),
                                   col_character()))
}
    
get_data_raw_dir() %>% 
  list.files() %>% 
  as.list() %>% 
  lapply(read_amparo) %>% 
  bind_rows() %>% 
  write_rds(fname_data("amparo.rds"))
