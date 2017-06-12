library(dplyr)
library(magrittr)
library(tibble)

arqs <- list.files("./data")
data <- rep(NA, length(arqs)) %>% as.list
for(ii in 1:length(arqs))
{
  con <- paste("./data/",arqs[ii],sep="") %>% file("r")
  lines <- con %>% readLines
  close(con)
  stage <- (lines[10] %>% strsplit(",") %>% unlist)[2]
  r_data <- lines[12:length(lines)] %>%
              strsplit(",")
  c_data <- 1:length(r_data) %>% 
            lapply(function(tt) c(arqs[ii],
                                  stage,
                                  tt,
                                  r_data[[tt]][3] %>% as.logical,
                                  r_data[[tt]][1] %>% as.numeric))
  if(length(r_data) == 0) c_data <- NULL
  data[[ii]] <- c_data
}
data %<>% unlist %>% matrix(ncol=5,byrow=TRUE) %>% as_tibble
colnames(data) <- c("id", "stage", "tempo", "acerto", "seq")
data %<>% mutate(id=as.factor(id), 
                 stage=as.factor(stage), 
                 tempo=as.numeric(tempo), 
                 acerto=as.logical(acerto),
                 seq=as.numeric(seq))
saveRDS(data, "./clean_data/data.rds")
