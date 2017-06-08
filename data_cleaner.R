library(dplyr)

arqs <- list.files("./data")
stages <- rep(NA, length(arqs))
max.seq <- 0
for(ii in 1:length(arqs))
{
  con <- paste("./data/",arqs[ii],sep="") %>% file("r")
  lines <- con %>% readLines
  stages[ii] <- (lines[10] %>% strsplit(",") %>% unlist)[2]
  max.seq <- max(max.seq, lines[12:length(lines)] %>% length)
  close(con)
}

tempos <- 1:max.seq %>% rep(length(arqs))
dim(tempos) <- c(max.seq, length(arqs))
tempos <- tempos %>% t %>% as.matrix
colnames(tempos) <- 1:max.seq %>% 
                    sapply(function(x) paste("tempo",x,sep=""))

acertos <- matrix(NA, nrow=length(arqs), ncol=max.seq)
for(ii in 1:length(arqs))
{
  con <- paste("./data/",arqs[ii],sep="") %>% file("r")
  lines <- con %>% readLines
  estes_acertos <- lines[12:length(lines)] %>%
                   strsplit(",") %>%
                   sapply(function(x) x[3] %>% as.logical)
  acertos[ii,1:length(estes_acertos)] <- estes_acertos
  close(con)
}
colnames(acertos) <- 1:max.seq %>% 
                     sapply(function(x) paste("acerto",x,sep=""))

data <- data.frame(stages, tempos, acertos)
saveRDS(data, "./clean_data/data.R")
