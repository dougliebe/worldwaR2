library(dplyr)
library(ggplot2)
setwd("C:/Users/Doug/Documents/CoD/cwl-data/data")
# read in the csv
filenames <- list.files( pattern="*.csv", full.names=TRUE)

output <- data.frame()

for (i in 1:length(filenames)) {
  data <- read.csv(filenames[i])
  data <- subset(data, data$mode == 'Hardpoint') # sort by mode
  output <- rbind(output, data)
}

output %>%
  mutate(win = ifelse(win. == "W",1,0)) %>%
  group_by(match.id, team,map) %>%
  summarise(wins = mean(win)) %>%
  ungroup() %>%
  group_by(team,map) %>%
  summarise(wins = mean(wins), W = n()*wins, L = n()-W, n = n()) %>%
  filter(team == "Complexity") %>%
  arrange(desc(wins))

output %>%
  group_by(player, map) %>%
  summarise(m = sum(kills)/sum(deaths), n = n()) %>%
  filter(player == 'Denz') %>%
  arrange(desc(m))
  