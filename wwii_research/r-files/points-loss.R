library(dplyr)
library(ggplot2)
setwd("~/Documents/CoD/full-game-data")
# read in the csv
filenames <- list.files( pattern="*.csv", full.names=TRUE)

output <- data.frame()

for (i in 1:length(filenames)) {
data <- read.csv(filenames[i])
data <- subset(data, data$mode == 'Hardpoint')
output <- rbind(output, data)
}
# Make table by game
first <- output %>%
  group_by(match.id, team) %>%
  mutate(win = ifelse(win. == "W",1,0)) %>%
  summarise(score = mean(score), kills = sum(kills), deaths = sum(deaths), ksa = sum(kills..stayed.alive.), win = mean(win)) %>%
  filter(score != 250) %>%
  ungroup() %>% group_by(team) %>%
  summarise(score = mean(score), n = n())

second <- output %>%
  group_by(team) %>%
  mutate(win = ifelse(win. == "W",1,0)) %>%
  summarise(wins = mean(win))

first <- merge(first, second, by = 'team')
first <- subset(first, first$n >5 & first$wins > 0.5)

ggplot(first, aes(wins, score)) + geom_point(shape = 1, size = 3, stroke = 2)+
  geom_smooth(method = 'lm', se = F) +
  ggtitle("Average Points Scored in Losses \nCompared to Team HP Win%") +
  xlab("Team Win%") + ylab("Average Score in Losses") + 
  theme(text = element_text(size = 18))


# Look at wins where team was outslayed
output %>%
  group_by(match.id, team) %>%
  mutate(win = ifelse(win. == "W",1,0)) %>%
  summarise(score = mean(score), kills = sum(kills), deaths = sum(deaths), ksa = sum(kills..stayed.alive.), win = mean(win)) %>%
  filter(team == "Red Reserve", win == 1) %>%
  mutate(outslay = ifelse(kills > deaths, 1,0)) %>%
  group_by(outslay) %>%
  summarise(ksa = mean(deaths, na.rm = T), n = n())
