library(dplyr)
setwd("/home/dougliebe/Documents/CoD/cod/data")

game1 <- read.csv('forest_week34.csv')
game1$X <- NULL
start_time = 1
colnames(game1) <- c('team','x','y','area','time', 'opp', 'score', 'map')

# Set hps either 4 or 5
hp <- rep(seq(1,4),5, each = 60)
hp_time <- rep(seq(1,60),20)
set <- rep(seq(1,4), each = 60*5)
# hp <- rep(seq(1,5),5, each = 60)
# set <- rep(seq(1,5), each = 60*5)
time <- seq(start_time, start_time+length(hp)-1)
df <- data.frame(time,hp,set, hp_time)
data = merge(game1, df, by = 'time')

team_in_hp_first <- data %>%
  group_by(team, opp) %>%
  arrange(time) %>%
  mutate(gain = (score - lag(score, 1))) %>%
  ungroup() %>%
  filter(hp_time < 10) %>%
  group_by(team, opp,set, hp) %>%
  summarise(m = sum(gain, na.rm = T)) %>% data.frame()

team_hp_total <- data %>%
  # filter(time > 400) %>%
  group_by(team, opp) %>%
  arrange(time) %>%
  mutate(gain = (score - lag(score, 1))) %>%
  ungroup() %>%
  group_by(team, opp,set, hp) %>%
  summarise(l = sum(gain, na.rm = T)) %>% data.frame()


result <- merge(team_in_hp_first, team_hp_total, by = c('team','opp','set','hp'))

# compare teams that score points in first 10 seconds of hp compared to not
result %>%
  group_by(hp,gr=cut(m, breaks= c(-1,2, 6,100), right=T, left = F) ) %>%
  summarise(sum = mean(l))



