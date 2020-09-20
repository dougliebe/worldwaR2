library(jsonlite)
library(dplyr)
setwd('~/Documents/CoD/')
filenames <- list.files("structured-2018-01-14-neworleans", pattern="*.json", full.names=TRUE)
# hills <- list()
#output df
# output <- data.frame()

for (i in 1:length(filenames)) {
data_json <- read_json(filenames[i], simplifyVector = T)



# For each file

if (data_json$mode == "Hardpoint") {
events <- (data_json$events)
data <- subset(events, events$type == 'death')

# make team chart
team_players <- data.frame(name = data_json$players$name, killed.team = data_json$players$team)
levels(team_players$name) <- c(levels(team_players$name), 'HEADACHES')
team_players[as.character(team_players$name) == "ACHES",'name'] <- "HEADACHES"  #doesnt work
start_time = 5000

# Make point chart
hills[i] <- length(data_json$teams$round_scores[[1]])

pt_chart <-data.frame(team = rep(data_json$teams$name,each = length(data_json$teams$round_scores[[1]])),
           hp = rep(rep(seq(1,length(data_json$hp_hill_names)),4)[1:length(data_json$teams$round_scores[[1]])],2),
           set = rep(rep(seq(1,4),each = length(data_json$hp_hill_names))[1:length(data_json$teams$round_scores[[1]])],2),
           score = unlist(data_json$teams$round_scores))
team_chart <-data.frame(name = rep(team_players$name, each = (nrow(pt_chart)/2)),
           team = rep(team_players$killed.team, each = (nrow(pt_chart)/2)),
           hp = rep(rep(seq(1,length(data_json$hp_hill_names)),4)[1:(nrow(pt_chart)/2)],8),
           set = rep(rep(seq(1,4),each = length(data_json$hp_hill_names))[1:(nrow(pt_chart)/2)],8))
score_chart <- merge(team_chart, pt_chart, by = c('team', 'hp', 'set'))


# Split by hp and set
if (length(data_json$hp_hill_names)==4) {
  hp <- rep(seq(1,4),4, each = 60*1000)
  time <- seq(start_time, start_time+length(hp)-1) # shift 10 secs to account for rotation
  set <- rep(seq(1,4),each = 60*1000*4)
  df <- data.frame(time,hp, set)
  data = merge(data, df, by.x = 'time_ms', by.y = 'time')
  # data$data <- merge(data$data, team_players, by.x = 'id', by.y = 'name')
} else {
  hp <- rep(seq(1,5),4, each = 60*1000)
  time <- seq(start_time, start_time+length(hp)-1) # shift 10 secs to account for rotation
  set <- rep(seq(1,5),each = 60*1000*4)
  df <- data.frame(time,hp, set)
  data = merge(data, df, by.x = 'time_ms', by.y = 'time')
  # data$data <- merge(data$data, team_players, by.x = 'id', by.y = 'name')
}

df <- data.frame(name = data$data$id,hp = data$hp, set = data$set)
# get kills per hill to know engagements
df2 <- data.frame(name = data$data$attacker$id ,hp = data$hp, set = data$set)
# Get deaths per set (or other in the future)
# Need to record which map too, since this can change
kills <- df2 %>%
  group_by(name, set, hp) %>%
  summarise(kills = n()) %>% data.frame()
deaths <- df %>%
  group_by(name,set,hp) %>%
  summarise(deaths = n()) %>% data.frame()
deaths$map = data_json$map
deaths <- merge(deaths, score_chart, by = c('name','hp','set'))
deaths <- merge(deaths, kills,  by = c('name','hp','set'))
# Store deaths per set per player
output <- rbind(output,deaths)
}}


#show effect of each death on score per hp
output %>%
  group_by(map, hp) %>%
  summarise(c = summary(lm(score ~ deaths))$coefficients[2,1])

#player effect of deaths per map
player_map <- output %>%
  group_by(name,team,map) %>%
  summarise(c = summary(lm(score ~ deaths))$coefficients[2,1], n = n()) %>%
  filter(n > 30) %>%
  arrange((c)) %>% data.frame()

# now combined pts lost
# if player dies once on each map
output %>%
  group_by(name,team,map) %>%
  summarise(c = summary(lm(score ~ deaths))$coefficients[2,1], n = n(), deaths = mean(deaths)) %>%
  filter(n > 30) %>%
  group_by(name,team) %>%
  summarise(total_loss = sum(c), n = sum(n)) %>%
  arrange((total_loss)) %>% data.frame()

# how much does each death cost * deaths per hill
output %>%
  group_by(name,team,map) %>%
  summarise(c = summary(lm(score ~ deaths))$coefficients[2,1], n = n(), deaths = mean(deaths)) %>%
  filter(n > 30) %>%
  mutate(loss = (deaths-2.5)*c) %>%
  group_by(name,team) %>%
  summarise(total_loss = sum(loss), n = sum(n), saved = total_loss*10.8) %>%
  arrange(desc(total_loss)) 

team_map <- output %>%
  group_by(team, map, hp ,set) %>%
  summarise(scored = mean(score)) %>%
  ungroup() %>%
  group_by(team,map) %>%
  summarise(pts = sum(scored)) %>%
  arrange(desc(pts)) %>% data.frame()

#points lost per map
ppm <- merge(player_map, team_map, by = c('team','map'))
ppm$pct_lost <- as.numeric((ppm$pts)* (-ppm$c) )     
hist(ppm$pct_lost)
ppm %>%
  arrange(desc(pct_lost)) %>% head()
