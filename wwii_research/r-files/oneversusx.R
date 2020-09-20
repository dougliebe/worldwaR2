library(jsonlite)
library(ggplot2)
library(scales)
library(png)
library(dplyr)

setwd('C:/Users/Doug/Documents/CoD/')
img <- readPNG('cwl-data/maps/ww2/sainte_marie_du_mont.png')

# set blank output
output <- data.frame()

# For each event folder
location <- list.files(path = 'cwl-data/data/structured', pattern = "structured")
for(j in 2:length(location)) {
  filenames <- list.files( path = paste('cwl-data/data/structured/',location[j], sep = ""),pattern="*.json", full.names=TRUE)
  print(location[j])
  
  # for each game in each event folder
  for (i in 1:length(filenames)) {
    #read json
    data_json <- fromJSON(filenames[i], simplifyVector = T)
    # filter out by mode if desired
    if(data_json$mode == "Search & Destroy" & !is.null(nrow(data_json$events))){
      
      
      events <- (data_json$events)
      data <- subset(events, events$type == 'death')
      
      team_rounds <- data.frame(player.team = rep(data_json$teams$name,each = data_json$rounds),
                                round = rep(seq(1,data_json$rounds),2),
                                offdef = c(rep(c("off",'def'),length.out = data_json$rounds),rep(c("def",'off'),length.out = data_json$rounds)),
                                win = c(data_json$teams$round_scores[[1]],data_json$teams$round_scores[[2]]))
      team_players <- data.frame(name = data_json$players$name, player.team = data_json$players$team, gun = data_json$players$fave_weapon)
      #convert list of lists to df
      data = do.call(cbind.data.frame, data)
      data = do.call(cbind.data.frame, data)
      data = do.call(cbind.data.frame, data)
      
      data <- merge(data, team_players, by.x = 'data.attacker.id', by.y = 'name') # name players
      data <- merge(data, team_players, by.x = 'data.id', by.y = 'name') 
      data <- merge(data, team_rounds, by.x = c('player.team.x', 'round'),
                    by.y = c('player.team', 'round')) # win or not?
      
      # the cumsum takes cumulative sum of when player is on team 1 and player is the killer,
      # then considers that kill count = team2 death count, then visa versa
      data <- data %>%
        arrange(time_ms) %>%
        dplyr::group_by(round) %>%
        
        dplyr::mutate(team2dead = cumsum(player.team.x == data_json$teams$name[[1]])) %>% 
        mutate(team1dead = cumsum(player.team.x == data_json$teams$name[[2]])) %>%
        
        #winner = 1 if team 1 wins, 2 for team 2, so 1 + if team2 wins = T
        mutate(winner = 1 + (data_json$teams$round_scores[[2]][round] == 1)) %>%
        
        # need this line to prevent the kill before a 1vX clutch to count as clutch
        # make sure the killer is on the winning team!
        mutate(team1gettingkill = ifelse(lag(team1dead,1) < team1dead, 0,1)) %>%
        
        data.frame() 

      # add row plus gun, win and name
      output <- rbind(output, data.frame(map = data_json$map, id = data_json$id, data, event = location[j], round.id = data_json$series_id))
    }
  }
}


# find when team 1 gets kill with 1 player and wins, or team 2 does it and winns
#also need to make sure the line has the winning team getting the kill
#otherwise, a player who gets a kill to make it 3v1, then entire team gets killed, will give credit to
# dying team for having "opening kill"
wins <- output %>%
  filter((team1dead==3 & winner == 1 & team1gettingkill == 1 & team2dead>0)) %>%
  mutate(x = 5-team2dead) %>%
  # filter(x == 4) %>%
  group_by(data.attacker.id,x) %>% summarise(wins = n()) %>%
  # filter(x == 2) %>%
  arrange(desc(wins))
wins2 <- output %>%
  filter((team2dead==3 & winner == 2 & team1gettingkill == 0 & team1dead > 0)) %>%
  mutate(x = 5-team1dead) %>%
  # filter(x == 4) %>%
  group_by(data.attacker.id,x) %>% summarise(wins = n()) %>%
  # filter(x == 2) %>%
  arrange(desc(wins))
wins <- rbind(wins, wins2)
wins <- wins %>%
  group_by(data.attacker.id,x) %>%
  summarise(wins = sum(wins))

# losses are easier, just when the last death (death #4)
# comes and the other team wins, check how many people were left on the winning team and credit
# the "loss" to the last player to die
loss = output %>%
  filter((team1dead==4 & winner == 2)) %>%
  mutate(x = 4-team2dead) %>%
  # filter(x == 4) %>%
  group_by(data.id,x) %>% summarise(losses = n()) %>%
  # filter(x == 2) %>%
  arrange(desc(losses)) 
loss2 = output %>%
  filter((team2dead==4 & winner == 1)) %>%
  mutate(x = 4-team1dead) %>%
  # filter(x == 4) %>%
  group_by(data.id,x) %>% summarise(losses = n()) %>%
  # filter(x == 2) %>%
  arrange(desc(losses)) 
loss <- rbind(loss, loss2)
loss <- loss %>%
  group_by(data.id,x) %>%
  summarise(losses = sum(losses))

# Combine it
pct <- merge(wins, loss , by.x = c('data.attacker.id', 'x'), by.y = c('data.id', 'x'))
pct$pct_won <- round(pct$wins/ (pct$wins+pct$losses),2)
pct$n <- pct$wins + pct$losses
pct %>%
  filter(x==4) %>% #change filter to see 1vX
  arrange(desc(pct_won)) %>% head()


pct %>%
  mutate(wwins = ifelse(x == 1, wins,
                        ifelse(x == 2, wins*4,
                               ifelse(x == 3 | x == 4, wins*20, wins)))) %>%
  mutate(wlosses = losses*20) %>%

  group_by(data.attacker.id) %>%
  summarise(wpct = sum(wwins)/sum(wlosses), n = sum(n)) %>%
  # filter(n > 20) %>%
  arrange(desc(wpct)) %>% data.frame() %>% head()
