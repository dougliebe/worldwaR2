library(jsonlite)
library(ggplot2)
library(scales)
library(png)
library(dplyr)
library(lme4)
library(caret)

setwd('C:/Users/Doug/Documents/CoD/')
img <- readPNG('cwl-data/maps/ww2/sainte_marie_du_mont.png')

# set blank output
output <- data.frame()

# For each event folder
location <- list.files(path = 'cwl-data/data/structured', pattern = "structured")
for(j in 1:(length(location))) {
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
      data <- data %>%
        group_by(round) %>%
        arrange(time_ms) %>%
        mutate(game_win = ifelse(player.team.x == data_json$teams$name[[1]], data_json$teams$is_victor[[1]]*1,data_json$teams$is_victor[[2]]*1)) %>%
        mutate(round_duration = max(round_time_ms)) %>%
        mutate(first_kill = ifelse(round_time_ms == min(round_time_ms),1,0)) %>%
        mutate(trade = ifelse(round_time_ms == max(round_time_ms),2,
                              ifelse((player.team.y == lead(player.team.x,1)) &
                                       (lead(round_time_ms,1) - round_time_ms < 5000),1,0))) %>%
        mutate(avenge = ifelse(lag(trade,1) == 1, 1,0))
      # add row plus gun, win and name
      output <- rbind(output, data.frame(map = data_json$map, id = data_json$id, date = data_json$start_time_s, data))
    }
  }
}


o <- output %>%
  filter(round != 11) %>%
  # filter(offdef == 'off' ) %>%
  data.frame() %>% 
  group_by(date, id, map, player.team.x, player.team.y, game_win, round) %>%
  summarise(rounds = 1, trades = sum(trade), duration = sum(round_duration),
            fb = sum(first_kill), win = ifelse(sum(win) > 0,1,0), kills = n()) %>% 
  ungroup() %>%
  group_by(date, id, map, player.team.x, player.team.y, game_win) %>%
  summarise(rounds = sum(rounds), trades = sum(trades), duration = sum(duration),
            fb = sum(fb), win = sum(win), kills = sum(kills)) %>% 
  ungroup() %>%
  group_by(map, player.team.x) %>%
  mutate(rounds = cumsum(rounds), trades = cumsum(trades)/rounds , game_win_pct = cummean(game_win),
         duration = cumsum(duration)/rounds, fb = cumsum(fb)/rounds,wins = cumsum(win)/rounds,
         kills = cumsum(kills)/rounds) %>%
  mutate(pred_rounds = lag(rounds, 1), pred_trades = lag(trades, 1), pred_fb = lag(fb,1),
         pred_win_pct = lag(game_win_pct,1),
         pred_duration = lag(duration,1), pred_win = lag(wins,1), pred_kills = lag(kills,1)) %>%
  data.frame()

oppoff <- o 
# %>%
#   filter(offdef == 'off')

o <- o %>%
  full_join(oppoff, by = c('player.team.x' = 'player.team.y', 'player.team.y' = 'player.team.x',
                           'id'='id')) %>%
  filter(rounds.x > 25, rounds.y > 25) %>%
  data.frame() 

# ona = o %>%
#   group_by(date.x, id, map.x, player.team.x, player.team.y, game_win.x) %>%
#   summarise(pred_win.x = first(pred_kills.x), pred_win.y = first(pred_kills.y))
o$pred <- ifelse(o$pred_win.x > o$pred_win.y,1,0)
o$pred <- (predict(m, o,type = 'response')>0.5)*1

# ona <- ona[!is.na(ona$pred),]
confusionMatrix(as.factor(o$pred), as.factor(o$game_win.x))
RMSE(o$pred, o$game_win.x)

m <- glm(game_win.x ~ pred_win.x*pred_win_pct.x, data = o, family = 'binomial')
summary(m)

# get output with per round stats
# remove rnd 11
# need: map, off/def, date, team, opp
# trades/rd, time/rd, fb/rd, win% per round
o <- output %>%
  filter(round != 11) %>%
  data.frame() %>% 
  group_by(date, id, map, player.team.x, player.team.y, game_win, round, offdef) %>%
  summarise(rounds = 1, trades = sum(trade), duration = sum(round_duration),
            fb = sum(first_kill), win = mean(win), kills = n()) %>%
  ungroup() %>%
  group_by(player.team.x,map,offdef) %>%
  mutate(rounds = cumsum(rounds), trades = cumsum(trades)/rounds ,
         duration = cumsum(duration)/rounds, fb = cumsum(fb)/rounds,wins = cumsum(win)/rounds, kills = cumsum(kills)/rounds) %>%
  mutate(pred_rounds = lag(rounds, 1), pred_trades = lag(trades, 1), pred_fb = lag(fb,1),
         pred_duration = lag(duration,1), pred_win = lag(wins,1), pred_kills = lag(kills,1)) %>%
  data.frame() 
oppoff <- o %>%
  filter(offdef == 'off')
o <- o %>%
  full_join(oppoff, by = c('player.team.x' = 'player.team.y', 'player.team.y' = 'player.team.x',
                      'id'='id'))
o <- o %>%
  mutate(category=cut(pred_win.x, breaks=c(0.4,0.45,0.5,0.55, 0.6,0.65, 0.7))) %>%
  mutate(opponent=cut(pred_win.y, breaks=c(0.4,0.45,0.5,0.55, 0.6,0.65, 0.7))) %>%
  mutate(dif = pred_win.x - pred_win.y) %>%
  mutate(dif_bin=cut(dif, breaks=seq(-0.2,0.2,0.01)))
s <- o %>%
  group_by(dif_bin) %>%
  summarise(winpct = mean(win.x), n = n())
o <- o[o$rounds.x > 30 & o$rounds.y > 30,]
  
# ggplot(o[o$offdef.x == 'off',], aes(pred_win.x, win.x, color = map.x)) +
#   geom_point()+geom_smooth(method = 'lm') + xlab('Round Duration (s)') + ylab('Team Win%') +
#   # scale_color_discrete(labels = c('Defense', "Offense"), name = 'Side') +
#   ggtitle('London Docks \nSearch & Destroy')
o <- o[complete.cases(o),]
## offense preds
oo <- o[o$offdef.x == 'off',]
oo$windif <- oo$pred_win.x - oo$pred_win.y
oo$killdif <- oo$pred_kills.x - oo$pred_kills.y
oo$pred_kills.x <- scale(oo$pred_kills.x)
oo$pred_kills.y <- scale(oo$pred_kills.y)

m <- glmer(win.x ~ pred_win.x + (1|map.x), data = oo, family = 'binomial')
summary(m)

m_sm <- glm(win.x ~ pred_win.x*pred_win.y, data = oo[oo$map.x == 'Sainte Marie du Mont',], family = 'binomial')
summary(m_sm)

m_ld2 <- glm(win.y ~ pred_win.x*pred_win.y, data = oo[oo$map.x == 'London Docks',], family = 'binomial')
summary(m_ld)

m_va <- glm(win.x ~ pred_win.x, data = oo[oo$map.x == 'Valkyrie',], family = 'binomial')
summary(m_va)

m_af <- glm(win.x ~ pred_win.x*pred_win.y , data = oo[oo$map.x == 'Ardennes Forest',], family = 'binomial')
summary(m_af)

m_tx <- glm(win.x ~ pred_win.x*pred_win.y, data = oo[oo$map.x == 'USS Texas',], family = 'binomial')
summary(m_tx)



games <- output %>% 
  group_by(id, map, player.team.x, player.team.y) %>%
  summarise(win = sum(win))
games$win <- ifelse(games$win == 24,1,0)

test <- games %>% 
  full_join(last, by = c('player.team.x'='player.team.x', 'map'='map')) %>%
  full_join(last, by = c('player.team.y'='player.team.x', 'map'='map'))

predict(m_ld, test)
test$pred2 <- predict(m_ld2, test)
