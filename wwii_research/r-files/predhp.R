library(dplyr)
library(ggplot2)
library(jsonlite)
library(ggplot2)
library(scales)
library(png)
library(dplyr)
library(MASS)
library(reshape2)

setwd('C:/Users/Doug/Documents/CoD/')
img <- readPNG('cwl-data/maps/ww2/london_docks.png')
output <- data.frame()

# Go through all games all events
location <- list.files(path = 'cwl-data/data/structured', pattern = "structured")
for(j in 1:(length(location))) {
  filenames <- list.files( path = paste('cwl-data/data/structured/',location[j], sep = ""),pattern="*.json", full.names=TRUE)
  print(location[j])
  
  start_time = 5000
  for (i in 1:length(filenames)) {
    data_json <- fromJSON(filenames[i], simplifyVector = T)
    if(data_json$mode == "Hardpoint" & !is.null(nrow(data_json$events))){
      # 
      # 
      events <- (data_json$events)
      data <- subset(events, events$type == 'death')
      pt_chart <-data.frame(team = rep(data_json$teams$name,each = length(data_json$teams$round_scores[[1]])),
                            hp = rep(rep(seq(1,length(data_json$hp_hill_names)),4)[1:length(data_json$teams$round_scores[[1]])],2),
                            set = rep(rep(seq(1,4),each = length(data_json$hp_hill_names))[1:length(data_json$teams$round_scores[[1]])],2),
                            score = c(data_json$teams$round_scores[[1]]-data_json$teams$round_scores[[2]],-data_json$teams$round_scores[[1]]+data_json$teams$round_scores[[2]]), 
                            win = rep(data_json$teams$is_victor, each = length(data_json$teams$round_scores[[1]])),
                            opp = rev(rep(data_json$teams$name,each = length(data_json$teams$round_scores[[1]]))))
      pt_chart$lead <- c(cumsum(pt_chart$score[1:length(data_json$teams$round_scores[[1]])]),
                         cumsum(pt_chart$score[(length(data_json$teams$round_scores[[1]])+1):(length(data_json$teams$round_scores[[1]])*2)]))
      # # # team_rounds <- data.frame(player.team = rep(data_json$teams$name,each = data_json$rounds),
      # #                           round = rep(seq(1,data_json$rounds),2),
      # #                           offdef = c(rep(c("off",'def'),length.out = data_json$rounds),rep(c("def",'off'),length.out = data_json$rounds)),
      # #                           win = c(data_json$teams$round_scores[[1]],data_json$teams$round_scores[[2]]))
      team_players <- data.frame(name = data_json$players$name, team = data_json$players$team, gun = data_json$players$fave_weapon)
      data = do.call(cbind.data.frame, data)
      data = do.call(cbind.data.frame, data)
      data = do.call(cbind.data.frame, data)
      
      # Split by hp and set
      if (length(data_json$hp_hill_names)==4) {
        hp <- rep(seq(1,4),4, each = 60*1000)
        time <- seq(start_time, start_time+length(hp)-1) # shift 10 secs to account for rotation
        set <- rep(seq(1,4),each = 60*1000*4)
        df <- data.frame(time,hp, set)
        data = merge(data, df, by.x = 'time_ms', by.y = 'time')
      } else {
        hp <- rep(seq(1,5),4, each = 60*1000)
        time <- seq(start_time, start_time+length(hp)-1) # shift 10 secs to account for rotation
        set <- rep(seq(1,5),each = 60*1000*4)
        df <- data.frame(time,hp, set)
        data = merge(data, df, by.x = 'time_ms', by.y = 'time')
      }
      
      data <- merge(data, team_players, by.x = 'data.attacker.id', by.y = 'name')
      data <- merge(data, team_players, by.x ='data.id', by.y = 'name')
      data <- merge(data, pt_chart, by.x = c('team.x', 'hp', 'set'), by.y = c('team','hp','set'))
      data <- data %>%
        mutate(game_win = ifelse(team.x == data_json$teams$name[[1]],
              data_json$teams$is_victor[[1]]*1,data_json$teams$is_victor[[2]]*1))
      output <- rbind(output, data.frame(map = data_json$map, data, event = location[j],id = data_json$id))
      # output <- rbind(output, data.frame(map = data_json$map, offdef = data$offdef, x = data$data.attacker.pos.x, y = data$data.attacker.pos.y))
      # output <- rbind(output, data.frame(pt_chart, map = data_json$map, id = data_json$id, code = data_json$series_id, event = location[j]))
    }
  }
}
# add trades 
output <- output %>%
  group_by(id) %>%
  arrange(time_ms) %>%
  mutate(trade = ifelse((opp == lead(team.x,1)) &
                          (lead(time_ms,1) - time_ms < 5000),1,0)) %>%
  data.frame()


o <- output %>%
  data.frame() %>% 
  group_by(id, map, team.x, opp, game_win) %>%
  summarise(round = 1, win = ifelse(sum(win) > 0,1,0), kills = n()) %>% 
  ungroup() %>%
  group_by(map, team.x) %>%
  mutate(rounds = cumsum(round), game_win_pct = cummean(game_win),wins = cumsum(win)/rounds,
         kills = cumsum(kills)/rounds) %>%
  mutate(pred_win_pct = lag(game_win_pct,1),pred_win = lag(wins,1), pred_kills = lag(kills,1)) %>%
  data.frame()


## testing on stage 2 data
last <- o %>%
  group_by(team.x, map) %>%
  summarise(wins = last(wins), n = max(rounds)) %>%
  mutate(w = round(wins*n), l = n-w)



oppoff <- o 
# %>%
#   filter(offdef == 'off')

o <- o %>%
  full_join(oppoff, by = c('team.x' = 'opp', 'opp' = 'team.x',
                           'id'='id')) %>%
  filter(rounds.x > 10, rounds.y > 10) %>%
  data.frame() 

# ona = o %>%
#   group_by(date.x, id, map.x, player.team.x, player.team.y, game_win.x) %>%
#   summarise(pred_win.x = first(pred_kills.x), pred_win.y = first(pred_kills.y))
o$pred <- ifelse(o$pred_kills.x > o$pred_kills.y,1,0)
o$pred <- (predict(m, o,type = 'response')>2.1)*1

# ona <- ona[!is.na(ona$pred),]
confusionMatrix(as.factor(o$pred), as.factor(o$game_win.x))
RMSE(o$pred, o$game_win.x)

o$dif <- o$pred_win.x - o$pred_win_pct.y
m <- glm(win.x ~ dif, data = o)
summary(m)
exp(coefficients(m))


m <- glm(win.x ~ pred_win.x + pred_win.y, data = o, family = 'binomial')
summary(m)
exp(coefficients(m))

m_sm <- glm(win.x ~ dif, data = o[o$map.x == 'Sainte Marie du Mont',], family = 'binomial')
summary(m_sm)
exp(coefficients(m_sm))

m_ld <- glm(as.factor(win.x) ~ dif, data = o[o$map.x == 'London Docks',], family = 'binomial')
summary(m_ld)
exp(coefficients(m_ld))

m_va <- glm(win.x ~ dif, data = o[o$map.x == 'Valkyrie',], family = 'binomial')
summary(m_va)
exp(coefficients(m_va))

m_af <- glm(win.x ~ dif, data = o[o$map.x == 'Ardennes Forest',], family = 'binomial')
summary(m_af)
exp(coefficients(m_af))

m_gib <- glm(win.x ~ dif, data = o[o$map.x == 'Gibraltar',], family = 'binomial')
summary(m_gib)
exp(coefficients(m_gib))


#function that returns winprob
winprob <- function(data) {
  r = ifelse(data$map == "Sainte Marie du Mont", predict(m_sm, data),
         ifelse(data$map == 'London Docks', predict(m_ld, data),
                ifelse(data$map == 'Ardennes Forest', predict(m_af, data), 
                       ifelse(data$map == "Gibraltar",predict(m_gib,data),predict(m_ld,data)))))
  return(exp(r)/(1+exp(r)))
}

# with stage 2 data, make df of each matchup, which map they played and win/loss
test <- o[,c('map','team.x','opp',"game_win")]
d = test %>%
  full_join(last, by = c('team.x'='team.x','map'='map')) %>%
  full_join(last, by = c('opp'='team.x','map'='map')) %>%
  filter(n.x > 7, n.y > 7, map != 'Valkyrie', map != "Gibraltar") %>%
  data.frame()
  
head(d)
d$dif <- d$wins.x - d$wins.y
d$pred_win <- winprob(d)
p = d %>%
  group_by(team.x) %>%
  summarise(s = sum(pred_win), w = sum(game_win))

# add columns for each map win chance
df <- expand.grid(team.x = unique(d$team.x), opp = unique(d$opp), map = unique(d$map))
df = df %>%
  full_join(last, by = c('team.x'='team.x','map'='map')) %>%
  full_join(last, by = c('opp'='team.x','map'='map')) %>%
  filter(n.x > 7, n.y > 7) %>%
  data.frame()
df$dif <- df$wins.x - df$wins.y
df$pred = winprob(df)

df = df %>%
  group_by(team.x, opp) %>%
  summarise(winpct = max(pred))
d = d %>%
  full_join(df, by = c('team.x'='team.x', 'opp'='opp'))
d$pctdif <- d$pred_win - d$winpct
d <- d[!is.na(d$pctdif),]
d %>%
  group_by(team.x) %>% 
  summarise(pick = mean(pick_best),pred = sum(pred_win, na.rm =T), max = sum(winpct), a = mean(pctdif, na.rm = T), n = n()) %>%
  mutate(wins = max-pred)




# New predicitons
df <- expand.grid(team.x = 'EUNITED', opp = 'LUMINOSITY GAMING', map = unique(last$map))
df = df %>%
  full_join(last, by = c('team.x'='team.x','map'='map')) %>%
  full_join(last, by = c('opp'='team.x','map'='map')) %>%
  filter(!is.na(wins.y), !is.na(wins.x))
df$dif <- df$wins.x - df$wins.y
df$pred = winprob(df)
