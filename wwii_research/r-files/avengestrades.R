library(jsonlite)
library(ggplot2)
library(scales)
library(png)
library(dplyr)

img <- readPNG(here::here('maps/ww2/sainte_marie_du_mont.png'))

# set blank output
output <- data.frame()

# For each event folder
location <- list.files(path = 'cwl-data/data/structured', pattern = "structured")
for(j in 7:length(location)) {
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
                                win = c(data_json$teams$round_scores[[1]],data_json$teams$round_scores[[2]]),
                                game_win = c(rep(data_json$teams$is_victor[[1]]*1,data_json$rounds),rep(data_json$teams$is_victor[[2]]*1,data_json$rounds)))
      team_players <- data.frame(name = data_json$players$name, player.team = data_json$players$team, gun = data_json$players$fave_weapon)
      #convert list of lists to df
      data = do.call(cbind.data.frame, data)
      data = do.call(cbind.data.frame, data)
      data = do.call(cbind.data.frame, data)
      
      #if kill was earliest in round, label first blood
      # data <- data %>%
      #   dplyr::group_by(round) %>%
      #   dplyr::mutate(first_kill = ifelse(round_time_ms == min(round_time_ms),1,0)) %>%
      #   data.frame() %>%
      #   filter(first_kill == 1) # take only first bloods
      data <- merge(data, team_players, by.x = 'data.attacker.id', by.y = 'name') # name players
      data <- merge(data, team_players, by.x = 'data.id', by.y = 'name') 
      data <- merge(data, team_rounds, by.x = c('player.team.x', 'round'),
                    by.y = c('player.team', 'round')) # win or not?
      data <- data %>%
        group_by(round) %>%
        arrange(time_ms) %>%
        mutate(first_kill = ifelse(round_time_ms == min(round_time_ms),1,0)) %>%
        mutate(trade = ifelse(round_time_ms == max(round_time_ms),2,
                              ifelse((as.character(data.attacker.id) == as.character(lead(data.id ,1))) &
                                (lead(round_time_ms,1) - round_time_ms < 5000),1,0))) %>%
        mutate(avenge = ifelse(lag(trade,1) == 1, 1,0)) %>%
        mutate(lastdeath = ifelse(round_time_ms == max(round_time_ms),1,0)) %>%
        
        data.frame()
      if(data_json$map == "Ardennes Forest") {
          #docks
          zone1 = c(435,585)
          data <- data %>%
            mutate(site = ifelse(data.attacker.pos.y < zone1[1],"A",
                                 ifelse(data.attacker.pos.y < zone1[2],"M", "B")))
        }
        else if(data_json$map == "London Docks"){
          zone1 = c(400,550)
          data <- data %>%
            mutate(site = ifelse(data.attacker.pos.x < zone1[1],"A",
                                 ifelse(data.attacker.pos.x < zone1[2],"M", "B")))
        }
        else if(data_json$map == "Sainte Marie du Mont"){
          zone1 = c(390,540)
          data <- data %>%
            mutate(site = ifelse(data.attacker.pos.x < zone1[1],"A",
                                 ifelse(data.attacker.pos.x < zone1[2],"M", "B")))
        }
      else if(data_json$map == "Valkyrie"){
        zone1 = c(600)
        data <- data %>%
          mutate(site = ifelse(data.attacker.pos.y < zone1[1],"A", "B"))
      }
      else {
        #uss texas
        zone1 = c(510)
        data <- data %>%
          mutate(site = ifelse(data.attacker.pos.y < zone1[1],"A", "B"))
      }
      
      # add row plus gun, win and name
      output <- rbind(output, data.frame(map = data_json$map, id = data_json$id, data))
    }
  }
}

#avenges
output %>%
  mutate(otherdeaths = teamdeaths - playerdeaths - teamlastdeaths + lastdeaths) %>%
  group_by(data.id, map) %>%
  # filter(!is.na(avenge)) %>%
  summarise( avenges = sum(avenges)/ sum(otherdeaths)*100, n = sum(otherdeaths)) %>%
  filter(n > 20, map == "London Docks") %>%
  arrange(desc(avenges)) %>% head()

#deaths traded
output %>%
  group_by(data.attacker.id) %>%
  filter(first_kill == 1) %>%
  filter(data.attacker.weapon !="Kar98k" , data.attacker.weapon !="Springfield") %>%
  summarise( trades = 1-mean(trade), n = n()) %>%
  filter(n > 60) %>%
  arrange((trades)) %>% head()

#untraded first bloods per round
output %>%
  group_by(id) %>%
  mutate(rounds_max = max(round)) %>%
  ungroup() %>%
  group_by(id, data.attacker.id) %>% 
  mutate(sniper = ifelse(data.attacker.weapon == "Kar98k" | data.attacker.weapon =="Springfield", 1, 0)) %>%
  summarise(ntfb = sum(first_kill == 1 & trade == 0 & sniper == 0),fbs = sum(first_kill & sniper == 0), rounds = last(rounds_max)) %>% 
  ungroup() %>%
  group_by(data.attacker.id) %>%
  summarise(ntfb_per_rd = sum(ntfb)/sum(fbs),fbs = sum(fbs), rounds = sum(rounds)) %>%
  filter(rounds > 500, data.attacker.id == "TJHALY") %>%
  arrange(desc(ntfb_per_rd)) %>% data.frame() %>% head()
  
# win rate by map by first blood traded
output %>%
  filter(first_kill == 1) %>%
  # group_by(id, map, player.team.x, offdef, round) %>%
  # summarise(win = ifelse(sum(win)>0,1,0), trade = (sum(trade)==1)*1) %>%
  # ungroup() %>%
  group_by(map, offdef, trade) %>%
  summarise(w = mean(win))

# get stats against players
# get stats against players
output1 <- data.frame(event = 'kill', map = output$map, player.1 = output$data.attacker.id,
                      player.2 = output$data.id, trade = output$trade, opp = output$player.team.y, 
                      pos.x = output$data.attacker.pos.x, pos.y = output$data.attacker.pos.y,
                      gun = output$gun.x, round_win = output$win,
                      game_win = output$game_win, offdef = output$offdef,
                      round_time = output$round_time_ms, round = output$round,
                      first_kill = output$first_kill, team = output$player.team.x,
                      id = output$id, site = output$site)
output2 <- data.frame(event = 'death', map = output$map, player.1 = output$data.id,
                      player.2 = output$data.attacker.id, trade = output$trade,
                      opp = output$player.team.x, pos.x = output$data.pos.x,
                      pos.y = output$data.pos.y, gun = output$gun.y, 
                      round_win = ifelse(output$win == 1,0,1),
                      game_win = ifelse(output$game_win == 1,0,1),
                      offdef = ifelse(output$offdef=='off','def','off'),
                      round_time = output$round_time_ms, round = output$round,
                      first_kill = output$first_kill, team = output$player.team.y,
                      id = output$id, site = output$site)
output3 <- rbind(output1, output2)
write.csv(output3, 'allkills_snd.csv')

output3 %>%
  filter(offdef == 'off', round_time < 30000) %>%
  group_by(id, map, round) %>%
  summarise(win = ifelse(sum(round_win)>0,1,0), 
            site = ifelse(sum(site == "A") > sum(site == "B"),1,
                          ifelse(sum(site == "A") < sum(site == "B"),2,3))) %>%
  ungroup() %>%
  group_by(map, site) %>%
  filter(site %in% c(1,2)) %>%
  summarise(win = mean(win), n = n()) %>%
  ungroup() %>%
  group_by(map) %>%
  mutate(attempt_pct = n/sum(n))
  



output3 %>%
  filter(player.1 == "SKRAPZ") %>%
  group_by(player.1, map,offdef) %>%
  summarise(kills = sum(event == 'kill'), deaths = sum(event == 'death'),
            eng = sum(kills,deaths), kd = kills/deaths) %>%
  filter(eng > 0) %>% data.frame() #<- df2

df <- data.frame(expand.grid(player.1 = c("LOONY", "TJHALY","GUNLESS","SLASHER"),
                             win = c(0,1)))
df3 <- merge(df, df2, by = c('player.1','win'))


three <- merge(one, two, by = c('player.1'))
three %>%
  mutate(dif = kd.x/kd.y) %>%
  arrange(desc(dif)) %>% head()


# team wl in snd
output %>%
  group_by(id,map, player.team.x) %>%
  summarise(win = ifelse(sum(game_win)> 0,1,0)) %>%
  ungroup() %>%
  group_by(map, player.team.x) %>%
  summarise(win = sum(win), loss = n()-win, games = n()) %>%
  filter(player.team.x == 'RISE NATION')
