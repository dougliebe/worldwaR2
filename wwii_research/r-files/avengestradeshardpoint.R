library(jsonlite)
library(ggplot2)
library(scales)
library(png)
library(dplyr)

setwd('C:/Users/Doug/Documents/CoD/')
img <- readPNG('cwl-data/maps/ww2/sainte_marie_du_mont.png')

# set blank output
output <- data.frame()
start_time = 5000

# For each event folder
location <- list.files(path = 'cwl-data/data/structured', pattern = "structured")
for(j in 5:(length(location))) {
  filenames <- list.files( path = paste('cwl-data/data/structured/',location[j], sep = ""),pattern="*.json", full.names=TRUE)
  print(location[j])
  
  # for each game in each event folder
  for (i in 1:length(filenames)) {
    #read json
    data_json <- fromJSON(filenames[i], simplifyVector = T)
    # filter out by mode if desired
    if(data_json$mode == "Hardpoint" & !is.null(nrow(data_json$events))){
      
      
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
        dur <- (time-start_time)%%60000
        set <- rep(seq(1,4),each = 60*1000*4)
        df <- data.frame(time, dur,hp, set)
        data = merge(data, df, by.x = 'time_ms', by.y = 'time')
        # only works for this hardpoint example with 4 hps
        # you can add others tho
        if(data_json$map == "Gibraltar") {
          #gibraltar
          hill1 = c(500,500)
          hill2 = c(750, 500)
          hill3 = c(300, 375)
          hill4 = c(550, 700)
        } else if (data_json$map == "Ardennes Forest") {
          # ardennes
          hill1 = c(525,220)
          hill2 = c(500,750)
          hill3 <- c(375,375)
          hill4 <- c(600, 500)
        } else if (data_json$map == "Sainte Marie du Mont") {
          # st marie
          hill1 = c(500,525)
          hill2 = c(300,200)
          hill3 <- c(650,800)
          hill4 <- c(250, 525)
        }
        
        
        data$dist1 <- (((data$data.attacker.pos.x-hill1[1])^2)+((data$data.attacker.pos.y-hill1[2])^2) <= 150^2)*1
        data$dist2 <- (((data$data.attacker.pos.x-hill2[1])^2)+((data$data.attacker.pos.y-hill2[2])^2) <= 150^2)*1
        data$dist3 <- (((data$data.attacker.pos.x-hill3[1])^2)+((data$data.attacker.pos.y-hill3[2])^2) <= 150^2)*1
        data$dist4 <- (((data$data.attacker.pos.x-hill4[1])^2)+((data$data.attacker.pos.y-hill4[2])^2) <= 150^2)*1
        data$dist5 <- 0
        data$closerto <- ifelse(data$hp == 1 & data$dist1 == 1, 1, 
                                ifelse(data$hp == 2 & data$dist2 == 1, 1,
                                       ifelse(data$hp == 3 & data$dist3 == 1, 1,
                                              ifelse(data$hp == 4 & data$dist4 == 1, 1,0))))
      } else {
        hp <- rep(seq(1,5),4, each = 60*1000)
        time <- seq(start_time, start_time+length(hp)-1) # shift 10 secs to account for rotation
        dur <- (time-start_time)%%60000
        set <- rep(seq(1,5),each = 60*1000*4)
        df <- data.frame(time,dur,hp, set)
        
        data = merge(data, df, by.x = 'time_ms', by.y = 'time')
        if( data_json$map == "London Docks") {
          #london docks
          hill1 = c(500,550)
          hill2 = c(700,650)
          hill3 <- c(250,550)
          hill4 <- c(550, 350)
          hill5 <- c(425, 900)
        } else {
          # valk
          hill1 = c(525,475)
          hill2 = c(475,275)
          hill3 <- c(475,740)
          hill4 <- c(300, 425)
          hill5 <- c(750, 625)
        }
        data$dist1 <- (((data$data.attacker.pos.x-hill1[1])^2)+((data$data.attacker.pos.y-hill1[2])^2) <= 150^2)*1
        data$dist2 <- (((data$data.attacker.pos.x-hill2[1])^2)+((data$data.attacker.pos.y-hill2[2])^2) <= 150^2)*1
        data$dist3 <- (((data$data.attacker.pos.x-hill3[1])^2)+((data$data.attacker.pos.y-hill3[2])^2) <= 150^2)*1
        data$dist4 <- (((data$data.attacker.pos.x-hill4[1])^2)+((data$data.attacker.pos.y-hill4[2])^2) <= 150^2)*1
        data$dist5 <- (((data$data.attacker.pos.x-hill5[1])^2)+((data$data.attacker.pos.y-hill5[2])^2) <= 150^2)*1
        data$closerto <- ifelse(data$hp == 1 & data$dist1 == 1, 1, 
                                ifelse(data$hp == 2 & data$dist2 == 1, 1,
                                       ifelse(data$hp == 3 & data$dist3 == 1, 1,
                                              ifelse(data$hp == 4 & data$dist4 == 1, 1,
                                                     ifelse(data$hp == 5 & data$dist5 == 1, 1, 0)))))
        
      }
      
      data <- merge(data, team_players, by.x = 'data.attacker.id', by.y = 'name')
      data <- merge(data, team_players, by.x ='data.id', by.y = 'name')
      data <- merge(data, pt_chart, by.x = c('team.x', 'hp', 'set'), by.y = c('team','hp','set'))
      data <- data %>%
        mutate(killDist = round(sqrt((data.attacker.pos.x-data.pos.x)^2+(data.attacker.pos.y-data.pos.y)^2))) %>%
        mutate(game_win = ifelse(team.x == data_json$teams$name[[1]],
                                 data_json$teams$is_victor[[1]]*1,data_json$teams$is_victor[[2]]*1))
      data <- data %>%
        group_by(round) %>%
        arrange(time_ms) %>%
        mutate(trade = ifelse(round_time_ms == max(round_time_ms),2,
                              ifelse((as.character(data.attacker.id) == as.character(lead(data.id ,1))) &
                                       (lead(time_ms,1) - time_ms < 5000),1,0))) %>%
        mutate(killtrade = ifelse(round_time_ms == max(round_time_ms),2,
                              ifelse((as.character(data.attacker.id) == as.character(lead(data.id ,1))) &
                                       (lead(time_ms,1) - time_ms < 5000) & (as.character(data.id) == as.character(lag(data.attacker.id ,1))),1,0))) %>%
        mutate(avenge = ifelse(lag(trade,1) == 1, 1,0)) %>%
        data.frame()
      # add row plus gun, win and name
      output <- rbind(output, data.frame(map = data_json$map, id = data_json$id, data, date = data_json$start_time_s))
    }
  }
}

# add distances of kills and AR or sub kill
kd1 = output %>%
  # mutate(killDistance = round(sqrt((data.attacker.pos.x-data.pos.x)^2+(data.attacker.pos.y-data.pos.y)^2))) %>%
  mutate(killWeapon = ifelse(gun.x %in% c("FG 42", 'BAR', 'STG-44'), "AR","SUB")) %>%
  mutate(deathWeapon = ifelse(gun.y %in% c("FG 42", 'BAR', 'STG-44'), "AR","SUB")) %>%
  group_by(killDist) %>%
  summarise(kd = sum(killWeapon == "AR")/sum(deathWeapon == "AR"), n = n()) %>%
  filter(n > 100)
kd = rbind(data.frame(gun = "AR",kd1),data.frame(gun = 'sub',kd2))

output %>% 
  filter(trade != 2) %>%
  group_by(data.id) %>%
  summarise(killtrade = sum(killtrade, na.rm = T)/sum(trade), n = n()) %>%
  filter(n > 1500) %>%
  ungroup() %>%
  summarise(mean = mean(killtrade))
  arrange(desc(killtrade)) %>%
  data.frame() %>% head(25)
  

#deaths traded
output %>%
  filter(trade != 2) %>%
  group_by(data.id) %>%
  summarise( trades = mean(trade), n = n()) %>%
  filter( n > 1500) %>%
  arrange(desc(trades)) %>% head()

output %>%
  group_by(id,team.x, team.y) %>%
  summarise(win = ifelse(sum(game_win)>0,1,0)) %>% 
  ungroup() %>%
  group_by(team.x, team.y) %>%
  summarise(win = mean(win), n = n()) %>%
  filter(team.x=="EUNITED") %>% data.frame()
  ungroup() %>%
  group_by(data.attacker.id) %>%
  summarise(avengepct = sum(avenge, na.rm = T)/sum(deaths), deaths = sum(deaths)) %>%
  filter(deaths > 300) %>%
  arrange(desc(avengepct))
  



# get stats against players
output1 <- data.frame(event = 'kill', map = output$map, player.1 = output$data.attacker.id,
                      player.2 = output$data.id, trade = output$trade, opp = output$opp, 
                      pos.x = output$data.attacker.pos.x, pos.y = output$data.attacker.pos.y,
                      gun = output$gun.x, team = output$team.x, id = output$id,
                      win = output$game_win, hp = output$hp, score = output$score,
                      date = output$date, closerto = output$closerto, hpdur = output$dur,
                      killDist = output$killDist)
output2 <- data.frame(event = 'death', map = output$map, player.1 = output$data.id,
                      player.2 = output$data.attacker.id, trade = output$trade,
                      opp = output$team.x, team = output$team.y, pos.x = output$data.pos.x,
                      pos.y = output$data.pos.y, gun = output$gun.y, id = output$id,
                      win = ifelse(output$game_win == 1,0,1), hp = output$hp, score = -output$score,
                      date = output$date, closerto = output$closerto, hpdur = output$dur,
                      killDist = output$killDist)
output3 <- rbind(output1, output2)
write.csv(output3, 'allkills_hp2.csv')


df2 <- output3 %>%
  # filter(trade != 1) %>%
  group_by(player.1, player.2) %>%
  summarise(kills = sum(event == 'kill'), deaths = sum(event == 'death'),
            eng = sum(kills,deaths), kd = kills/deaths) %>%
  filter(eng > 10) %>% data.frame()

df <- data.frame(expand.grid(player.1 = c("LOONY", "TJHALY","GUNLESS","SLASHER"),
                             player.2 = c("SKRAPZ","JOEE","ZER0","RATED")))
df3 <- merge(df, df2, by = c('player.1','player.2'))

df3 %>%
  group_by(player.1) %>%
  summarise(kills = sum(kills), deaths = sum(deaths),
            eng = sum(kills,deaths), kd = kills/deaths) %>%
  filter(eng > 10) %>% data.frame()




# Percent of AR deaths outside 100 units
output3 %>%
  mutate(isAR = ifelse(gun %in% c("FG 42", 'BAR', 'STG-44'), 1,0)) %>%
  filter(isAR == 0) %>%
  group_by(player.1) %>%
  summarise(pctLS = sum(killDist < 100, na.rm = T)/n(),n = n(), kd = sum(event == 'kill')/sum(event == 'death')) %>%
  filter(n > 1000) %>% arrange((pctLS))


output3 %>%
  mutate(isAR = ifelse(gun %in% c("FG 42", 'BAR', 'STG-44'), 1,0)) %>%
  group_by(player.1) %>%
  summarise(killDist = mean(killDist, na.rm = T), n = n(), ar = mean(isAR)) %>%
  filter(n > 1000) %>% arrange(desc(killDist))
