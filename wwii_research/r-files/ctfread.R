library(jsonlite)
library(dplyr)
library(ggplot2)
library(scales)
library(png)
# library(gganimate)

setwd('C:/Users/Doug/Documents/CoD/')
img <- readPNG('cwl-data/maps/ww2/london_docks.png')
output <- data.frame()

# Go through all games all events
location <- list.files(path = 'cwl-data/data/structured', pattern = "structured")
for(j in 7:(length(location))) {
  filenames <- list.files( path = paste('cwl-data/data/structured/',location[j], sep = ""),pattern="*.json", full.names=TRUE)
  print(location[j]) # show when you start each new event
  
  
  for (i in 1:length(filenames)) {
    
    #read each json file in as list of lists
    data_json <- fromJSON(filenames[i], simplifyVector = T)
    # print(length(data_json$events$data))
    # I filter out by mode,
    # but you can change this to whatever
    if(data_json$mode == "Capture The Flag" & !is.null(nrow(data_json$events))){

      # get just spawns and deaths
      events <- (data_json$events)
      data <- subset(events, events$type == 'death')

      # make df of players and teams to match
      team_players <- data.frame(name = data_json$players$name, player.team = data_json$players$team,
                                 gun = data_json$players$fave_weapon,map=data_json$map,
                                 win = c(rep(data_json$teams$is_victor[[1]]*1,4),rep(data_json$teams$is_victor[[2]]*1,4)))

      data = do.call(cbind.data.frame, data)
      data = do.call(cbind.data.frame, data)
      data = do.call(cbind.data.frame, data)



      data<-merge(data, team_players, by.x ='data.attacker.id', by.y = 'name')
      data<-merge(data,team_players,by.x='data.id',by.y='name')
      if(('data.is_overtime' %in% colnames(data))==F) {
        data$data.is_overtime = 0
      }
      output<-rbind(output,data.frame(id = data_json$id, data))
    }}}



# get stats against players
output1 <- data.frame(event = 'kill', map = output$map.x, player.1 = output$data.attacker.id,
                      player.2 = output$data.id, opp = output$player.team.y, 
                      pos.x = output$data.attacker.pos.x, pos.y = output$data.attacker.pos.y,
                      gun = output$gun.x, team = output$player.team.x, id = output$id,
                      game_win = output$win.x)
output2 <- data.frame(event = 'death', map = output$map.x, player.1 = output$data.id,
                      player.2 = output$data.attacker.id,
                      opp = output$player.team.x, team = output$player.team.y, pos.x = output$data.pos.x,
                      pos.y = output$data.pos.y, gun = output$gun.y, id = output$id,
                      game_win = ifelse(output$win.x == 1,0,1))
output3 <- rbind(output1, output2)
write.csv(output3, 'allkills_ctf2.csv')
