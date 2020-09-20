library(jsonlite)
library(dplyr)
library(ggplot2)
library(scales)
library(png)
library(gridExtra)
library(RColorBrewer)
# library(gganimate)

setwd('C:/Users/Doug/Documents/CoD/')
img <- readPNG('cwl-data/maps/ww2/sainte_marie_du_mont.png')
output <- data.frame()

# Go through all games all events
location <- list.files(path = 'cwl-data/data/structured', pattern = "structured")
for(j in 1:length(location)) {
  filenames <- list.files( path = paste('cwl-data/data/structured/',location[j], sep = ""),pattern="*.json", full.names=TRUE)
  print(location[j])
  
  
  for (i in 1:length(filenames)) {
    
    #read each json file in as list of lists
    data_json <- fromJSON(filenames[i], simplifyVector = T)
    # I filter out by mode,
    # but you can change this to whatever
    if(data_json$mode == "Hardpoint" &  !is.null(nrow(data_json$events))){
      
      # get just spawns and deaths
      events <- (data_json$events)
      data <- subset(events, events$type == 'spawn')
      
      # make df of players and teams to match 
      team_players <- data.frame(name = data_json$players$name, player.team = data_json$players$team, gun = data_json$players$fave_weapon,map=data_json$map)
      
      data = do.call(cbind.data.frame, data)
      data = do.call(cbind.data.frame, data)
      data = do.call(cbind.data.frame, data)
      
      #start_time=45000
      start_time=5000
      
      # # Split by hp and set
      # if (data_json$mode=="Hardpoint"){
      #   rot <- rep(seq(1,3),4,each = 20*1000)
      #   time <- seq(start_time, start_time+length(rot)-1) 
      #   df <- data.frame(time,rot)
      #   data = merge(data, df, by.x = 'time_ms', by.y = 'time')
      # }
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
          hill2 = c(350,125)
          hill3 <- c(650,800)
          hill4 <- c(250, 525)
        }
        
        
        data$dist1 <- sqrt(((data$data.pos.x-hill1[1])^2)+((data$data.pos.y-hill1[2])^2))
        data$dist2 <- sqrt(((data$data.pos.x-hill2[1])^2)+((data$data.pos.y-hill2[2])^2))
        data$dist3 <- sqrt(((data$data.pos.x-hill3[1])^2)+((data$data.pos.y-hill3[2])^2))
        data$dist4 <- sqrt(((data$data.pos.x-hill4[1])^2)+((data$data.pos.y-hill4[2])^2))
        data$dist5 <- 0
        data$closerto <- ifelse(data$hp == 1,as.numeric(data$dist1), 
                                ifelse(data$hp == 2,as.numeric(data$dist2), 
                                       ifelse(data$hp == 3,as.numeric(data$dist3), 
                                              ifelse(data$hp == 4,as.numeric(data$dist4), 0))))
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
        data$dist1 <- sqrt(((data$data.pos.x-hill1[1])^2)+((data$data.pos.y-hill1[2])^2))
        data$dist2 <- sqrt(((data$data.pos.x-hill2[1])^2)+((data$data.pos.y-hill2[2])^2))
        data$dist3 <- sqrt(((data$data.pos.x-hill3[1])^2)+((data$data.pos.y-hill3[2])^2))
        data$dist4 <- sqrt(((data$data.pos.x-hill4[1])^2)+((data$data.pos.y-hill4[2])^2))
        data$dist5 <- sqrt(((data$data.pos.x-hill5[1])^2)+((data$data.pos.y-hill5[2])^2))
        data$closerto <- ifelse(data$hp == 1, as.numeric(data$dist1), 
                                ifelse(data$hp == 2, as.numeric(data$dist2),
                                       ifelse(data$hp == 3, as.numeric(data$dist3),
                                              ifelse(data$hp == 4, as.numeric(data$dist4),
                                                     ifelse(data$hp == 5, as.numeric(data$dist5), 0)))))
        
      }
      
      
      
      data<-merge(data, team_players, by.x ='data.id', by.y = 'name')
      data$opp <- ifelse(data$player.team == data_json$teams$name[1], as.character(data_json$teams$name[2]),as.character(data_json$teams$name[1]))
      output<-rbind(output,data.frame(map = data_json$map, data, event = location[j],id = data_json$id))
    }}}

head(output)

output %>%
  filter(dur < 40000) %>%
  filter(event %in% c('structured-2018-04-22-seattle','structured-2018-06-17-anaheim')) %>%
  group_by(opp, map) %>%
  summarise(d = mean(closerto), n = n()) %>%
  filter(n > 100) %>%
  ungroup() %>%
  group_by(map) %>%
  mutate(dist = d/mean(d)) %>%

  arrange(desc(dist)) %>% data.frame()
