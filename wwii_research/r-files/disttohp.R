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
      data <- subset(events, events$type == 'death')
      
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


        data$dist1 <- (((data$data.attacker.pos.x-hill1[1])^2)+((data$data.attacker.pos.y-hill1[2])^2) <= 250^2)*1
        data$dist2 <- (((data$data.attacker.pos.x-hill2[1])^2)+((data$data.attacker.pos.y-hill2[2])^2) <= 250^2)*1
        data$dist3 <- (((data$data.attacker.pos.x-hill3[1])^2)+((data$data.attacker.pos.y-hill3[2])^2) <= 250^2)*1
        data$dist4 <- (((data$data.attacker.pos.x-hill4[1])^2)+((data$data.attacker.pos.y-hill4[2])^2) <= 250^2)*1
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
        data$dist1 <- (((data$data.attacker.pos.x-hill1[1])^2)+((data$data.attacker.pos.y-hill1[2])^2) <= 250^2)*1
        data$dist2 <- (((data$data.attacker.pos.x-hill2[1])^2)+((data$data.attacker.pos.y-hill2[2])^2) <= 250^2)*1
        data$dist3 <- (((data$data.attacker.pos.x-hill3[1])^2)+((data$data.attacker.pos.y-hill3[2])^2) <= 250^2)*1
        data$dist4 <- (((data$data.attacker.pos.x-hill4[1])^2)+((data$data.attacker.pos.y-hill4[2])^2) <= 250^2)*1
        data$dist5 <- (((data$data.attacker.pos.x-hill5[1])^2)+((data$data.attacker.pos.y-hill5[2])^2) <= 250^2)*1
        data$closerto <- ifelse(data$hp == 1 & data$dist1 == 1, 1, 
                                ifelse(data$hp == 2 & data$dist2 == 1, 1,
                                       ifelse(data$hp == 3 & data$dist3 == 1, 1,
                                              ifelse(data$hp == 4 & data$dist4 == 1, 1,
                                                     ifelse(data$hp == 5 & data$dist5 == 1, 1, 0)))))
        
        }

        
      
      data<-merge(data, team_players, by.x ='data.attacker.id', by.y = 'name')
      output<-rbind(output,data)
    }}}

h=dim(img)[1]
w=dim(img)[2]

gib1 <- subset(output,output$map == "Gibraltar" &  output$dur > 40000 & output$hp == 2)
ggplot(data=gib1) +
  annotation_custom(grid::rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), 0, w, 0,-h) +
  theme_bw() + theme(legend.position = 'none',axis.title = element_blank()) +
  coord_equal() + # To keep the aspect ratio of the image.
  scale_x_continuous(expand=c(0,0),limits=c(0,w)) +
  scale_y_reverse(expand=c(0,0),limits=c(h,0)) +
  geom_point(aes(data.attacker.pos.x,data.attacker.pos.y,color = as.factor(closerto))) +
  scale_color_manual(values=c("#CC6666", "#9999CC")) +
  geom_point(aes(x = hill3[1], y = hill3[2]), shape = 17, size = 5, color = 'yellow') +
  geom_point(aes(x = hill2[1], y = hill2[2]), shape = 17, size = 5, color = 'yellow') +
  # stat_bin2d(binwidth=3,aes(x,y,alpha=(..count..)^(1/2))) +
  # stat_binhex(aes(x=x, y=y, alpha=(..density..)^1), bins = 40) +
  # scale_fill_gradientn(colours=c( ('green'), ('yellow'),("red")),name = "Frequency") +
  # guides(alpha="none") +
  # ggtitle(label = paste(team1, " setup on HP #", hpt, sep = ""))
  ggtitle(label = "Last 20 seconds of Hill #2 - Gibraltar")


kills<-output %>%
  mutate(rot = ifelse(closerto ==0 & dur > 40000,1,0))%>%
  filter(rot == 1) %>%
  group_by(data.attacker.id, map) %>%
  summarise(kills=n())
deaths<-output %>%
  mutate(rot = ifelse(closerto == 0 & dur > 40000,1,0))%>%
  filter(rot == 1) %>%
  group_by(data.id, map) %>%
  summarise(deaths=n())
KD<-merge(kills,deaths, by.x=c("data.attacker.id", 'map'),by.y=c('data.id', 'map'))
KD$KD <- KD$kills/KD$deaths
KD$n<-KD$kills+KD$deaths
KD %>%
  filter (n > 35, map == "Valkyrie")%>%
  arrange(desc(KD))%>%
  head()



cum <- output %>%
  mutate(rot = ifelse(dur > 40000,1,0)) %>%
  filter(rot == 1) %>%
  group_by(map, hp, closerto,  dur) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(map, hp, closerto) %>%
  mutate(cs = cumsum(n)) %>%
  ungroup() %>%
  group_by(map, hp) %>%
  mutate(pct_engage = cs) %>%
  filter(closerto == 0)
ggplot(cum[cum$map == "Sainte Marie du Mont",] , aes(dur/1000, pct_engage, color = as.factor(hp)))+geom_line(size = 2)+
  scale_color_brewer(palette = 'Set1', name = "Hill Rotation", labels = c("1 to 2", "2 to 3", "3 to 4", "4 to 1", "5 to 1")) +
  ylab("Rotation Engagements") + xlab("Current Hill Duration") + ggtitle("St. Marie Engagements \nAway from Current HP")
