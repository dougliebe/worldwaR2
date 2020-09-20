library(jsonlite)
library(dplyr)
library(ggplot2)
library(scales)
library(png)
library(gganimate)

#set wd
setwd('C:/Users/Doug/Documents/CoD/')

#make df to put output in
output <- data.frame()

# Go through all games all events
location <- list.files(path = 'cwl-data/data/structured', pattern = "structured")
for(j in 1:length(location)) {
  
  filenames <- list.files( path = paste('cwl-data/data/structured/',location[j], sep = ""),pattern="*.json", full.names=TRUE)
  print(location[j]) # show when you start each new event
  
  
  for (i in 1:length(filenames)) {
    
    #read each json file in as list of lists
    data_json <- fromJSON(filenames[i], simplifyVector = T)
    # I filter out by mode,
    # but you can change this to whatever
    if(data_json$mode == "Hardpoint" & !is.null(nrow(data_json$events))){
      
      # get just spawns and deaths
      events <- (data_json$events)
      data <- subset(events, events$type=='spawn'| events$type == 'death')
      
      # make df of players and teams to match 
      team_players <- data.frame(id=data_json$id,name = data_json$players$name, player.team = data_json$players$team, 
                                 gun = data_json$players$fave_weapon,map=data_json$map,
                                 player.opponent=ifelse(data_json$players$team==data_json$teams$name[1],data_json$teams$name[2],data_json$teams$name[1]))
      
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
      data<-merge(data,team_players,by.x='data.id',by.y='name')
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
        data$closerto <- ifelse(data$hp == 1 & data$dist1 == 1, 1, 
                                ifelse(data$hp == 2 & data$dist2 == 1, 1,
                                       ifelse(data$hp == 3 & data$dist3 == 1, 1,
                                              ifelse(data$hp == 4 & data$dist4 == 1, 1,0))))
        
        data$dist2nexthp <- ifelse(data$hp == 1 , data$dist2, 
                                 ifelse(data$hp == 2 , data$dist3,
                                        ifelse(data$hp == 3 , data$dist4,
                                               ifelse(data$hp == 4 , data$dist1, data$dist5))))
        
        data$sdist1 <- (((data$data.pos.x-hill1[1])^2)+((data$data.pos.y-hill1[2])^2) <= (((data$data.pos.x-hill2[1])^2)+((data$data.pos.y-hill2[2])^2)))*1
        data$sdist2 <- (((data$data.pos.x-hill2[1])^2)+((data$data.pos.y-hill2[2])^2) <= (((data$data.pos.x-hill3[1])^2)+((data$data.pos.y-hill3[2])^2)))*1
        data$sdist3 <- (((data$data.pos.x-hill3[1])^2)+((data$data.pos.y-hill3[2])^2) <= (((data$data.pos.x-hill4[1])^2)+((data$data.pos.y-hill4[2])^2)))*1
        data$sdist4 <- (((data$data.pos.x-hill4[1])^2)+((data$data.pos.y-hill4[2])^2) <= (((data$data.pos.x-hill1[1])^2)+((data$data.pos.y-hill1[2])^2)))*1
        data$sdist5 <- 0
        
        data$scloserto <- ifelse(data$type=="death" | (data$hp == 1 & data$sdist1 == 1), 1, 
                                 ifelse(data$type=="death" | (data$hp == 2 & data$sdist2 == 1), 1,
                                        ifelse(data$type=="death" | (data$hp == 3 & data$sdist3 == 1), 1,
                                               ifelse(data$type=="death" | (data$hp == 4 & data$sdist4 == 1), 1,0))))
        # for(x in 1:nrow(data)){
        #   data$anchored <-ifelse(data$scloserto==0 &
        #                            #data$player.team %in% anchor_function2(data,x) &
        #                            !is.na(match(data$player.team,anchor_function2(data,x))) &
        #                            data$dur>40000 &
        #                            anchor_function1(data,x) - data$time_ms > -5000,1,0)
        #   data$anchorplayer <-ifelse(data$scloserto==0 &
        #                                #data$player.team %in% anchor_function2(data,x) &
        #                                !is.na(match(data$player.team,anchor_function2(data,x))) &
        #                                data$dur>40000 &
        #                                anchor_function1(data,x) - data$time_ms > -5000,as.character(anchor_function3(data,x)[match(data$player.team,anchor_function2(data,x))]),0)
        # }
      }
      else {
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
        data$closerto <- ifelse(data$hp == 1 & data$dist1 == 1, 1, 
                                ifelse(data$hp == 2 & data$dist2 == 1, 1,
                                       ifelse(data$hp == 3 & data$dist3 == 1, 1,
                                              ifelse(data$hp == 4 & data$dist4 == 1, 1,
                                                     ifelse(data$hp == 5 & data$dist5 == 1, 1, 0)))))
        
        data$dist2nexthp <- ifelse(data$hp == 1 , data$dist2, 
                               ifelse(data$hp == 2 , data$dist3,
                                      ifelse(data$hp == 3 , data$dist4,
                                             ifelse(data$hp == 4 , data$dist5, data$dist1))))
        
        data$sdist1 <- (((data$data.pos.x-hill1[1])^2)+((data$data.pos.y-hill1[2])^2) <= (((data$data.pos.x-hill2[1])^2)+((data$data.pos.y-hill2[2])^2)))*1
        data$sdist2 <- (((data$data.pos.x-hill2[1])^2)+((data$data.pos.y-hill2[2])^2) <= (((data$data.pos.x-hill3[1])^2)+((data$data.pos.y-hill3[2])^2)))*1
        data$sdist3 <- (((data$data.pos.x-hill3[1])^2)+((data$data.pos.y-hill3[2])^2) <= (((data$data.pos.x-hill4[1])^2)+((data$data.pos.y-hill4[2])^2)))*1
        data$sdist4 <- (((data$data.pos.x-hill4[1])^2)+((data$data.pos.y-hill4[2])^2) <= (((data$data.pos.x-hill5[1])^2)+((data$data.pos.y-hill5[2])^2)))*1
        data$sdist5 <- (((data$data.pos.x-hill5[1])^2)+((data$data.pos.y-hill5[2])^2) <= (((data$data.pos.x-hill1[1])^2)+((data$data.pos.y-hill1[2])^2)))*1
        data$scloserto <- ifelse(data$type=="death" | (data$hp == 1 & data$sdist1 == 1), 1, 
                                 ifelse(data$type=="death" | (data$hp == 2 & data$sdist2 == 1), 1,
                                        ifelse(data$type=="death" | (data$hp == 3 & data$sdist3 == 1), 1,
                                               ifelse(data$type=="death" | (data$hp == 4 & data$sdist4 == 1), 1,
                                                      ifelse(data$type=="death" | (data$hp == 5 & data$sdist5 == 1), 1, 0)))))
        # for(x in 1:nrow(data)){
        #   
        #   data$anchored <-ifelse(data$scloserto==0 &
        #                            !is.na(match(data$player.team[x],anchor_function2(data,x))) &
        #                            data$dur>40000 &
        #                            anchor_function1(data,x) - data$time_ms > -5000,1,0)
        #   data$anchorplayer <-ifelse(data$scloserto==0 &
        #                                !is.na(match(data$player.team[x],anchor_function2(data,x))) &
        #                                data$dur>40000 &
        #                                anchor_function1(data,x) - data$time_ms > -5000,anchor_function3(data,x)[match(data$player.team[x],anchor_function2(data,x))],0)
        #    }
      }
      
      
      
      #  data<-merge(data, team_players, by.x ='data.attacker.id', by.y = 'name')
      #data<-merge(data,team_players,by.x='data.id',by.y='name')
      output<-rbind(output,data)
    }}}


## Make a new dataframe with each kill combined with each instance
# of teammate spawn in the next 5 seconds
anchorKills <- data.frame()
kills <- output %>%
  filter(type == 'death', dur > 40000)
spawn = output %>%
  filter(type == 'spawn', dur > 40000)
for(i in 1:nrow(kills)) {
  team = kills[i, "player.team"]
  game = kills[i, "id"]
  time = kills[i, 'time_ms']
  spawns = subset(spawn, spawn$id == game & spawn$player.team == team & 
                    spawn$time_ms %in% (time+1):(time+5000))
  if(nrow(spawns) > 0){
  anchorKills <- rbind(anchorKills, 
                      data.frame(killer = rep(kills[i,'data.id'], nrow(spawns)),
                      dist = spawns$dist2nexthp,
                      map = spawns$map, hp = spawns$hp))
}}


#function to get last death
anchor_function1<-function(data,x){
  d1<-data[1:x,]
  d1$time_ms[max(which(d1$type=='death'))]
}
anchor_function2<-function(data,x){
  d1<-data[1:x,]
  c(as.character(d1$player.opponent[max(which(d1$type=='death'))]),
    as.character(d1$player.opponent[max(which(d1$type=='death'))-1]),
    as.character(d1$player.opponent[max(which(d1$type=='death'))-2]),
    as.character(d1$player.opponent[max(which(d1$type=='death'))-3]),
    as.character(d1$player.opponent[max(which(d1$type=='death'))-4]),
    as.character(d1$player.opponent[max(which(d1$type=='death'))-5]))
}
anchor_function3<-function(data,x){
  d1<-data[1:x,]
  c(as.character(d1$data.attacker.id[max(which(d1$type=='death'))]),
    as.character(d1$data.attacker.id[max(which(d1$type=='death'))-1]),
    as.character(d1$data.attacker.id[max(which(d1$type=='death'))-2]),
    as.character(d1$data.attacker.id[max(which(d1$type=='death'))-3]),
    as.character(d1$data.attacker.id[max(which(d1$type=='death'))-4]),
    as.character(d1$data.attacker.id[max(which(d1$type=='death'))-5]))
}


h=dim(img)[1]
w=dim(img)[2]

gib1 <- subset(output,output$map.x == "London Docks" &  output$dur > 40000 & output$hp == 1 & output$closerto==0)
gib2 <- subset(output,output$map.x == "London Docks" &  output$dur > 40000 & output$hp == 2 & output$closerto==0)
gib3 <- subset(output,output$map.x == "London Docks" &  output$dur > 40000 & output$hp == 3 & output$closerto==0)
gib4 <- subset(output,output$map.x == "London Docks" &  output$dur > 40000 & output$hp == 4 & output$closerto==0)
gib5 <- subset(output,output$map.x == "London Docks" &  output$dur > 40000 & output$hp == 5 & output$closerto==0)
one<-ggplot(data=gib1) +
  annotation_custom(grid::rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), 0, w, 0,-h) +
  theme_bw() + theme(legend.position = 'none',axis.title = element_blank()) +
  coord_equal() + # To keep the aspect ratio of the image.
  scale_x_continuous(expand=c(0,0),limits=c(0,w)) +
  scale_y_reverse(expand=c(0,0),limits=c(h,0)) +
  stat_density2d(aes(x=data.pos.x,y=data.pos.y, fill=..level..,alpha=(..level..)),h=150,n=900,geom='polygon') +
  scale_fill_gradientn(colours=c('purple','blue',"light blue"),name = "Frequency") +
  guides(alpha="none")

two<-ggplot(data=gib1) +
  annotation_custom(grid::rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), 0, w, 0,-h) +
  theme_bw() + theme(legend.position = 'none',axis.title = element_blank()) +
  coord_equal() + # To keep the aspect ratio of the image.
  scale_x_continuous(expand=c(0,0),limits=c(0,w)) +
  scale_y_reverse(expand=c(0,0),limits=c(h,0)) +
  stat_density2d(aes(x=data.attacker.pos.x,y=data.attacker.pos.y, fill=..level..,alpha=(..level..)),h=150,n=900,geom='polygon') +
  scale_fill_gradientn(colours=c('yellow','orange',"red"),name = "Frequency") +
  guides(alpha="none")





two<-ggplot(data=gib2) +
  annotation_custom(grid::rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), 0, w, 0,-h) +
  theme_bw() + theme(legend.position = 'none',axis.title = element_blank()) +
  coord_equal() + # To keep the aspect ratio of the image.
  scale_x_continuous(expand=c(0,0),limits=c(0,w)) +
  scale_y_reverse(expand=c(0,0),limits=c(h,0)) +
  stat_density2d(aes(x=data.attacker.pos.x,y=data.attacker.pos.y, fill=..level..,alpha=(..level..)),h=150,n=900,geom='polygon') +
  scale_fill_gradientn(colours=c('green','yellow',"red"),name = "Frequency") +
  guides(alpha="none")
three<-ggplot(data=gib3) +
  annotation_custom(grid::rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), 0, w, 0,-h) +
  theme_bw() + theme(legend.position = 'none',axis.title = element_blank()) +
  coord_equal() + # To keep the aspect ratio of the image.
  scale_x_continuous(expand=c(0,0),limits=c(0,w)) +
  scale_y_reverse(expand=c(0,0),limits=c(h,0)) +
  stat_density2d(aes(x=data.attacker.pos.x,y=data.attacker.pos.y, fill=..level..,alpha=(..level..)),h=150,n=900,geom='polygon') +
  scale_fill_gradientn(colours=c('green','yellow',"red"),name = "Frequency") +
  guides(alpha="none")
four<-ggplot(data=gib4) +
  annotation_custom(grid::rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), 0, w, 0,-h) +
  theme_bw() + theme(legend.position = 'none',axis.title = element_blank()) +
  coord_equal() + # To keep the aspect ratio of the image.
  scale_x_continuous(expand=c(0,0),limits=c(0,w)) +
  scale_y_reverse(expand=c(0,0),limits=c(h,0)) +
  stat_density2d(aes(x=data.attacker.pos.x,y=data.attacker.pos.y, fill=..level..,alpha=(..level..)),h=150,n=900,geom='polygon') +
  scale_fill_gradientn(colours=c('green','yellow',"red"),name = "Frequency") +
  guides(alpha="none")
five<-ggplot(data=gib5) +
  annotation_custom(grid::rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), 0, w, 0,-h) +
  theme_bw() + theme(legend.position = 'none',axis.title = element_blank()) +
  coord_equal() + # To keep the aspect ratio of the image.
  scale_x_continuous(expand=c(0,0),limits=c(0,w)) +
  scale_y_reverse(expand=c(0,0),limits=c(h,0)) +
  stat_density2d(aes(x=data.attacker.pos.x,y=data.attacker.pos.y, fill=..level..,alpha=(..level..)),h=150,n=900,geom='polygon') +
  scale_fill_gradientn(colours=c('green','yellow',"red"),name = "Frequency") +
  guides(alpha="none")
grid.arrange(one,two, ncol=2)




kills<-output %>%
  mutate(rot = ifelse(closerto ==0 & dur > 40000,1,0))%>%
  filter(rot == 1) %>%
  group_by(data.attacker.id) %>%
  #group_by(data.attacker.id) %>%
  summarise(kills=n())
deaths<-output %>%
  mutate(rot = ifelse(closerto == 0 & dur > 40000,1,0))%>%
  filter(rot == 1) %>%
  group_by(data.id) %>%
  #group_by(data.id) %>%
  summarise(deaths=n())
KD<-merge(kills,deaths, by.x=c("data.attacker.id"),by.y=c('data.id'))
#KD<-merge(kills,deaths, by.x=c("data.attacker.id"),by.y=c('data.id'))
KD$KD <- KD$kills/KD$deaths
KD$n<-KD$kills+KD$deaths
KD %>%
  filter (n > 300)%>%
  arrange(desc(KD))%>%
  head()


kills<-output %>%
  mutate(rot = ifelse(closerto ==0 & dur > 40000,1,0))%>%
  filter(rot == 1) %>%
  group_by(data.attacker.id, map.x) %>%
  summarise(kills=n())
deaths<-output %>%
  mutate(rot = ifelse(closerto == 0 & dur > 40000,1,0))%>%
  filter(rot == 1) %>%
  group_by(data.id, map.x) %>%
  summarise(deaths=n())
KD<-merge(kills,deaths, by.x=c("data.attacker.id", 'map.x'),by.y=c('data.id', 'map.x'))
KD$KD <- KD$kills/KD$deaths
KD$n<-KD$kills+KD$deaths
KD %>%
  filter (n > 150)%>%
  arrange(desc(KD))%>%
  head()

kills<-output %>%
  mutate(rot = ifelse(closerto ==0 & dur > 40000,1,0))%>%
  filter(rot == 1) %>%
  group_by(player.team.x) %>%
  summarise(kills=n())
deaths<-output %>%
  mutate(rot = ifelse(closerto == 0 & dur > 40000,1,0))%>%
  filter(rot == 1) %>%
  group_by(player.team.y) %>%
  summarise(deaths=n())
KD<-merge(kills,deaths, by.x=c("player.team.x"),by.y=c('player.team.y'))
KD$KD <- KD$kills/KD$deaths
KD$n<-KD$kills+KD$deaths
KD %>%
  filter (n > 100)%>%
  arrange(desc(KD))%>%
  head()

# 
# cum <- output %>%
#   mutate(rot = ifelse(dur > 40000,1,0)) %>%
#   filter(rot == 1) %>%
#   group_by(map, hp, dur) %>% 
#   summarise(n = n()) %>%
#   ungroup() %>%
#   group_by(map, hp) %>%
#   mutate(cs = cumsum(n))
# ggplot(cum[cum$map == "London Docks",] , aes(dur, cs, color = as.factor(hp)))+geom_line(size = 2)+
#   scale_color_brewer(palette = 'Set1')