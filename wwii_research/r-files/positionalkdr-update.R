library(jsonlite)
library(ggplot2)
library(scales)
library(png)
library(dplyr)

setwd('C:/Users/Doug/Documents/CoD/')
img <- readPNG('cwl-data/maps/ww2/ardennes_forest.png')

# set blank output
output <- data.frame()
start_time = 5000

# For each event folder
location <- list.files(path = 'cwl-data/data/structured', pattern = "structured")
for(j in 6:(length(location))) {
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




# get stats against players
output1 <- data.frame(event = 'kill', map = output$map, player.1 = output$data.attacker.id,
                      player.2 = output$data.id, trade = output$trade, opp = output$opp, 
                      pos.x = output$data.attacker.pos.x, pos.y = output$data.attacker.pos.y,
                      gun = output$gun.x, team = output$team.x, id = output$id,
                      win = output$game_win, hp = output$hp, score = output$score,
                      date = output$date, closerto = output$closerto, hpdur = output$dur)
output2 <- data.frame(event = 'death', map = output$map, player.1 = output$data.id,
                      player.2 = output$data.attacker.id, trade = output$trade,
                      opp = output$team.x, team = output$team.y, pos.x = output$data.pos.x,
                      pos.y = output$data.pos.y, gun = output$gun.y, id = output$id,
                      win = ifelse(output$game_win == 1,0,1), hp = output$hp, score = -output$score,
                      date = output$date, closerto = output$closerto, hpdur = output$dur)
output3 <- rbind(output1, output2)

# make set for average
h = dim(img)[1]
w = dim(img)[2]

mapp = 'Valkyrie'

data1 <- subset(output3, output3$map == mapp & output3$event == 'kill' & !is.na(output3$pos.x))
data2 <- subset(output3, output3$map == mapp & output3$event == 'death' & !is.na(output3$pos.x))

# Calculate the common x and y range for geyser1 and geyser2
xrng = c(0,1024) #range(c(data1$pos.x, data2$pos.x))
yrng = c(0,1024) #range(c(data1$pos.y, data2$pos.y))

# Calculate the 2d density estimate over the common range
d1 = kde2d(data1$pos.x, data1$pos.y, lims=c(xrng, yrng), n=128)
d2 = kde2d(data2$pos.x, data2$pos.y, lims=c(xrng, yrng), n=128)

# Confirm that the grid points for each density estimate are identical
identical(d1$x, d2$x) # TRUE
identical(d1$y, d2$y) # TRUE

# Calculate the difference between the 2d density estimates
diff12 = d1 
# diff12$z = d1$z - d2$z
diff12$z = d1$z / (d1$z+d2$z)
m <- mean(diff12$z)
s <- sd(diff12$z)
# diff12$z <- scale(diff12$z)
## Melt data into long format
# First, add row and column names (x and y grid values) to the z-value matrix
rownames(diff12$z) = diff12$x
colnames(diff12$z) = diff12$y
# Now melt it to long format
LD_poskd = melt(diff12$z, id.var=rownames(diff12))
names(LD_poskd) = c("x","y","z")



ggplot(LD_poskd, aes(x, y, z=z, alpha=0.3)) +
  annotation_custom(grid::rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), 0, w, 0, -h) +
  geom_tile() +
  stat_contour(aes(color=..level..), lwd = 1) +
  # scale_fill_gradient2(low="red", high="green", midpoint=0) +
  scale_colour_gradient2(low= ('red'), mid = muted("tan"), high=("green"), midpoint=0.4,name = "Probability") +
  coord_equal() +
  guides(alpha="none") +
  scale_x_continuous(expand=c(0,0),limits=c(0,w)) +
  scale_y_reverse(expand=c(0,0),limits=c(h,0)) +
  theme(panel.background = element_blank(),axis.title = element_blank(),
        legend.position = 'right') +ggtitle("Likelihood of Breaking \nHardpoint (from position)")



LD_poskd = read.csv('LDposkd.csv')
VK_poskd = read.csv('VKposkd.csv')
AF_poskd = read.csv('AFposkd.csv')
GB_poskd = read.csv('GBposkd.csv')
SM_poskd = read.csv('SMposkd.csv')
total_poskd <- rbind(data.frame(map = 'London Docks', LD_poskd),
                     data.frame(map = 'Ardennes Forest', AF_poskd))
total_poskd <- rbind(total_poskd,
                     data.frame(map = 'Gibraltar', GB_poskd))
total_poskd <- rbind(total_poskd,
                     data.frame(map = 'Sainte Marie du Mont', SM_poskd))
total_poskd <- rbind(total_poskd,
                     data.frame(map = 'Valkyrie', VK_poskd))
# make funct to find closest values
finditx <- function(x) {
  return(LD_poskd$x[which.min(abs(LD_poskd$x - x))])
}

findity <- function(x) {
  return(LD_poskd$y[which.min(abs(LD_poskd$y - x))])
}

output3$adj.x <- sapply(output3$pos.x, finditx)
output3$adj.y <- sapply(output3$pos.y, findity)
output3 <- merge(output3,total_poskd, by.x = c('map','adj.x','adj.y'), by.y = c('map','x','y'))


output3 %>%
  mutate(actual = ifelse(event == 'kill', 1-z, 0-z)) %>%
  group_by(player.1) %>%
  # filter(z < 0.510 & z > 0.490) %>%
  summarise(m = mean(z), kills = sum(event == 'kill'), deaths = sum(event == 'death'),
            eng = sum(kills,deaths), kd = kills/deaths, exp_kd = sum(z) /(eng-sum(z)),
            diff = kills - sum(z),d2 = sum(actual), m_act = kills/eng) %>%
  # filter(eng > 1000) %>%
  arrange(desc(diff)) %>%
  head(10)
