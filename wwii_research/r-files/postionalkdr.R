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
for(j in 1:length(location)) {
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
      output <- rbind(output, data.frame(map = data_json$map, data, event = location[j],id = data_json$id))
      # output <- rbind(output, data.frame(map = data_json$map, offdef = data$offdef, x = data$data.attacker.pos.x, y = data$data.attacker.pos.y))
      # output <- rbind(output, data.frame(pt_chart, map = data_json$map, id = data_json$id, code = data_json$series_id, event = location[j]))
    }
  }
}

# output %>%
#   mutate(ss = ifelse(data.attacker.means_of_death == 'projectile_splash' | data.attacker.means_of_death == 'explosive_bullet',1,0)) %>%
#   group_by(data.attacker.id, map) %>%
#   summarise(ss = sum(ss)/length(unique(id)), n = n()) %>%
#   filter(n > 400) %>% arrange(desc(ss))
# k = output %>%
#   group_by(data.attacker.id, map) %>%
#   summarise(kills = n(), n = n()) %>%
#   filter(n > 300, map == 'Sainte Marie du Mont') %>% arrange(desc(kills))
# d = output %>%
#   group_by(data.id, map) %>%
#   summarise(deaths = n(), n = n()) %>%
#   filter(n > 300, map == 'Sainte Marie du Mont') %>% arrange(desc(deaths))
# kd <- merge(k,d, by.x = c('data.attacker.id'), by.y = c('data.id'))
# kd$kd <- kd$kills/kd$deaths
# kd %>%
#   mutate(n = kills+deaths) %>%
#   filter(n > 300) %>%
#   arrange(desc(kd))


data1 <-subset(output, output$map == "London Docks" & output$hp == 2 & !is.na(output$data.pos.x) & !is.na(output$data.attacker.pos.x))
# Calculate the common x and y range for geyser1 and geyser2
xrng = range(c(data1$data.attacker.pos.x,data1$data.pos.x))
yrng = range(c(data1$data.attacker.pos.y,data1$data.pos.y))

# Calculate the 2d density estimate over the common range
d1 = kde2d(data1$data.attacker.pos.x, data1$data.attacker.pos.y, lims=c(xrng, yrng), n=500)
d2 = kde2d(data1$data.pos.x, data1$data.pos.y, lims=c(xrng, yrng), n=500)

# Confirm that the grid points for each density estimate are identical
identical(d1$x, d2$x) # TRUE
identical(d1$y, d2$y) # TRUE

# Calculate the difference between the 2d density estimates
diff12 = d1 
# diff12$z = d1$z - d2$z
diff12$z = round(d1$z / (d2$z+d1$z),2)
m <- mean(diff12$z)
s <- sd(diff12$z)
# diff12$z <- scale(diff12$z)
## Melt data into long format
# First, add row and column names (x and y grid values) to the z-value matrix
rownames(diff12$z) = diff12$x
colnames(diff12$z) = diff12$y

# Now melt it to long format
diff12.m = melt(diff12$z, id.var=rownames(diff12))
names(diff12.m) = c("x","y","z")
diff12.m[diff12.m$z >= 0.75,'z'] <- 0.5
diff12.m[diff12.m$z <= 0.25,'z'] <- 0.5


h = dim(img)[1]
w = dim(img)[2]
# Plot difference between geyser2 and geyser1 density
ggplot(diff12.m, aes(x, y, z=z, alpha=0.4)) +
  annotation_custom(grid::rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), 0, w, 0, -h) +
  # geom_tile() +
  geom_raster(aes(fill = z) ) +
  # stat_summary_hex(fun = function(x) sum(x)) +
  # stat_density_2d() +
  scale_fill_gradient2(low="red", high="green", midpoint=0.5, name = "P(kill)\n") +
  # scale_colour_gradient2(low= ('red'), mid = muted("tan"), high=("green"), midpoint=0.4,name = "Probability") +
  coord_equal() +
  guides(alpha="none") +
  scale_x_continuous(expand=c(0,0),limits=c(0,w)) +
  scale_y_reverse(expand=c(0,0),limits=c(h,0)) +
  theme(panel.background = element_blank(),axis.title = element_blank(),
        legend.position = 'right') +ggtitle("KDR by position \nLondon Docks - HP #1")



