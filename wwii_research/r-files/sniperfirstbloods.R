library(jsonlite)
library(ggplot2)
library(scales)
library(png)
library(dplyr)

setwd('C:/Users/Doug/Documents/CoD/')
img <- readPNG('cwl-data/maps/ww2/sainte_marie_du_mont.png')

# set blank output
output <- data.frame()

# For each event folder
location <- list.files(path = 'cwl-data/data/structured', pattern = "structured")
for(j in 1:length(location)) {
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
  
    # #if kill was earliest in round, label first blood
    # data <- data %>%
    #   dplyr::group_by(round) %>%
    #   dplyr::mutate(first_kill = ifelse(round_time_ms == min(round_time_ms),1,0)) %>%
    #   data.frame() %>%
    #   filter(first_kill == 1) # take only first bloods
    data <- merge(data, team_players, by.x = 'data.attacker.id', by.y = 'name') # name players
    data <- merge(data, team_players, by.x = 'data.id', by.y = 'name') 
    data <- merge(data, team_rounds, by.x = c('player.team.x', 'round'),
                  by.y = c('player.team', 'round')) # win or not?
    # add row plus gun, win and name
    output <- rbind(output, data.frame(map = data_json$map, id = data_json$id, data))
}
}
}


h = dim(img)[1]
w = dim(img)[2]
valk <- subset(output, output$map == "Sainte Marie du Mont" & output$time_ms < 60000 & output$offdef == 'off')
ggplot(data=valk) +
  annotation_custom(grid::rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), 0, w, 0,-h) +
  theme_bw() + theme(legend.position = 'none',axis.title = element_blank(), axis.text = element_blank()) +
  coord_equal() + # To keep the aspect ratio of the image.
  scale_x_continuous(expand=c(0,0),limits=c(0,w)) +
  scale_y_reverse(expand=c(0,0),limits=c(h,0)) +
  stat_density2d(aes(data.attacker.pos.x,data.attacker.pos.y, fill=..level..,alpha=(..level..)^2),
                 geom='polygon', h = 75) +
  # stat_bin2d(binwidth=3,aes(x,y,alpha=(..count..)^(1/2))) +
  # stat_binhex(aes(x=x, y=y, alpha=(..density..)^1), bins = 40) +
  scale_fill_gradientn(colours=c( ('green'), ('yellow'),("red")),name = "Frequency") +
  guides(alpha="none") +
  # ggtitle(label = paste(team1, " setup on HP #", hpt, sep = ""))
  ggtitle(label = "AR kill first 60 sec - Offense")


data1 <- subset(output, output$map == "Valkyrie" & output$time_ms < 60000 & output$offdef == 'off' & output$data.attacker.weapon == 'STG-44')

# Calculate the common x and y range for geyser1 and geyser2
xrng = range(c(data1$x, data2$x))
yrng = range(c(data1$y, data2$y))

# Calculate the 2d density estimate over the common range
d1 = kde2d(data1$x, data1$y, lims=c(xrng, yrng), n=500)
d2 = kde2d(data2$x, data2$y, lims=c(xrng, yrng), n=500)

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
diff12.m = melt(diff12$z, id.var=rownames(diff12))
names(diff12.m) = c("x","y","z")

# Plot difference between geyser2 and geyser1 density
ggplot(diff12.m, aes(x, y, z=z, alpha=0.3)) +
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


output %>%
  dplyr::group_by(map, data.attacker.means_of_death) %>%
  dplyr::summarise(win = mean(win), n = n()) %>%
  filter(n > 20) %>%
  arrange(desc(win)) %>% data.frame()

output %>%
  mutate(nade = ifelse(data.attacker.means_of_death == 'grenade_splash',1,0)) %>%
  dplyr::group_by( id, round, data.attacker.id) %>%
  dplyr::summarise(nade_per_round = sum(nade)) %>%
  ungroup() %>%
  group_by( data.attacker.id) %>%
  summarise(nade_rd = mean(nade_per_round), n = n()) %>%
  filter(n > 50) %>%
  arrange(desc(nade_rd))
