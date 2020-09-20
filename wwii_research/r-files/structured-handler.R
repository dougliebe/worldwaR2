library(jsonlite)
library(ggplot2)
library(scales)
library(png)

setwd('C:/Users/Doug/Documents/CoD/')
img <- readPNG('cwl-data/maps/ww2/sainte_marie_du_mont.png')

filenames <- list.files( path = 'cwl-data/data/structured/structured-2018-06-17-anaheim',pattern="*.json", full.names=TRUE)

output <- data.frame()

for (i in 1:length(filenames)) {
data_json <- read_json(filenames[i], simplifyVector = T)
if(data_json$map == "Sainte Marie du Mont" & !is.null(nrow(data_json$events))){
  print('yes')
events <- (data_json$events)
data <- subset(events, events$type == 'death')

team_players <- data.frame(name = data_json$players$name, player.team = data_json$players$team)

start_time = 5000

hp <- rep(seq(1,4),4, each = 60*1000)
time <- seq(start_time, start_time+length(hp)-1) # shift 10 secs to account for rotation
set <- rep(seq(1,4),each = 60*1000*4)
df <- data.frame(time,hp, set)
data = merge(data, df, by.x = 'time_ms', by.y = 'time')
new <- merge(data$data, team_players, by.x = 'id', by.y = 'name')
new$attacker <- merge(new$attacker, team_players,by.x = 'id', by.y = 'name')
output <- rbind(output, data.frame(kill = 0, new$pos, hp = data$hp))
output <- rbind(output, data.frame(kill = 1, new$attacker$pos, hp = data$hp))
}}


h = dim(img)[1]
w = dim(img)[2]
data1 <- subset(output, output$kill == 0 & !is.na(output$x) & output$hp == 3)
data2 <- subset(output, output$kill == 1 & !is.na(output$x) & output$hp == 3)

## Show the differences in where we score and don't
library(reshape2) # For melt function
library(MASS)

show_set = c(1,2,3,4)
hpt = 1

# data1 <- data$data$pos
# data2 <- data$data$attacker$pos


# Calculate the common x and y range 
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
diff12$z = pmax(pmin(d2$z / (d1$z+1E-9),1.5),0.5)
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
  stat_contour(aes(color=..level..), lwd = 1) +
  scale_fill_gradient2(low="red", high="green", midpoint=0) +
  scale_colour_gradient2(low= ('red'), mid = muted("tan"), high=("green"), midpoint=0.9,name = "Probability") +
  coord_equal() +
  guides(alpha="none") +
  scale_x_continuous(expand=c(0,0),limits=c(0,w)) +
  scale_y_reverse(expand=c(0,0),limits=c(h,0)) +
  theme(panel.background = element_blank(),axis.title = element_blank(),
                               legend.position = 'right') #+ggtitle("Likelihood of Breaking \nHardpoint (from position)")


l_break <- function(x,y,data) {
  test = data
  test$a <- sqrt((data$x - x)^2+(data$y - y)^2)
  return(mean(test[test$a <= 10,'z']))
} 

l_break(646,220, diff12.m)
l_break(80,140, diff12.m)
l_break(150,150, diff12.m)

# ggplot(events, aes(data$pos$x, data$pos$y, col = round))+geom_point()
# Plot difference between geyser2 and geyser1 density
ggplot(output, aes(x, y)) +
  annotation_custom(grid::rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), 0, w, 0, -h) +
  geom_tile() +
  geom_point(col = 'red') +
  coord_equal() +
  guides(alpha="none") +
  scale_x_continuous(expand=c(0,0),limits=c(0,w)) +
  scale_y_reverse(expand=c(0,0),limits=c(h,0)) +
  theme(panel.background = element_blank(),axis.title = element_blank(),
        legend.position = 'right') #+ggtitle("Likelihood of Breaking \nHardpoint (from position)")

