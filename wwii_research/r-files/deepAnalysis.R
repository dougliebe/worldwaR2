library(ggplot2)
library(RColorBrewer)
library(png)
library(scales)
library(hexbin)
library(grid)
library(gridExtra)
library(dplyr)


setwd("/home/dougliebe/Documents/CoD")
team1 = 'eunited'
team2 = 'optic'
map = 'gib'
start_time = 1

img <- readPNG(paste(map,'.png', sep = ''))
# game1 <- read.csv(paste('csvs/', team1, team2, map,'.csv',sep = ""),header= F)
# score <- read.csv(paste('scores/',team1, team2, map,'score.csv',sep = ""),header= F)
colnames(game1) <- c('team','x','y','area','time', 'opp', 'score', 'map')
# colnames(score) <- c('left','right','time')
# game1 <- merge(game1, score, by = 'time')


game1$x <- game1$x - min(game1$x)
game1$y <- game1$y - min(game1$y)
w = max(game1$x)
h = max(game1$y)
return_corrected <- function(data) {
  output = data.frame()
  for(i in 1:nrow(data)) {
    for(z in 1:(max(2,round(data[i,'area']/300)+1))){
      output <- rbind(output, data[i,])
    }}
  return(output)
}

data <- return_corrected(game1)

#Plot game score
ggplot(data) + geom_line(aes(time, score, color = team)) +
  theme_bw() + ylab('Score') + xlab('time') +theme(text = element_text(size = 18))


hp <- rep(seq(1,4),5, each = 60)
set <- rep(seq(1,4), each = 60*5)
# hp <- rep(seq(1,5),5, each = 60)
# set <- rep(seq(1,5), each = 60*5)
time <- seq(start_time, start_time+length(hp)-1)
df <- data.frame(time,hp,set)
data = merge(data, df, by = 'time')
# data$comeback <- ifelse(data$time > 400, 1, 0)

show_set = c(1,2,3,4)
hpt = c(4)
data1 <- subset(data,  data$set %in% show_set & data$hp == 4)
data2 <- subset(data, data$ == 'eunited' & data$set %in% show_set & data$hp == 4)
data3 <- subset(data, data$team == 'eunited' & data$set %in% show_set & data$hp == 3)
data4 <- subset(data, data$team == 'eunited' & data$set %in% show_set & data$hp == 4)

#Plots
one <- ggplot(data=data1) +
  annotation_custom(grid::rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), 0, w, 0,-h) +
  theme_bw() + theme(legend.position = 'none',axis.title = element_blank(), axis.text = element_blank()) +
  coord_equal() + # To keep the aspect ratio of the image.
  scale_x_continuous(expand=c(0,0),limits=c(0,w)) +
  scale_y_reverse(expand=c(0,0),limits=c(h,0)) +
  stat_density2d(aes(x,y, fill=..level..,alpha=(..level..)^2),geom='polygon', h = 30, n = 500) +
  # stat_bin2d(binwidth=3,aes(x,y,alpha=(..count..)^(1/2))) +
  # stat_binhex(aes(x=x, y=y, alpha=(..density..)^1), bins = 40) +
  scale_fill_gradientn(colours=c( ('green'), ('yellow'),("red")),name = "Frequency") +
  guides(alpha="none") +
  # ggtitle(label = paste(team1, " setup on HP #", hpt, sep = ""))
  ggtitle(label = "Overtake densities")

two <- ggplot(data=data2) +
  annotation_custom(grid::rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), 0, w, 0, -h) +
  theme_bw() + theme(legend.position = 'none',axis.title = element_blank(), axis.text = element_blank()) +
  coord_equal() + # To keep the aspect ratio of the image.
  scale_x_continuous(expand=c(0,0),limits=c(0,w)) +
  scale_y_reverse(expand=c(0,0),limits=c(h,0)) +
  stat_density2d(aes(x,y, fill=..level..,alpha=(..level..)^2),geom='polygon', h = 30, n = 500) +
  # stat_bin2d(binwidth=3,aes(x,y,alpha=(..count..)^(1/2))) +
  # stat_binhex(aes(x=x, y=y, alpha=(..count..)^1), bins = 40) +
  scale_fill_gradientn(colours=c( ('green'), ('yellow'),("red")),name = "Frequency") +
  # ggtitle(label = paste(team2, " setup on HP #", hpt, sep = ""))
  ggtitle(label = "Failure densities")
three <- ggplot(data=data3) +
  annotation_custom(grid::rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), 0, w, 0,-h) +
  theme_bw() + theme(legend.position = 'none',axis.title = element_blank(), axis.text = element_blank()) +
  coord_equal() + # To keep the aspect ratio of the image.
  scale_x_continuous(expand=c(0,0),limits=c(0,w)) +
  scale_y_reverse(expand=c(0,0),limits=c(h,0)) +
  stat_density2d(aes(x,y, fill=..level..,alpha=(..level..)^2),geom='polygon', h = 30, n = 500) +
  # stat_bin2d(binwidth=3,aes(x,y,alpha=(..count..)^(1/2))) +
  # stat_binhex(aes(x=x, y=y, alpha=(..density..)^1), bins = 40) +
  scale_fill_gradientn(colours=c( ('green'), ('yellow'),("red")),name = "Frequency") +
  guides(alpha="none") +
  # ggtitle(label = paste(team1, " setup on HP #", hpt, sep = ""))
  ggtitle(label = "HP #3")

four <- ggplot(data=data4) +
  annotation_custom(grid::rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), 0, w, 0, -h) +
  theme_bw() + theme(legend.position = 'none',axis.title = element_blank(), axis.text = element_blank()) +
  coord_equal() + # To keep the aspect ratio of the image.
  scale_x_continuous(expand=c(0,0),limits=c(0,w)) +
  scale_y_reverse(expand=c(0,0),limits=c(h,0)) +
  stat_density2d(aes(x,y, fill=..level..,alpha=(..level..)^2),geom='polygon', h = 30, n = 500) +
  # stat_bin2d(binwidth=3,aes(x,y,alpha=(..count..)^(1/2))) +
  # stat_binhex(aes(x=x, y=y, alpha=(..count..)^1), bins = 40) +
  scale_fill_gradientn(colours=c( ('green'), ('yellow'),("red")),name = "Frequency") +
  # ggtitle(label = paste(team2, " setup on HP #", hpt, sep = ""))
  ggtitle(label = "HP #4")


# grid.arrange(one,two,three,four, ncol = 4)
grid.arrange(one,two,ncol = 2)

data %>%
  # filter(time > 400) %>%
  group_by(team, opp) %>%
  arrange(time) %>%
  mutate(gain = (score - lag(score, 1))) %>%
  ungroup() %>%
  group_by(team, opp,set, hp) %>%
  summarise(l = sum(gain, na.rm = T)) %>%
  ungroup() %>%
  group_by(hp, set) %>%
  summarise(sd = sd(l, na.rm = T)) %>% 
  summarise(sd = mean(sd)) %>% data.frame()

ttt <- data %>%
  group_by(team) %>%
  mutate(gain = score - lag(score,1)) %>%
  ungroup() %>%
  group_by(team, time) %>%
  summarise(gain = ifelse(sum(gain)>0, 1,0)) %>%
  ungroup() %>%
  mutate(scoring = ifelse(gain - lag(gain, 1) > 0, 1, 0)) %>%
  mutate(overtake = ifelse(scoring == 0 & ((lead(scoring, 2) == 1) |
                             (lead(scoring, 3) == 1) |
                           (lead(scoring, 4) == 1) |
                           (lead(scoring, 5) == 1) |
                           (lead(scoring, 6) == 1) |
                           (lead(scoring, 7) == 1) |
                           (lead(scoring, 1) == 1)) , 1, 0)) %>%
  data.frame()
data <- merge(data, ttt[,c(1,2,4,5)])


## Show the differences in where we score and don't
library(reshape2) # For melt function
library(MASS)

show_set = c(1,2,3,4)
hpt = 1
data1 <- subset(data, data$scoring == 0 & data$set %in% show_set & data$hp == hpt & data$overtake == 1)
data2 <- subset(data, data$scoring == 0 &data$set %in% show_set & data$hp == hpt & data$overtake == 0)

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


l_break <- function(x,y,data) {
  test = data
  test$a <- sqrt((data$x - x)^2+(data$y - y)^2)
  return(mean(test[test$a <= 10,'z']))
} 

l_break(180,60, diff12.m)
l_break(80,140, diff12.m)
l_break(150,150, diff12.m)
