library(ggplot2)
library(RColorBrewer)
library(png)
library(scales)
library(hexbin)
library(grid)
library(gridExtra)

setwd("/home/dougliebe/Documents/CoD")
img <- readPNG('marie.png')
left <- read.csv('csvs/taintopticmarie.csv')
right <- read.csv('csvs/opticrisemarie.csv')
colnames(left) <- c('team','x','y','area','time')
colnames(right) <- c('team','x','y','area','time')
score <- read.csv('gamescore2.csv')
left <- merge(left, score, by = 'time')
right <- merge(right, score, by = 'time')
colnames(left) <- c('time','y','x','area','coL', "Unilad",'dif')
colnames(right) <- c('time','y','x','area','coL', "Unilad",'dif')

# right and left sep
# left$x <- left$x - min(left$x)
# left$y <- left$y - min(left$y)
# 

data = rbind(data.frame(left, side = 'left'), data.frame(right, side = 'right'))
# data = subset(data, data$time > 34)
data$x <- data$x - min(data$x)
data$y <- data$y - min(data$y)
w = max(data$y)
h = max(data$x)
return_corrected <- function(data) {
  output = data.frame()#time = NA, x = NA, y = NA, hp = NA, set = NA)
  for(i in 1:nrow(data)) {
    for(z in 1:(round(data[i,'area']/300)+1)){
      output <- rbind(output, data[i,])
    }}
  return(output)
}

data <- return_corrected(data)

start_time = 24

hp <- rep(seq(1,4),4, each = 60)
time <- seq(start_time, start_time+length(hp)-1) # shift 10 secs to account for rotation
df <- data.frame(time,hp)
data = merge(data, df, by = 'time')

data$set <- ifelse(data$time < 60*4+start_time, 1,2)
data$comeback <- ifelse(data$time > 456, 1, 0)
# data$a <- ((data$x-125)^2)/125^2 + ((data$y-60)^2)/60^2
# data <- data[data$a <= 1,]
show_set = 2
hpt = 1
data1 <- subset(data, data$side == 'right' & data$comeback == 1 & data$hp == hpt)
data2 <- subset(data, data$side == 'right' & data$comeback == 0 & data$hp == hpt)
# left <- subset(left, time < 75)


one <- ggplot(data=data1) +
  annotation_custom(grid::rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), 0, w, 0,-h) +
  theme_bw() + theme(legend.position = 'none',axis.title = element_blank(), axis.text = element_blank()) +
  coord_equal() + # To keep the aspect ratio of the image.
  scale_x_continuous(expand=c(0,0),limits=c(5,w-10)) +
  scale_y_reverse(expand=c(0,0),limits=c(h,0)) +
  stat_density2d(aes(y,x, fill=..level..,alpha=(..level..)-0.1),geom='polygon', h = 30, n = 500) +
  # stat_bin2d(binwidth=3,aes(x,y,alpha=(..count..)^(1/2))) +
  # stat_binhex(aes(x=x, y=y, alpha=(..density..)^1), bins = 40) +
  scale_fill_gradientn(colours=c( ('green'), ('yellow'),("red")),name = "Frequency") +
  guides(alpha="none") +
  ggtitle(label = paste("Hardpoint",hpt, 'before comeback'))
two <- ggplot(data=data2) +
  annotation_custom(grid::rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), 0, w, 0, -h) +
  theme_bw() + theme(legend.position = 'none',axis.title = element_blank(), axis.text = element_blank()) +
  coord_equal() + # To keep the aspect ratio of the image.
  scale_x_continuous(expand=c(0,0),limits=c(5,w-10)) +
  scale_y_reverse(expand=c(0,0),limits=c(h,0)) +
  stat_density2d(aes(y,x, fill=..level..,alpha=(..level..)-0.1),geom='polygon', h = 30, n = 500) +
  # stat_bin2d(binwidth=3,aes(x,y,alpha=(..count..)^(1/2))) +
  # stat_binhex(aes(x=x, y=y, alpha=(..count..)^1), bins = 40) +
  scale_fill_gradientn(colours=c( ('green'), ('yellow'),("red")),name = "Frequency") +
  ggtitle(label = paste("Hardpoint",hpt, 'after comeback'))
  
grid.arrange(one,two, ncol = 2)

# Overlap two densities

ggplot(rbind(data.frame(data1, group="a"), data.frame(data2, group="b")),aes(x=y,y=x)) +
  annotation_custom(grid::rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), 0, w, 0, -h) +
  theme_bw() + theme(legend.position = 'none',axis.title = element_blank(), axis.text = element_blank()) +
  coord_equal() + # To keep the aspect ratio of the image.
  scale_x_continuous(expand=c(0,0),limits=c(5,w-10)) +
  scale_y_reverse(expand=c(0,0),limits=c(h,0)) +
  stat_density2d(geom="polygon", h = 25, n = 100, aes(fill = group, alpha=(..level..)^(1/2))) +
  scale_fill_manual(values=c("a"="red", "b"="blue")) +
  guides(alpha="none") +
  xlab("Range Transformed Units")

library(reshape2) # For melt function
library(MASS)


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
diff12$z = d2$z - d1$z

## Melt data into long format
# First, add row and column names (x and y grid values) to the z-value matrix
rownames(diff12$z) = diff12$x
colnames(diff12$z) = diff12$y

# Now melt it to long format
diff12.m = melt(diff12$z, id.var=rownames(diff12))
names(diff12.m) = c("Duration","Waiting","z")

# Plot difference between geyser2 and geyser1 density
ggplot(diff12.m, aes(Duration, Waiting, z=z, fill=z)) +
  geom_tile() +
  stat_contour(aes(color=..level.., alpha = (..level..)^2), binwidth=5) +
  scale_fill_gradient2(low="red",mid="white", high="blue", midpoint=0) +
  scale_colour_gradient2(low=muted("red"), mid="white", high=muted("blue"), midpoint=0) +
  coord_equal() +
  scale_x_continuous(expand=c(0,0),limits=c(5,w-10)) +
  scale_y_reverse(expand=c(0,0),limits=c(h,0)) +
  guides(colour=FALSE)

mycol <- rgb(0, 0, 0, max = 255, alpha = 125)
ggplot(data=diff12.m) +
  annotation_custom(grid::rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), 0, w, 0, -h) +
  theme_bw() + theme(legend.position = 'none',axis.title = element_blank(), axis.text = element_blank()) +
  coord_equal() + # To keep the aspect ratio of the image.
  scale_x_continuous(expand=c(0,0),limits=c(5,w-10)) +
  scale_y_reverse(expand=c(0,0),limits=c(h,0)) + 
  stat_density2d(aes(Duration,Waiting,alpha=(..level..)^6),geom='polygon', h = 30) +
  # stat_bin2d(binwidth=3,aes(x,y,alpha=(..count..)^(1/2))) +
  # stat_binhex(aes(x=x, y=y, alpha=(..count..)^1), bins = 40) +
  # scale_colour_gradient2(low=muted("red"), mid="white", high=muted("blue"), midpoint=0) +
  scale_fill_gradientn(colours=c( 'green', 'yellow',"red"),name = "Frequency") +
  guides(alpha="none") +
  xlab("Range Transformed Units")

ggplot(diff12.m, aes(Duration, Waiting, z=z, fill=z)) +
  annotation_custom(grid::rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), 0, w, 0, -h) +
  geom_tile() +
  coord_equal() +
  stat_contour(aes(alpha=..level..), binwidth=0.1) +
  # scale_fill_gradient2(low="red",mid="white", high="blue", midpoint=0) +
  scale_colour_gradient2(low=muted("red"), mid="white", high=muted("blue"), midpoint=0) +
  coord_cartesian(xlim=xrng, ylim=yrng) +
  # scale_x_continuous(expand=c(0,0),limits=c(5,w-10)) +
  scale_y_reverse() +
  guides(colour=FALSE)
