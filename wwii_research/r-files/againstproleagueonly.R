#snd ratings against pro league teams only

library(dplyr)
library(ggplot2)
library(ggpubr)
setwd("C:/Users/Doug/Documents/CoD/cwl-data/data")
# read in the csv
filenames <- list.files( pattern="*.csv", full.names=TRUE)

output <- data.frame()

for (i in 1:(length(filenames))) {
  data <- read.csv(filenames[i])
  data <- subset(data, data$mode == 'Search & Destroy') # sort by mode
  output <- rbind(output, data.frame(data, event = filenames[i]))
}
m <- readRDS('sndrating.rds')
summary(m)

match_team<-subset(output,select=c("match.id","team"))
output$opptest<-match_team$team[match(output$match.id,match_team$match.id)]
output$opptest2<-match_team$team[match(output$match.id,match_team$match.id)+4]
output$opponent<-ifelse(output$team==output$opptest,as.character(output$opptest2),as.character(output$opptest))
output = subset(output, select = -c(opptest,opptest2) )

#filter out stage two opponents only
team_filter = c('FaZe Clan','OpTic Gaming','Luminosity','Luminosity Gaming',
                'Team Kaliber','Red Reserve','eUnited','Rise Nation','Team EnVyUs',
                'Team Envy','Echo Fox','Unilad','Complexity','Complexity Gaming',
                'Splyce','Mindfreak','Evil Geniuses','Tainted Minds','Epsilon')

output <- subset(output, output$opponent %in% team_filter)
head(output)

rating <- output %>%
  mutate(kpr = kills/snd.rounds) %>%
  mutate(ksapr = kills..stayed.alive./snd.rounds) %>%
  mutate(dpr = deaths/snd.rounds) %>%
  mutate(apr = assists/snd.rounds) %>%
  mutate(fpr = snd.firstbloods/snd.rounds) %>%
  mutate(mkpr = (snd.2.kill.round+snd.4.kill.round+snd.3.kill.round)/snd.rounds) %>%
  mutate(kas = (kills+assists+snd.survives)/snd.rounds) %>%
  mutate(sepr = scorestreaks.earned/snd.rounds) %>%
  mutate(fdr = (snd.firstdeaths)/snd.rounds) %>%
  
  mutate(win = ifelse(win. == "W",1,0))

rating$rating  <- predict(m,rating,  type = 'response')
rating$rating <- rating$rating/mean(rating$rating, na.rm = T)
#get ratings
rating %>%
  group_by(player) %>%
  summarise(rating_avg = mean(rating, na.rm = T), k = sum(kills), ntk = sum(kills..stayed.alive.),
            a = sum(assists), `fb:fd` = paste(sum(snd.firstbloods),sum(snd.firstdeaths),sep = ":"), n = n()) %>%
  filter(n > 5) %>%
  arrange(desc(rating_avg)) %>% head(20)
