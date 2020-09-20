library(httr)
library(jsonlite)
library(lubridate)
library(RCurl)
library(dplyr)

setwd('C:/Users/doug/Documents/CoD/cod')

Mode = function(x){ 
  ta = table(x)
  tam = max(ta)
  if (all(ta == tam))
    mod = NA
  else
    if(is.numeric(x))
      mod = as.numeric(names(ta)[ta == tam])
  else
    mod = names(ta)[ta == tam]
  return(mod)
}

url  <- "https://stats.gammaray.io/api/v1/report/cwl-champs-2018/playermatches/"

raw <- getURL(url = url)
data <- read.csv (text = raw)
data$date <- as.Date(gsub( " .*$", "", data$end.time ))

hppred <- readRDS('files/hprating.rds')
sndpred <- readRDS('files/sndrating.rds')
ctfpred <- readRDS('files/ctfrating.rds')


# Rating 0.1 snd
rating <- data %>%
  filter(mode == 'Search & Destroy') %>%
  mutate(kpr = kills/snd.rounds) %>%
  mutate(ksapr = kills..stayed.alive./snd.rounds) %>%
  mutate(dpr = deaths/snd.rounds) %>%
  mutate(apr = assists/snd.rounds) %>%
  mutate(fpr = snd.firstbloods/snd.rounds) %>%
  mutate(mkpr = (snd.2.kill.round+snd.4.kill.round+snd.3.kill.round)/snd.rounds) %>%
  mutate(sepr = scorestreaks.earned/snd.rounds) %>%
  # mutate(dpk = hits/(kills+1E-9)) %>%
  mutate(win = ifelse(win. == "W",1,0))
rating$rating  = round(exp(predict(sndpred,rating))/(1+exp(predict(sndpred, rating))),2)

# Rating 0.1 hp
ratinghp <- data %>%
  filter(mode == 'Hardpoint') %>%
  mutate(kpr = kills/duration..s.*600) %>%
  mutate(ksapr = kills..stayed.alive./duration..s.*600) %>%
  mutate(dpr = deaths/duration..s.*600) %>%
  mutate(apr = assists/duration..s.*600) %>%
  mutate(fpr = snd.firstbloods/snd.rounds) %>%
  mutate(mkpr = (snd.2.kill.round+snd.4.kill.round+snd.3.kill.round)/duration..s.*600) %>%
  mutate(sepr = scorestreaks.earned/duration..s.*600) %>%
  mutate(win = ifelse(win. == "W",1,0))
ratinghp$rating  = round(exp(predict(hppred,ratinghp))/(1+exp(predict(hppred, ratinghp))),2)

# Rating 0.1 ctf
ratingctf <- data %>%
  filter(mode == 'Capture The Flag') %>%
  mutate(kpr = kills/duration..s.*600) %>%
  mutate(ksapr = kills..stayed.alive./duration..s.*600) %>%
  mutate(dpr = deaths/duration..s.*600) %>%
  mutate(fpr = snd.firstbloods/snd.rounds) %>%
  mutate(apr = assists/duration..s.*600) %>%
  mutate(mkpr = (snd.2.kill.round+snd.4.kill.round+snd.3.kill.round)/duration..s.*600) %>%
  mutate(sepr = scorestreaks.earned/duration..s.*600) %>%
  mutate(win = ifelse(win. == "W",1,0))
ratingctf$rating  = round(exp(predict(ctfpred,ratingctf))/(1+exp(predict(ctfpred, ratingctf))),2)

rating <- rbind(rating, ratinghp)
rating <- rbind(rating, ratingctf)

# MVP rating
rating %>%
  # filter(mode == "Search & Destroy") %>%
  filter(date >= as.Date('2018-08-17')) %>%
  # mutate(half = ifelse(date < '2018-05-01', 1 , 2)) %>%
  # filter(half == 1 ) %>%
  group_by(player) %>%
  summarise(rating = round(mean(ifelse(mode == "Search & Destroy", rating*0.9805488,
                                       ifelse(mode == "Hardpoint", rating*1.2504342, rating*0.7690170)), na.rm = T),3), maps = n(), games = length(unique(series.id)),
            utk = round(sum(kills..stayed.alive.)/sum(kills),3),fbr = round(sum(snd.firstbloods)/sum(num.lives),3),
            fbfd = paste(sum(snd.firstbloods), sum(snd.firstdeaths), sep = ":"),
            kd = round(sum(kills)/sum(deaths),2),
            utkdr = sum(kills..stayed.alive.)/sum(deaths),
            gun = Mode(fave.weapon)[[1]],
            fbfdr = sum(snd.firstbloods)/(sum(snd.firstbloods)+sum(snd.firstdeaths)),
            sub.use = sum(fave.weapon == 'PPSh-41')/n(),
            eng.per10 = (sum(kills)+sum(deaths))/sum(duration..s.)*600,
            kpr = sum(kills)/maps, rounds = sum(snd.rounds)/maps) %>%
  ungroup() %>%
  # filter(games > 30) %>%
  # group_by(mode) %>%
  mutate(rank.utk = rank(-utk,ties.method = 'min'),
         rank.rating = rank(-rating,ties.method = 'min'),
         eng.rating = eng.per10* rating) %>%
  group_by(gun) %>%
  mutate(pos.rating = (rating - mean(rating))/mean(rating)) %>%
  filter(maps >= 20) %>%
  # filter(player == 'Scump') %>%
  arrange(desc(utkdr)) %>%
  data.frame() %>% head(20)

data %>%
  group_by(team,player, series.id) %>%
  summarise(utk = sum(kills..stayed.alive.)/sum(kills), utks = sum(kills..stayed.alive.), kills = sum(kills), n = n()) %>%
  # filter(team == "Red Reserve") %>%
  # filter(n > 5) %>%
  arrange(desc(utk)) %>%
  data.frame() %>% head()

rating %>%
  # filter(date >= as.Date('2018-08-14')) %>%
  group_by(team, match.id) %>%
  mutate(wins.above = rating  -  mean(rating)) %>%
  ungroup() %>%
  group_by(player) %>%
  summarise(wins.above = mean(wins.above),
            sub.use = sum(fave.weapon == 'PPSh-41')/n(), maps = n()) %>%
  # summarise(rating = mean(rating), n = n(), games = length(unique(series.id))) %>%
  filter(maps > 100) %>%
  # filter(match.id == 'c1bdecf3-0084-5359-a314-a93b00ce5f66') %>%
  arrange(desc(wins.above)) %>%
  data.frame() %>% head(20)


rating %>%
  mutate(rank.rating = rank(-rating,ties.method = 'min')) %>%
  arrange((rank.rating)) %>%
  # filter(player == 'Felony') %>%
  data.frame() %>% tail(5)

# MVP rating
r = rating %>%
  filter(mode != "Search & Destroy") %>%
  # filter(date >= as.Date('2018-08-17')) %>%
  group_by(player) %>%
  summarise(rating = round(mean(rating, na.rm = T),3), maps = n(),
            games = length(unique(series.id)),
            kills = sum(kills),
            deaths = sum(deaths),
            utk = sum(kills..stayed.alive.),
            fb = sum(snd.firstbloods),
            kd = round(sum(kills)/sum(deaths),3),
            gun = Mode(fave.weapon)[[1]],
            sub.use = round(sum(fave.weapon == 'PPSh-41')/n(),2)) %>%
  ungroup() %>%
  # filter(games > 30) %>%
  # group_by(mode) %>%
  # mutate(rank.utk = rank(-utk,ties.method = 'min'),
  #        rank.rating = rank(-rating,ties.method = 'min'),
  #        eng.rating = eng.per10* rating) %>%
  # group_by(gun) %>%
  # mutate(pos.rating = rating - mean(rating)) %>%
  # filter(gun == "PPSh-41") %>%
  # filter(team == "Heretics") %>%
  arrange(desc(rating)) %>%
  data.frame() # %>% head(20)
