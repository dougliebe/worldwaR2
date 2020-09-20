library(dplyr)
library(ggplot2)
library(jsonlite)
library(ggplot2)
library(scales)
library(png)
library(dplyr)

setwd('C:/Users/Doug/Documents/CoD/')
img <- readPNG('cwl-data/maps/ww2/valkyrie.png')
output <- data.frame()

# Go through all games all events
location <- list.files(path = 'cwl-data/data/structured', pattern = "structured")
for(j in 1:length(location)) {
  filenames <- list.files( path = paste('cwl-data/data/structured/',location[j], sep = ""),pattern="*.json", full.names=TRUE)
  print(location[j])
  
  
  for (i in 1:length(filenames)) {
    data_json <- fromJSON(filenames[i], simplifyVector = T)
    if(data_json$mode == "Hardpoint" & !is.null(nrow(data_json$events))){
      # 
      # 
      # events <- (data_json$events)
      # data <- subset(events, events$type == 'death')
      pt_chart <-data.frame(team = rep(data_json$teams$name,each = length(data_json$teams$round_scores[[1]])),
                            hp = rep(rep(seq(1,length(data_json$hp_hill_names)),4)[1:length(data_json$teams$round_scores[[1]])],2),
                            set = rep(rep(seq(1,4),each = length(data_json$hp_hill_names))[1:length(data_json$teams$round_scores[[1]])],2),
                            score = c(data_json$teams$round_scores[[1]]-data_json$teams$round_scores[[2]],-data_json$teams$round_scores[[1]]+data_json$teams$round_scores[[2]]), 
                            win = rep(data_json$teams$is_victor, each = length(data_json$teams$round_scores[[1]])),
                            opp = rev(rep(data_json$teams$name,each = length(data_json$teams$round_scores[[1]]))))
      # # # team_rounds <- data.frame(player.team = rep(data_json$teams$name,each = data_json$rounds),
      # #                           round = rep(seq(1,data_json$rounds),2),
      # #                           offdef = c(rep(c("off",'def'),length.out = data_json$rounds),rep(c("def",'off'),length.out = data_json$rounds)),
      # #                           win = c(data_json$teams$round_scores[[1]],data_json$teams$round_scores[[2]]))
      # team_players <- data.frame(name = data_json$players$name, player.team = data_json$players$team, gun = data_json$players$fave_weapon)
      # data = do.call(cbind.data.frame, data)
      # data = do.call(cbind.data.frame, data)
      # data = do.call(cbind.data.frame, data)
      # # data <- data.test %>%
      # #   dplyr::group_by(round) %>%
      # #   dplyr::mutate(first_kill = ifelse(round_time_ms == min(round_time_ms),1,0)) %>%
      # #   data.frame() %>%
      # #   filter(first_kill == 1)
      # data <- merge(data, team_players, by.x = 'data.attacker.id', by.y = 'name')
      # # data <- merge(data, team_rounds, by = c('player.team', 'round'))
      # output <- rbind(output, data.frame(map = data_json$map, offdef = data$offdef, x = data$data.attacker.pos.x, y = data$data.attacker.pos.y))
      output <- rbind(output, data.frame(pt_chart, map = data_json$map, id = data_json$id, code = data_json$series_id, event = location[j]))
    }}}

output %>%
  # filter(map == 'Sainte Marie du Mont') %>%
  dplyr::filter(win == TRUE) %>%
  dplyr::group_by(event, team, opp,id, hp, win, map) %>%
  dplyr::summarise(score = sum(score)) %>%
  filter(id == 'c1119828-62d8-512a-a4fd-9ef16df56a7f') %>% arrange((score))


output %>%
  filter(map == 'Sainte Marie du Mont') %>%
  dplyr::group_by(team, opp,id, hp, win) %>%
  dplyr::summarise(score = sum(score)) %>%
  dplyr::ungroup() %>%
  # dplyr::group_by(map) %>%
  dplyr::summarise(n = n(), one = exp(coefficients(glm(as.factor(win==T)~as.factor(hp):score, family = 'binomial'))[2]),
                   two = exp(coefficients(glm(as.factor(win==T)~as.factor(hp):score, family = 'binomial'))[3]),
                   three = exp(coefficients(glm(as.factor(win==T)~as.factor(hp):score, family = 'binomial'))[4]),
                   four = exp(coefficients(glm(as.factor(win==T)~as.factor(hp):score, family = 'binomial'))[5]))

map1 = 'Sainte Marie du Mont'
output %>%
  filter(map == map1) %>%
  dplyr::group_by(event, team, opp,id, hp, win) %>%
  dplyr::summarise(score = sum(score)) %>%
  dplyr::mutate(outscore = ifelse(score > 0, 1, 0)) %>%
  dplyr::ungroup() %>%
  # dplyr::group_by(team) %>%
  # dplyr::mutate(winpct = round(mean(win == T),2)) %>%
  # dplyr::ungroup() %>%
  dplyr::group_by(event,outscore, hp) %>%
  dplyr::summarise(n = n(), win_pct = mean(win)) %>%
  filter(outscore == 1, hp == 3) %>% data.frame()
d = output %>%
  filter(map == map1) %>%
  dplyr::group_by(team, opp,id, hp, win) %>%
  dplyr::summarise(score = sum(score)) %>%
  dplyr::mutate(outscore = ifelse(score > 0, 1, 0)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(team) %>%
  dplyr::mutate(winpct = round(mean(win == T),2)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(team, win, hp, winpct) %>%
  dplyr::summarise(n = n(), pdif = mean(score)) %>% 
  filter(hp == 3, win == T, n > 5) %>% data.frame()


