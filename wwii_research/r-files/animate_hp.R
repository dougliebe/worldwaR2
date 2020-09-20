library(tidyverse)
library(gifski)
library(gganimate)
library(tweenr)
library(odbc)
library(DBI)


# we need data from one HP game
# we need: kills, deaths, spawns
# map, time, team, player name
con <- DBI::dbConnect(odbc::odbc(), Driver = "MySQL ODBC 8.0 Unicode Driver", 
                      Server = "database-1.cyhyxhbkkfox.us-east-2.rds.amazonaws.com", 
                      UID = "admin", PWD = rstudioapi::askForPassword(),
                      Port = 3306, Database = "callofduty")

# get game info for 1 HP
id <- tbl(con, "match_info") %>%
  filter(mode == "Hardpoint", map == "Sainte Marie du Mont") %>%
  head(1) %>%
  collect()

# get events from that game
match_events <- tbl(con, 'events') %>%
  filter(match_id == !!id$match_id) %>%
  collect()

boxscores <- tbl(con, "boxscores") %>%
  filter(match_id == !!id$match_id) %>%
  collect()

# need to data to look like:
# time | x | y | player | team 
event_list <- match_events %>%
  select(time_ms, pos_x, pos_y, id, type) %>%
  bind_rows(
    match_events %>%
      filter(type == "death") %>%
      mutate(pos_x = as.numeric(attacker_pos_x), pos_y = as.numeric(attacker_pos_y)) %>%
      select(time_ms, pos_x, pos_y,id = attacker_id) %>%
      mutate(type = "kill")
  ) %>%
  left_join(boxscores %>% select(name, team), by = c('id'='name')) %>%
  rename(x = pos_x, y = pos_y) %>%
  mutate(team = str_replace_all(team, " ", ""),
         time_s =  round(as.numeric(time_ms),-2)/1000) %>%
  filter(as.numeric(time_ms) < 65*1000) %>%
  # bind_rows(data.frame(time_s = 1:max(as.numeric(match_events$time_ms)/1000))) %>%
  select(-type, -time_ms) %>%
  unite(group, team, id) %>%
  complete(time_s = seq(0,max(time_s),0.1), group = unique(group)) %>%
  arrange(time_s) %>%
  group_by(group) %>%
  group_modify(~tween_fill(., ease = 'linear')) %>%
  group_by(group) %>%
  # ## Focus on each life for each player
  # tween_fill( ease = 'linear') %>%
  # group_by(group) %>%
  # arrange(time_s) %>%
  mutate(frame.id = 1:n())


# plot
library(png)
map_img <- readPNG(here::here('maps', 'ww2', paste0("sainte_marie_du_mont",".png")))
h <- dim(map_img)[1]
w <- dim(map_img)[2]
p <- ggplot(data=event_list %>%
              separate(group, into = c('team', 'player'))) +
  annotation_custom(grid::rasterGrob(map_img, width=unit(1,"npc"), height=unit(1,"npc")), 0, w, 0,-h) +
  theme_bw() + theme(legend.position='none',axis.title = element_blank(), axis.text = element_blank()) +
  coord_equal() + # To keep the aspect ratio of the image.
  scale_x_continuous(expand=c(0,0),limits=c(5,w-10)) +
  scale_y_reverse(expand=c(0,0),limits=c(h,0))+
  geom_point(aes(x=x, y = y, group = team, color = team))+
  geom_label(aes(x = 50,
                 y = 50,
                 label = frame.id),
             size = 7)+
  transition_time(frame.id)+
  ease_aes('cubic-in-out')

animate(p, fps = 20, nframes = length(unique(event_list$time_s)))



########## Animate Killfeed and kill-tide ##################

head(match_events)
match_events %>%
  filter(type == "death") %>%
  mutate(record = paste(str_to_sentence(attacker_id), "kills", str_to_sentence(id), "with", attacker_weapon),
         time_s = as.numeric(time_ms)/1000) %>%
  mutate(breaks = cut(time_s, breaks = seq(0,max(time_s),10))) %>%
  group_by(breaks) %>%
  summarise(records = paste(record, collapse = "\n")) %>%
  # data.frame() %>% head
  ggplot(aes(x = 1, y = 1, label = records)) + 
  geom_label()+
  transition_states(breaks)

team_1_color = 'blue'
team_2_color = 'gold4'

game <- match_events %>%
  filter(type == "death") %>%
  left_join(boxscores %>% select(name, team), by = c('id'='name')) %>%
  mutate(color = case_when(
    team == "TEAM ENVYUS" ~ team_1_color,
    team == "SPLYCE" ~ team_2_color
  ),
  color_attacker = case_when(
    team == "TEAM ENVYUS" ~ team_2_color,
    team == "SPLYCE" ~ team_1_color
  ),
  attacker_id = paste0("<span style='color:", color_attacker,"'>**", str_to_sentence(attacker_id), "**</span>"),
  id = paste0("<span style='color:", color,"'>**", str_to_sentence(id), "**</span>")) %>%
  mutate(time_s = round(as.numeric(time_ms)/1000),
         record = paste("@",time_s, " " ,attacker_id, "kills", id))

killfeed <- tibble()
for(i in 5:max(game$time_s)) {
  killfeed <- bind_rows(killfeed, data.frame(time_s = i, game %>%
                                               filter(time_s < i, time_s > i-20) %>%
                                               summarise(records = paste(record, collapse = "<br>")))
                        
  )
  
}

plusminus <- game %>%
  mutate(pm = ifelse(color == team_1_color,1,-1)) %>%
  complete(time_s = 1:max(.$time_s), fill = list(pm = 0)) %>%
  mutate(cum_pm = cumsum(pm)) %>%
  group_by(time_s) %>%
  summarise(pm = max(cum_pm)) %>%
  ungroup()


library(ggtext)
killfeed %>%
  # filter()
  ggplot()+
  geom_richtext(aes(x = 1, y = 1, label = records, vjust = 0, hjust = 0)) +
  geom_label(aes(x = -20, y = 1, label = time_s))+
  transition_states(time_s)

p <- plusminus %>%
  left_join(killfeed, by = 'time_s') %>%
  tween_
  ggplot()+
  geom_line(aes(x = time_s, y = pm, group = 1), color = 'black', size = 1, alpha = 0.3)+
  geom_point(aes(x = time_s, y = pm, group = 1), color = 'red', size = 2)+
  geom_richtext(aes(x = 200, y = 0, label = records, vjust = 0, hjust = 0))+
  transition_reveal(time_s)+
  ggtitle('Now showing',subtitle = 'Frame {frame} of {nframes}')


animate(p, fps = 60, nframes = length(unique(p$data$time_s)))





