library(odbc)
library(DBI)
library(tidyverse)
library(png)
library(RColorBrewer)
con <- DBI::dbConnect(odbc::odbc(), Driver = "MySQL ODBC 8.0 Unicode Driver", 
                      Server = "database-1.cyhyxhbkkfox.us-east-2.rds.amazonaws.com", 
                      UID = "admin", PWD = rstudioapi::askForPassword(), Port = 3306, Database = "callofduty")

getEvents <- tbl(con, "events") %>%
  filter(type %in% c("spawn", "death")) %>%
  select(match_id,type, time_ms, id, pos_x, pos_y, attacker_id, attacker_pos_x, attacker_pos_y, life, attacker_life) %>%
  collect()



match_info <- games <- tbl(con, "match_info") %>%
  filter(mode == "Hardpoint") %>%
  select(match_id,series_id, start_time_s, map,mode, event) %>%
  collect()
series_info <- tbl(con, 'series_info') %>%
  distinct() %>%
  collect() 
ids <- match_info$match_id %>% unique() %>% unlist()
scores <- tbl(con, "match_scores") %>%
  filter(match_id %in% !!ids) %>%
  select(match_id, team = name, score, is_victor, opp_score, side) %>%
  collect()
## get round scores for only the games in gameinfo
hill_scores <- tbl(con, "round_scores") %>%
  filter(match_id %in% !!ids) %>%
  select(-document.id) %>%
  collect()
full_scores <- hill_scores %>%
  rename(hill_score = score,
         opp_hill_score = opp_score) %>%
  left_join(scores, by = c("match_id", 'team')) %>%
  left_join(match_info, by = "match_id") %>%
  mutate(date = lubridate::as_datetime(start_time_s)) %>%
  filter(date > "2017-10-01", date < "2018-10-01")

## Make a df for # of hills on each map
hill_nos <- data.frame(
  map = unique(full_scores$map),
  hill_nos = c(4,5,4,5,4, NA)
)

## add in hill nos and change hill_no
## to be 1-4/5 instead of incremental

with_hill_no <- full_scores %>%
  left_join(hill_nos, by = 'map') %>%
  mutate(hill_no = hill-((hill-1)%/%hill_nos)*hill_nos) %>%
  select(-hill_nos) 


wwii_hps <- getEvents %>%
  left_join(match_info %>% select(match_id, series_id, mode, map, event), by = 'match_id') %>%
  left_join(series_info %>% select(series_id, title, event), by = c('series_id', 'event')) %>%
  filter(mode == "Hardpoint", title == "ww2")

boxscores <- tbl(con, "boxscores") %>%
  filter(match_id %in% !!ids) %>%
  collect()

kill_death_spawn <- wwii_hps %>%
  left_join(boxscores %>% select(id = name, team, match_id), by = c("match_id", "id")) %>%
  group_by(match_id, id, type) %>%
  mutate(event_no = 1:n(),
         event_no = ifelse(type == "spawn", event_no, event_no-1)) 

## spawn and first kill/death
example <- wwii_hps %>%
  left_join(boxscores %>% select(id = name, team, match_id), by = c("match_id", "id")) %>%
  filter(type == 'spawn') %>%
  select(match_id, id, life, pos_x, pos_y, time_ms, series_id, map, mode) %>%
  left_join(wwii_hps %>%
              filter(type == 'death') %>%
              select(match_id,id,
                     death_pos_x = pos_x,
                     death_pos_y = pos_y,
                     life,
                     death_time_ms = time_ms),
            by = c('match_id','life', 'id')) %>%
  left_join(wwii_hps %>%
              filter(type == 'death') %>%
              select(match_id,id = attacker_id,
                     kill_pos_x = attacker_pos_x,
                     kill_pos_y = attacker_pos_y,
                     life = attacker_life,
                     kill_time_ms = time_ms) %>%
              mutate(life = as.numeric(life)),
            by = c('match_id','life', 'id')) %>%
  filter(match_id == "bcd8cb72-3bd6-5aa7-b209-44fbda5a64ad")

library(matlib)
hills_w_angles <- example %>%
  # filter(life == 1) %>%
  # data.frame()
  mutate(across(c(pos_x, pos_y,
                 kill_pos_x, kill_pos_y,
                 death_pos_x, death_pos_y,
                 time_ms, death_time_ms, kill_time_ms),
            as.numeric)) %>%
  group_by(match_id, life,id) %>%
  slice(1) %>%
  mutate(first_eng_x = case_when(
                          is.na(kill_time_ms) ~ death_pos_x,
                          kill_time_ms < death_time_ms ~ kill_pos_x,
                          TRUE ~ death_pos_x
                        ),
          first_eng_y = case_when(
                          is.na(kill_time_ms) ~ death_pos_y,
                          kill_time_ms < death_time_ms ~ kill_pos_y,
                          TRUE ~ death_pos_y
                        )
        ) %>%
  mutate(angle = atan2(first_eng_x - pos_x, first_eng_y - pos_y),
         angle = case_when(
           angle < 0 ~ 360 + (angle * 180/pi),
                   T ~ angle * 180/pi
                 )
         ) %>%
  select(match_id, id, time_ms, spawn_x = pos_x, spawn_y = pos_y, 
         map, mode, first_eng_x, first_eng_y, angle) %>%
  mutate(time_ms = as.numeric(time_ms), 
         hill = case_when(
           time_ms < (60000)*1+5000 ~ 1,
           time_ms < (60000)*2+5000 ~ 2,
           time_ms < (60000)*3+5000 ~ 3,
           time_ms < (60000)*4+5000 ~ 4,
           time_ms < (60000)*5+5000 ~ 5,
           time_ms < (60000)*6+5000 ~ 6,
           time_ms < (60000)*7+5000 ~ 7,
           time_ms < (60000)*8+5000 ~ 8,
           time_ms < (60000)*9+5000 ~ 9,
           time_ms < (60000)*10+5000 ~ 10,
           time_ms < (60000)*11+5000 ~ 11,
           time_ms < (60000)*12+5000 ~ 12,
           TRUE ~ 0
         )
  ) %>%
  left_join(boxscores %>% select(id = name, team, match_id), by = c("match_id", "id")) %>%
  left_join(hill_nos, by = 'map') %>%
  mutate(hill_no = hill-((hill-1)%/%hill_nos)*hill_nos)
  # head(10) %>%
  # data.frame()
  
## spawn clusters for each map and hill
  ## use for training 
train <- wwii_hps %>%
  mutate(time_ms = as.numeric(time_ms), 
          hill = case_when(
                time_ms < (60000)*1+5000 ~ 1,
                time_ms < (60000)*2+5000 ~ 2,
                time_ms < (60000)*3+5000 ~ 3,
                time_ms < (60000)*4+5000 ~ 4,
                time_ms < (60000)*5+5000 ~ 5,
                time_ms < (60000)*6+5000 ~ 6,
                time_ms < (60000)*7+5000 ~ 7,
                time_ms < (60000)*8+5000 ~ 8,
                time_ms < (60000)*9+5000 ~ 9,
                time_ms < (60000)*10+5000 ~ 10,
                time_ms < (60000)*11+5000 ~ 11,
                time_ms < (60000)*12+5000 ~ 12,
                TRUE ~ 0
              )
  ) %>%
  left_join(boxscores %>% select(id = name, team, match_id), by = c("match_id", "id")) %>%
  filter(type == 'spawn', time_ms != 0) %>%
  left_join(hill_nos, by = 'map') %>%
  mutate(hill_no = hill-((hill-1)%/%hill_nos)*hill_nos) %>%
  select(map, hill_no, pos_x, pos_y) %>%
  mutate(across(hill_no, as.factor))

## Get 4 most popular spawns for each hill
spawns <- train %>%
  mutate(hill_no = as.numeric(hill_no)) %>%
  # filter(map == "London Docks") %>%
  group_by(map, hill_no, pos_x, pos_y) %>%
  count() %>%
  group_by(map, hill_no) %>%
  arrange(desc(n)) %>%
  slice(1:4)

### Filter out spawns that are the most common from 
### df with angles

hills_w_angles %>%
  left_join(spawns %>%
              mutate(in_it = 1) %>%
              select(-n), by = c('map', 'hill_no',
                                              'spawn_x' = 'pos_x',
                                              'spawn_y' = 'pos_y')) %>%
  mutate(spawn = paste0(spawn_x, spawn_y)) %>%
  filter(in_it == 1) %>%
  ggplot(aes(x = angle)) +
  ## Make the boundary 1/2 of the binwidth so that
  ## each bin is centered around the interval number
  geom_histogram(binwidth = 30, boundary = -15, color = "black") +
  coord_polar() +
  scale_x_continuous(limits = c(0,360))+
  facet_wrap(~ hill_no+spawn)

base_p <- ggplot() +
  annotation_custom(grid::rasterGrob(map_img, width=unit(1,"npc"), height=unit(1,"npc")), 0, w, 0,-h) +
  theme_bw() + theme(legend.position='none',axis.title = element_blank(), axis.text = element_blank()) +
  coord_equal() + # To keep the aspect ratio of the image.
  scale_x_continuous(expand=c(0,0),limits=c(5,w-10)) +
  scale_y_reverse(expand=c(0,0),limits=c(h,0))
library(cowplot)
ggdraw(base_p)+
  draw_plot(ggplot(data = data.frame(x = 173, y = 344), aes(x,y))+
              geom_point(color = 'red')+
              # set transparency
              theme(
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.background = element_rect(fill = "transparent",colour = NA),
                plot.background = element_rect(fill = "transparent",colour = NA),
                axis.text = element_blank(),
                axis.title = element_blank(),
                axis.ticks = element_blank()
              ),
            hjust = 0.25)
  

  
# plot a map
library(png)
map_img <- readPNG(here::here('maps', 'ww2', paste0("ardennes_forest",".png")))
h <- dim(map_img)[1]
w <- dim(map_img)[2]
p <- ggplot(data = train %>%
              filter(map == "London Docks")) +
  annotation_custom(grid::rasterGrob(map_img, width=unit(1,"npc"), height=unit(1,"npc")), 0, w, 0,-h) +
  theme_bw() + theme(legend.position='none',axis.title = element_blank(), axis.text = element_blank()) +
  coord_equal() + # To keep the aspect ratio of the image.
  scale_x_continuous(expand=c(0,0),limits=c(5,w-10)) +
  scale_y_reverse(expand=c(0,0),limits=c(h,0))+
  geom_point(aes(pos_x, pos_y), alpha = 0.01, color = "blue")+
  geom_point(aes(x = 616, y = 448), color = 'red')+
  facet_wrap(~map + hill_no)

train %>%
  filter(map == "Ardennes Forest",
         pos_x == 617,
         pos_y == 549)

### Spawns are very consistent across hills
smdm <- train %>%
  filter(map == "Sainte Marie du Mont") %>%
  select(pos_x, pos_y)

library(tidymodels)
kclusts <- 
  tibble(k = 1:9) %>%
  mutate(
    kclust = map(k, ~kmeans(smdm, .x)),
    tidied = map(kclust, tidy),
    glanced = map(kclust, glance),
    augmented = map(kclust, augment, smdm)
  ) %>%
  filter(k == 4)
clusters <- 
  kclusts %>%
  unnest(cols = c(tidied))

assignments <- 
  kclusts %>% 
  unnest(cols = c(augmented))

clusterings <- 
  kclusts %>%
  unnest(cols = c(glanced))

ggplot(assignments %>% filter(k == 4), aes(x = pos_x, y = pos_y)) +
  annotation_custom(grid::rasterGrob(map_img, width=unit(1,"npc"), height=unit(1,"npc")), 0, w, 0,-h) +
  theme_bw() + theme(legend.position='none',axis.title = element_blank(), axis.text = element_blank()) +
  coord_equal() + # To keep the aspect ratio of the image.
  scale_x_continuous(expand=c(0,0),limits=c(5,w-10)) +
  scale_y_reverse(expand=c(0,0),limits=c(h,0))+
  geom_point(aes(color = .cluster), alpha = 0.8) + 
  facet_wrap(~ k)
ggplot(clusterings, aes(k, tot.withinss)) +
  geom_line() +
  geom_point()
