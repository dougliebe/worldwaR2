library(odbc)
library(DBI)
library(tidyverse)
library(png)
library(RColorBrewer)
con <- DBI::dbConnect(odbc::odbc(), Driver = "MySQL ODBC 8.0 Unicode Driver", 
                      Server = "database-1.cyhyxhbkkfox.us-east-2.rds.amazonaws.com", 
                      UID = "admin", PWD = "guerrillas", Port = 3306, Database = "callofduty")

getEvents <- tbl(con, "events") %>%
  filter(type %in% c("spawn", "death")) %>%
  select(match_id, type, time_ms, id, pos_x, pos_y, attacker_pos_x, attacker_pos_y) %>%
  collect()
  


match_info <- tbl(con, "game_info") %>%
  collect()
series_info <- tbl(con, 'match_info') %>%
  collect()

wwii_hps <- getEvents %>%
  left_join(match_info %>% select(match_id, series_id, mode, map), by = 'match_id') %>%
  left_join(series_info %>% select(series_id, title), by = 'series_id') %>%
  filter(mode == "Hardpoint", title == "ww2")
ids <- wwii_hps$match_id %>% unique() %>% unlist()
boxscores <- tbl(con, "boxscores") %>%
  filter(match_id %in% !!ids) %>%
  collect()

kill_death_spawn <- wwii_hps %>%
  left_join(boxscores %>% select(id = name, team, match_id), by = c("match_id", "id")) %>%
  group_by(match_id, id, type) %>%
  mutate(event_no = 1:n(),
         event_no = ifelse(type == "spawn", event_no, event_no-1)) 


by_event_no <- kill_death_spawn %>%
  filter(match_id == "0020d7ee-4370-5c5a-9dbb-0bac41b16a73") %>%
  # filter(id == "JOSHH") %>%
  # select(type, id, team, pos_x, pos_y, attacker_pos_x, attacker_pos_y, map, event_no) %>%
  group_by(team, map, id, event_no) %>%
  summarise(kill_x = sum(parse_number(attacker_pos_x), na.rm = T),
            kill_y = sum(parse_number(attacker_pos_y), na.rm = T),
            death_x = sum(ifelse(type == "death", pos_x, 0)),
            death_y = sum(ifelse(type == "death", pos_y, 0)),
            spawn_x = sum(ifelse(type == "spawn", pos_x, 0)),
            spawn_y = sum(ifelse(type == "spawn", pos_y, 0)),
            time_ms = min(time_ms)
  ) %>%
  ungroup() %>%
  pivot_longer(cols = c(5:10),
               names_to = c("type", ".value"),
               names_pattern = c("(.*)_(.)")) %>%
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
    TRUE ~ 0
  ),
  hill_no = case_when(
    time_ms < (60000)*1+5000 ~ 1,
    time_ms < (60000)*2+5000 ~ 2,
    time_ms < (60000)*3+5000 ~ 3,
    time_ms < (60000)*4+5000 ~ 4,
    time_ms < (60000)*5+5000 ~ 1,
    time_ms < (60000)*6+5000 ~ 2,
    time_ms < (60000)*7+5000 ~ 3,
    time_ms < (60000)*8+5000 ~ 4,
    time_ms < (60000)*9+5000 ~ 1,
    time_ms < (60000)*10+5000 ~ 2,
    time_ms < (60000)*11+5000 ~ 3,
    TRUE ~ 0
  ))

hill_scores <- tbl(con, 'round_scores') %>%
  filter(match_id == "0020d7ee-4370-5c5a-9dbb-0bac41b16a73") %>%
  collect()



map_img <- readPNG(here::here('maps', 'ww2', paste0("sainte_marie_du_mont",".png")))
h <- dim(map_img)[1]
w <- dim(map_img)[2]
ggplot(data=by_event_no %>%
         filter(type != "kill", event_no != 0) %>%
         left_join(hill_scores, by = c('hill','team'))) +
  annotation_custom(grid::rasterGrob(map_img, width=unit(1,"npc"), height=unit(1,"npc")), 0, w, 0,-h) +
  theme_bw() + theme(legend.position = 'none',axis.title = element_blank(), axis.text = element_blank()) +
  coord_equal() + # To keep the aspect ratio of the image.
  scale_x_continuous(expand=c(0,0),limits=c(5,w-10)) +
  scale_y_reverse(expand=c(0,0),limits=c(h,0))+
  # geom_polygon(aes(x,y, alpha = 0.1, fill = 'none'))+
  geom_path(aes(x,y,alpha = 0.7, group = event_no, col = team))+
  # geom_point(aes(x,y, color = score))+
  facet_wrap(~as.factor(hill))





