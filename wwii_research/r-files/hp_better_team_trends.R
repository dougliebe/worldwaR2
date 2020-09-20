library(odbc)
library(DBI)
library(tidyverse)
con <- DBI::dbConnect(odbc::odbc(), Driver = "MySQL ODBC 8.0 Unicode Driver", 
                      Server = "database-1.cyhyxhbkkfox.us-east-2.rds.amazonaws.com", 
                      UID = "admin", PWD = "guerrillas", Port = 3306, Database = "callofduty")


## Get game info for hardpoints only
games <- tbl(con, "match_info") %>%
  filter(mode == "Hardpoint") %>%
  select(match_id, start_time_s, map) %>%
  collect()
## get scores for only the games listed in gameinfo
ids <- games$match_id %>% unique() %>% unlist()
scores <- tbl(con, "match_scores") %>%
  filter(match_id %in% !!ids) %>%
  select(match_id, team = name, score, is_victor, opp_score, side) %>%
  collect()
## get round scores for only the games in gameinfo
hill_scores <- tbl(con, "round_scores") %>%
  filter(match_id %in% !!ids) %>%
  select(-document.id) %>%
  collect()

## Join game-level data to hill-by-hill data
full_scores <- hill_scores %>%
  rename(hill_score = score,
         opp_hill_score = opp_score) %>%
  left_join(scores, by = c("match_id", 'team')) %>%
  left_join(games, by = "match_id") %>%
  mutate(date = lubridate::as_datetime(start_time_s)) %>%
  filter(date > "2017-10-01", date < "2018-10-01")

## Make a df for # of hills on each map
hill_nos <- data.frame(
  map = unique(full_scores$map),
  hill_nos = c(4,5,4,5,4,NA)
)

## add in hill nos and change hill_no
## to be 1-4/5 instead of incremental

with_hill_no <- full_scores %>%
  left_join(hill_nos, by = 'map') %>%
  mutate(hill_no = hill-((hill-1)%/%hill_nos)*hill_nos) %>%
  select(-hill_nos) 

with_hill_no %>%
  arrange(date, hill) %>%
  group_by(team, map, hill_no) %>%
  mutate(roll_hill_pts = zoo::rollmedianr(hill_score, k = 9, na.pad = T)) %>%
  ggplot(aes(date, roll_hill_pts, color = as.factor(hill_no)))+
  facet_wrap(~map)+theme(legend.position = 'none')+
  geom_smooth(aes(date, roll_hill_pts))

with_hill_no %>%
  arrange(date, hill) %>%
  group_by(team, map, hill_no) %>%
  mutate(roll_hill_pts = zoo::rollmedianr(hill_score, k = 9, na.pad = T))

with_hill_no %>%
  arrange(date, hill) %>%
  filter(is_victor==T) %>%
  group_by(match_id) %>%
  mutate(prev_hill=lag(hill_score)) %>%
  group_by(team, map, hill_no) %>%
  summarise(avg_points = mean(hill_score),
            avg_pm = mean(hill_score-opp_hill_score),
            avg_two = mean(hill_score+prev_hill, na.rm = T)
            ) %>%
  left_join(
    with_hill_no %>%
      arrange(date, hill) %>%
      group_by(match_id, team, map) %>%
      summarise(win = mean(ifelse(is_victor == T, 1,0))) %>%
      group_by(team, map) %>%
      summarise(wl = mean(win),
                n = n ()),
    by = c('team', 'map')
  ) %>%
  filter(n >= 10) %>%
  ggplot(aes(wl, avg_two ))+geom_point() + facet_wrap(~map+hill_no)+
  geom_smooth(method = "lm")+geom_hline(yintercept = 0)
















