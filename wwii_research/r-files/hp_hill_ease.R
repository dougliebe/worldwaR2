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
  filter(map != "?") %>%
  group_by(map, is_victor, hill_no) %>%
  summarise(pts = mean(hill_score-opp_hill_score),
            n = n()) %>%
  ggplot(aes(hill_no, pts, fill = is_victor))+geom_col(position = "dodge") + facet_wrap(~map)

with_hill_no %>%
  filter(map != "?") %>%
  mutate(full_hold = ifelse(hill_score > 45,1,0)) %>%
  group_by(map, is_victor, hill_no) %>%
  summarise(pts = mean(hill_score-opp_hill_score),
            fh = mean(full_hold),
            n = n()) %>%
  ggplot(aes(hill_no, fh, fill = is_victor))+geom_col(position = "dodge") + facet_wrap(~map)


### Lets look at one game
# break up by difficulty of points
# plot that in treeplot
match_id = "95a7317b-bfe5-5eed-b555-03b48de345d3" # This is a london docks HP
# LG 250 - 170 Mindfreak
scores %>%
  filter(match_id == !!match_id)

# Get the ease of holding relative to the easiest hill
ease <- with_hill_no %>%
  filter(map != "?") %>%
  mutate(full_hold = ifelse(hill_score > 50,1,0)) %>%
  group_by(map, hill_no) %>%
  summarise(pts = mean(hill_score),
            fh = mean(full_hold)*100,
            n = n()) %>%
  group_by(map) %>%
  mutate(ease = ifelse(fh <= 5 & pts > 19 , 1, 0))

with_hill_no %>%
  filter(map != "?") %>%
  group_by(match_id) %>%
  mutate(full_hold = ifelse(hill_score > 50,1,0),
         prev_hill = lag(hill_score)) %>%
  group_by(map, hill_no) %>%
  summarise(pts = mean(hill_score),
            two_pts = mean(hill_score + prev_hill, na.rm = T),
            fh = mean(full_hold)*100,
            n = n()) %>%
  ungroup() %>%
  mutate(is_good = c(0,0,0,0,0,1,-1,1,0,0,-1,1,1,0,0,0,0,1,0,0,-1,1),
         is_good = case_when(
           is_good == 1 ~ "good",
           is_good == -1 ~ 'bad',
           TRUE ~ "same"
         )) %>%
  ggplot() + geom_point(aes(pts, fh, color = is_good)) + 
  facet_wrap(~map)


# The important requirement is, your data must have one
# variable each that describes the:
# 1. area of the tiles,
# 2. fill color, 
# 3. tile’s label
# 4. parent group.
library(ggplot2) 
library(treemapify)
for_graph <- with_hill_no %>%
  filter(match_id == !!match_id) %>%
  left_join(ease %>% select(hill_no, ease), by = c('hill_no')) %>%
  select(team, hill, hill_score, ease, hill_no)

ggplot(data = for_graph %>%
         arrange(hill), aes(area = ease, fill = team,
                            subgroup = team, subgroup2 = ease,
                            label = hill_score))+
  geom_treemap()+
  geom_treemap_text(place = "centre", alpha = .9, colour =
                      "White", fontface = "italic", min.size = 0)+
  geom_treemap_subgroup_border()+geom_treemap_subgroup2_border()

with_hill_no %>%
  left_join(ease %>% select(map, hill_no, ease), by = c('map','hill_no')) %>%
  mutate(easy_points = ease*hill_score) %>%
  group_by(team, map) %>%
  summarise(pts = mean(easy_points),
            n = length(unique(match_id))) %>%
  filter(n >= 10) %>%
  arrange(desc(pts))

library(zoo)
with_hill_no %>%
  filter(map != "?") %>%
  left_join(ease %>% select(map, hill_no, ease), by = c('map','hill_no')) %>%
  mutate(easy_points = ease*hill_score,
         win = ifelse(is_victor,1,0)) %>%
  group_by(date, match_id, team, map, win) %>%
  summarise(exp_points = sum(easy_points),
            true_points = sum(hill_score)) %>%
  group_by(team, map) %>%
  # mutate(n = length(unique(match_id))) %>%
  # filter(n > 10) %>%
  mutate(ep = lag(rollmeanr(exp_points, k = 10,na.pad=TRUE)),
            ap = lag(rollmeanr(true_points, k = 10,na.pad=TRUE)),
            n = length(unique(match_id)),
         wp = lag(rollmeanr(win, k = 10, na.pad = T))
         ) %>%
  filter(n >= 5) %>%
  ungroup() %>%
  group_by(map) %>%
  summarise(c = cor(wp,win, use = 'pairwise.complete.obs'))

## how long ago should results be considered?
result <- data.frame() 
for(i in seq(30, 120, 30)) {
  with_hill_no %>%
    filter(map != "?", date > ) %>%
    left_join(ease %>% select(map, hill_no, ease), by = c('map','hill_no')) %>%
    mutate(easy_points = ease*hill_score,
           win = ifelse(is_victor,1,0)) %>%
    group_by(date, match_id, team, map, win) %>%
    summarise(exp_points = sum(easy_points),
              true_points = sum(hill_score)) %>%
    group_by(team, map) %>%
    # mutate(n = length(unique(match_id))) %>%
    # filter(n > 10) %>%
    mutate(ep = lag(rollmeanr(exp_points, k = 10,na.pad=TRUE)),
           ap = lag(rollmeanr(true_points, k = 10,na.pad=TRUE)),
           n = length(unique(match_id)),
           wp = lag(rollmeanr(win, k = 10, na.pad = T))
    ) %>%
    filter(n >= 5) %>%
    ungroup() %>%
    group_by(map) %>%
    summarise(c = cor(ap,win, use = 'pairwise.complete.obs'))
}