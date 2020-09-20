library(tidyverse)
library(here)
library(tidyjson)

### Setup empty data frames
match_info <- 
  game_info <-
  match_scores <-
  round_scores <- 
  boxscores <-
  events <- 
  data.frame()

## Get all the event data folders
folders <- list.files(path = here('data/structured'), pattern = "structured", full.names = T)
for(i in 1:length(folders)) {
  
  ## get a file from a folder to play with
  filenames <- list.files(folders[i], full.names = T, pattern = ".*.json")
  event <- str_split(folders[i],"-", simplify = T) %>%
    last()
  for(j in 1:length(filenames)) {
    
    file <- filenames[j]
    # jsonlite::fromJSON(file) # for previewing file
    
    data <- read_json(file)
    
    base <- data %>%
      spread_all() %>%
      as_tibble() %>%
      select(-end_time_s) %>%
      rename(match_id = id)
    
    ## match_info
    match_info_temp <- base %>%
      select(1,2,3,5) %>%
      mutate(event = event)
    ## game_info
    game_info_temp <- base %>%
      select(-1,-2,-3) %>%
      mutate(event = event)
    ## match_scores
    # match_scores_temp <- data %>%
    #   spread_values(
    #     match_id = jstring(id)
    #   ) %>%
    #   enter_object(teams) %>%
    #   gather_array() %>%
    #   spread_all() %>%
    #   as_tibble() %>%
    #   mutate(opp_score = sum(score)-score)
    # 
    # ## Round scores
    # round_scores_temp <- data %>%
    #   spread_values(
    #     match_id = jstring(id)
    #   ) %>%
    #   enter_object(teams) %>%
    #   gather_array() %>%
    #   spread_values(
    #     team = jstring(name)
    #   ) %>%
    #   enter_object(round_scores) %>%
    #   gather_array(column.name = 'hill') %>%
    #   append_values_number(column.name = "score") %>%
    #   as_tibble() %>%
    #   group_by(hill) %>%
    #   mutate(opp_score = sum(score)-score) %>%
    #   select(-array.index)
    # 
    # ## boxscores
    # boxscores_temp <- data %>%
    #   spread_values(
    #     match_id = jstring(id)
    #   ) %>%
    #   # gather_object() %>%
    #   enter_object(players) %>%
    #   gather_array() %>%
    #   spread_all() %>%
    #   as_tibble() %>%
    #   janitor::clean_names() %>%
    #   select(-array_index)
    # 
    # ## events
    # events_temp <- data %>%
    #   spread_values(
    #     match_id = jstring(id)
    #   ) %>%
    #   # gather_object() %>%
    #   enter_object(events) %>%
    #   gather_array(column.name = 'event_id') %>%
    #   spread_all(recursive = F) %>%
    #   enter_object(data) %>%
    #   spread_all %>%
    #   as_tibble() %>%
    #   # select(-array.index) %>%
    #   janitor::clean_names() %>%
    #   data.frame() 
    
    match_info <- match_info %>% bind_rows(match_info_temp)
    game_info <- game_info %>% bind_rows(game_info_temp)
    # match_scores <- match_scores %>% bind_rows(match_scores_temp)
    # round_scores <-  round_scores %>% bind_rows(round_scores_temp)
    # boxscores <- boxscores %>% bind_rows(boxscores_temp)
    # events <-  events %>% bind_rows(events_temp)
  }
}
