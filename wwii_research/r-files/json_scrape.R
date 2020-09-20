library(jsonlite)
library(dplyr)

#set wd
setwd('C:/Users/Doug/Documents/CoD/')

#make df to put output in
output <- data.frame()

# Go through all games all events
location <- list.files(path = 'cwl-data/data/structured', pattern = "structured")
for(j in 1:length(location)) {
  
  filenames <- list.files( path = paste('cwl-data/data/structured/',location[j], sep = ""),pattern="*.json", full.names=TRUE)
  print(location[j]) # show when you start each new event
  
  
  for (i in 1:length(filenames)) {
    
    #read each json file in as list of lists
    data_json <- fromJSON(filenames[i], simplifyVector = T)
    # I filter out by mode,
    # but you can change this to whatever
    if(data_json$mode == "Hardpoint" & !is.null(nrow(data_json$events))){
      
      # get just spawns and deaths
      events <- (data_json$events)
      data <- subset(events, events$type == 'death' | events$type == 'spawn')
      
      # make df of players and teams to match 
      team_players <- data.frame(name = data_json$players$name, player.team = data_json$players$team,
                                 gun = data_json$players$fave_weapon)
      team_players <- rbind(team_players, data.frame(name = NA, player.team = NA, gun = NA))
      # this breaks down list of lists into good data frame
      data = do.call(cbind.data.frame, data)
      data = do.call(cbind.data.frame, data)
      data = do.call(cbind.data.frame, data)
      
      #merges with player list to name each attacker
      # I DONT have it name the person who died, but could be added
      data$data.attacker.id <- as.character(data$data.attacker.id)
      data <- merge(data, team_players, by.x = 'data.attacker.id', by.y = 'name')
      # this binds any data
      # I usually filter out more stuff that I want before this part just to 
      # limit the complexity of the resulting output, but whatever
      output <- rbind(output, data)
    }}}

# example of piping with dplyr on this
output %>%
  dplyr::group_by(data.id) %>%
  dplyr::summarise(total_kills = n())
