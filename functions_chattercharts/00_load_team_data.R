### LOAD TEAM'S META DATA
load_team_data <- function(main_team, opponent) {
  
  ### load into gloabl environment
  main_team_data <- read_csv("team_metadata.csv") %>%
    filter(team_name == main_team)
  
  opponent_team_data <- read_csv("team_metadata.csv") %>%
    filter(team_name == opponent)
  
  message("Loading Team-Releated Attributes and Social Media Info from team_metadata.csv")
  
  assign("main_team_data", main_team_data, envir = globalenv())
  assign("opponent_team_data", opponent_team_data, envir = globalenv())
  assign("team_twitter_accounts", c(main_team_data$twitter_account, opponent_team_data$twitter_account), envir = globalenv())
  assign("main_colour", main_team_data$colour_primary, envir = globalenv())
  assign("main_secondary_colour", main_team_data$colour_secondary, envir = globalenv())
  assign("opponent_colour", opponent_team_data$colour_primary, envir = globalenv())
  assign("opponent_secondary_colour", opponent_team_data$colour_secondary, envir = globalenv())
  assign("main_acronym", main_team_data$team_acronym, envir = globalenv())
  assign("opponent_acronym", opponent_team_data$team_acronym, envir = globalenv())
  assign("game_social_media_tag", str_glue("#{main_acronym}vs{opponent_acronym}"), envir = globalenv())
  assign("vip_list", main_team_data$vip_list, envir = globalenv())
  assign("sport", main_team_data$sport, envir = globalenv())
  assign("tweet_keywords", paste(main_team_data$tweet_keywords, "OR", game_social_media_tag), envir = globalenv())
  assign("main_team_file_format", str_replace_all(tolower(main_team), " ", "-"), envir = globalenv())
  assign("opponent_file_format", str_replace_all(tolower(opponent), " ", "-"), envir = globalenv())
  
}