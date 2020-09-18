### CREATE GAME MARKERS
create_game_markers <- function() {
  
  # get  recent tweets by the official accounts
  twitter_timeline <- rtweet::get_timeline(team_twitter_accounts, n = 200, token = twitter_token) %>%
    select(status_id, screen_name, created_at, text) %>%
    mutate(created_at = lubridate::with_tz(lubridate::ymd_hms(created_at), tzone = "America/New_York")) %>%
    filter(created_at >= game_date,
           created_at <= game_date + lubridate::days(2))
  
  assign("game_start_tweet", twitter_timeline %>% filter(status_id %in% social_game_start),
         envir = globalenv())
  
  assign("game_end_tweet", twitter_timeline %>% filter(status_id %in% social_game_end),
         envir = globalenv())
  
  assign("game_time_df", bind_rows(game_start_tweet, game_end_tweet),
         envir = globalenv())
  
  # goals
  if (sport == "hockey") {
    
    main_team_goals <- twitter_timeline %>%
      filter(status_id %in% main_team_goal_markers)
    
    assign("main_team_goals", twitter_timeline %>% filter(status_id %in% main_team_goal_markers),
           envir = globalenv())
    
    assign("opponent_goals", twitter_timeline %>% filter(status_id %in% opponent_goal_markers),
           envir = globalenv())
    
  }
  
  if (sport == "football") {
    
    main_team_scoring <- twitter_timeline %>%
      filter(status_id %in% main_team_td | status_id %in% main_team_fg) %>%
      mutate(score_type = ifelse(status_id %in% main_team_td, "touchdown", "field goal"))
    
    assign("main_team_scoring", main_team_scoring, envir = globalenv())
    
    opponent_scoring <-  twitter_timeline %>%
      filter(status_id %in% opponent_td | status_id %in% opponent_fg) %>%
      mutate(score_type = ifelse(status_id %in% opponent_td, "touchdown", "field goal"))
    
    assign("main_team_scoring", opponent_scoring, envir = globalenv())
  }
  
  
  ### Intermission markers
  period_marker_df <- twitter_timeline %>%
    filter(status_id %in% period_markers) %>%
    arrange(created_at) %>%
    select(created_at)
  
  intermission_one <- period_marker_df[1,] %>% cbind(period_marker_df[2,])
  intermission_df <- intermission_one[,c(1,2)]
  
  intermission_two <- period_marker_df[3,] %>% cbind(period_marker_df[4,])
  
  if (length(period_markers) == 4) {
    
    intermission_df <-  bind_rows(intermission_df, intermission_two[,c(1,2)]) %>%
      rename(start = created_at, end =  created_at.1)
    
  }
  
  if (length(period_markers) == 6) {
    
    overtime <- period_marker_df[5,] %>% cbind(period_marker_df[6,])
    
    intermission_df <-  bind_rows(intermission_df, intermission_two[,c(1,2)]) %>%
      bind_rows(overtime[,c(1,2)]) %>%
      rename(start = created_at, end =  created_at.1)
    
  } 
  
  if (length(period_markers) == 8) {
    
    overtime <- period_marker_df[5,] %>% cbind(period_marker_df[6,])
    
    double_overtime <- period_marker_df[7,] %>% cbind(period_marker_df[8,])
    
    intermission_df <-  bind_rows(intermission_df, intermission_two[,c(1,2)]) %>%
      bind_rows(overtime[,c(1,2)]) %>%
      bind_rows(double_overtime[,c(1,2)]) %>%
      rename(start = created_at, end =  created_at.1)
    
  }
  
  if (length(period_markers) == 10) {
    
    overtime <- period_marker_df[5,] %>% cbind(period_marker_df[6,])
    
    double_overtime <- period_marker_df[7,] %>% cbind(period_marker_df[8,])
    
    triple_overtime <- period_marker_df[9,] %>% cbind(period_marker_df[10,])
    
    intermission_df <-  bind_rows(intermission_df, intermission_two[,c(1,2)]) %>%
      bind_rows(overtime[,c(1,2)]) %>%
      bind_rows(double_overtime[,c(1,2)]) %>%
      bind_rows(triple_overtime[,c(1,2)]) %>%
      rename(start = created_at, end =  created_at.1)
    
  }
  
  assign("intermission_df", intermission_df, envir = globalenv())
  
  
  
}
