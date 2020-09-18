fetch_timelines <- function(account_names) {
  
  # get my tokens and secrets stored in my .Renviron
  twitter_token <- rtweet::create_token(app = Sys.getenv("CC_APP"), 
                                consumer_key = Sys.getenv("CC_CONSUMER_KEY"), 
                                consumer_secret = Sys.getenv("CC_CONSUMER_SECRET"),
                                access_token = Sys.getenv("CC_ACCESS_TOKEN"), 
                                access_secret = Sys.getenv("CC_ACCESS_TOKEN_SECRET"))
  
  message(str_glue("Getting your Tweets"))
  
  user_timelines <- rtweet::get_timelines(account_names, n = 100, token = twitter_token) %>%
    filter(created_at > min_hour,
           created_at < max_hour) %>%
    filter(is.na(reply_to_status_id)) %>%
    filter(is_quote == FALSE, is_retweet == FALSE) %>%
    filter(!str_detect(text, "^https")) %>%
    select(status_id, created_at, text, screen_name) %>%
    mutate(created_at = lubridate::with_tz(lubridate::ymd_hms(created_at), tzone = "America/New_York")) 
  
  if (include_team == TRUE) {
    
    official_game_markers <- data.frame(markers = as.character(c(social_game_start, main_team_goal_markers, 
                                                                 opponent_goal_markers, social_game_end, period_markers)))
    
    # HARD STORE THIS EVENTUALLY
    team_timeline <- rtweet::get_timelines(main_team_data$twitter_account, n = 100, 
                                   token = twitter_token) %>%
      filter(status_id %in% official_game_markers$markers) %>%
      select(status_id, created_at, text, screen_name) %>%
      mutate(created_at = lubridate::with_tz(lubridate::ymd_hms(created_at), tzone = "America/New_York")) 
    
    twitter_timeline <- user_timelines %>%
      bind_rows(team_timeline) %>%
      rename(interval = created_at) %>%
      arrange(interval)
    
  } else {
    
    twitter_timeline <- user_timelines %>%
      rename(interval = created_at) %>%
      arrange(interval)
    
  }
  
}