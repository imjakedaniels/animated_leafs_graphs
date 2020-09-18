### DATA SCRAPING
scrape_reddit <- function(reddit_url, rescrape = FALSE) {
  
  ### TOKEN
  assign("user_agent_reddit", Sys.getenv("USER_AGENT_REDDIT"), envir = globalenv())
  assign("client_id_reddit", Sys.getenv("CLIENT_ID_REDDIT"), envir = globalenv())
  assign("client_secret_reddit", Sys.getenv("CLIENT_SECRET_REDDIT"), envir = globalenv())
  assign("reddit_url", reddit_url, envir = globalenv())
  rescrape <- rescrape
  
  if (file.exists(here::here(str_glue("game-threads/{sport}-gamethread-{main_team_file_format}-{opponent_file_format}-{game_date}.csv"))) == FALSE | rescrape == TRUE) {
    
    source_python("reddit_game_thread_comment_scraper.py")
    
    reddit_df <- py$topics_data %>%
      mutate(created = lubridate::ymd_hms(lubridate::as_datetime(created) - lubridate::hours(12), tz = "America/New_York")) %>% # adjust the timezone to EST
      rename(text = body,
             created_at = created) %>% # make the date & text variable consistent on reddit with twitter
      arrange(created_at) 
    
    write_csv(reddit_df, path = here::here(str_glue("game-threads/{sport}-gamethread-{main_team_file_format}-{opponent_file_format}-{game_date}.csv")))
    
  } 
}
