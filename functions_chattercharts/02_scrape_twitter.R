scrape_twitter <- function(game_date, rescrape = FALSE, n = NULL) {
  
  
  # get my tokens and secrets stored in my .Renviron
  assign("twitter_token",  rtweet::create_token(app = Sys.getenv("CC_APP"), 
                                                consumer_key = Sys.getenv("CC_CONSUMER_KEY"), 
                                                consumer_secret = Sys.getenv("CC_CONSUMER_SECRET"),
                                                access_token = Sys.getenv("CC_ACCESS_TOKEN"), 
                                                access_secret = Sys.getenv("CC_ACCESS_TOKEN_SECRET")), 
         envir = globalenv())
  
  ### SCRAPE VIP LIST
  vip_df  <- read_csv("team_metadata.csv") %>%
    filter(team_name == main_team) %>%
    select(vip_list)
  
  vip_list <- str_split(vip_df, ", ") %>%
    unlist()
  
  assign("vip_list",  str_split(vip_df, ", ") %>% unlist(), envir = globalenv())
  
  rescrape <- rescrape
  
  if (file.exists(here::here(str_glue("tweets/{sport}-tweets-{main_team_file_format}-{opponent_file_format}-{game_date}.csv"))) == FALSE | rescrape == TRUE) {
    
    source(here::here("twitter_tweet_scraper.R"))
    
  }
} 