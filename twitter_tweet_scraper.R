# if you've already scraped the data, you won't need to do it again if you run all chunks
if(file.exists(here::here(str_glue("tweets/{sport}-tweets-{main_team_file_format}-{opponent_file_format}-{game_date}.csv"))) == FALSE | rescrape == TRUE) {
  
  since <- as.Date(game_date) #get the data since today and
  until <- since + days(2) # until two days after so i get all of yesterday
  
  # save tweets locally
  tweet_df <- search_tweets(tweet_keywords,
                            n = 30000,
                            since = since,
                            until = until,
                            type = "recent",
                            include_rts = FALSE,
                            retryonratelimit = TRUE,
                            token = twitter_token) %>%
    mutate_if(is.list, as.character) %>%
    select(created_at, text)
  
 vip_df  <- read_csv("team_metadata.csv") %>%
    filter(team_name == main_team) %>%
    select(vip_list)
   
 vip_list <- str_split(vip_df, ", ") %>%
   unlist()

  vip_1 <- get_timelines(vip_list[1:32],
                                n = 100,
                                token = twitter_token) %>%
    select(screen_name, created_at, text)
  
  vip_2 <- NULL
  vip_3 <- NULL
  vip_4 <- NULL
  
  if (length(vip_list) > 32) {
  vip_2 <- get_timelines(vip_list[33:64],
                                n = 100,
                                token = twitter_token) %>%
    select(screen_name, created_at, text)
  }
  
  if (length(vip_list) > 64) {
  vip_3 <- get_timelines(vip_list[65:96],
                                n = 100,
                                token = twitter_token) %>%
    select(screen_name, created_at, text)
  }
  
  if (length(vip_list) > 96) {
  vip_4 <- get_timelines(vip_list[97:128],
                                n = 100,
                                token = twitter_token) %>%
    select(screen_name, created_at, text)
  }
                          
  vip_df <- bind_rows(vip_1, vip_2, vip_3, vip_4) %>%
    filter(!is.na(screen_name)) %>%
    select(created_at, text)
  
  twitter_df <- tweet_df %>%
    full_join(vip_df) %>%
    group_by(text) %>%
    filter(row_number() == 1) %>% # remove duplicate entries found in both datasets
    ungroup() %>%
    mutate(created_at = with_tz(created_at, tzone = "America/New_York"))
  
}

