### QUERY
if (!is.null(n)) {
tweet_df <- rtweet::search_tweets(tweet_keywords,
                          since = as.Date(game_date),
                          until = as.Date(game_date) + lubridate::days(2),
                          type = "recent",
                          include_rts = FALSE,
                          retryonratelimit = TRUE,
                          #lang = "en"
                          token = twitter_token) %>% 
  mutate_if(is.list, as.character) %>%
  select(created_at, text, status_id, screen_name)
} else {
  tweet_df <- rtweet::search_tweets(tweet_keywords,
                                    n = n,
                                    type = "recent",
                                    include_rts = FALSE,
                                    retryonratelimit = TRUE,
                                    #lang = "en"
                                    token = twitter_token) %>% 
    mutate_if(is.list, as.character) %>%
    select(created_at, text, status_id, screen_name)
}

vip_1 <- rtweet::get_timelines(vip_list[1:32],
                       n = 100,
                       token = twitter_token) %>%
  select(screen_name, status_id, created_at, text)

vip_2 <- NULL
vip_3 <- NULL
vip_4 <- NULL

if (length(vip_list) > 32) {
  vip_2 <- rtweet::get_timelines(vip_list[33:64],
                         n = 100,
                         token = twitter_token) %>%
    select(screen_name, status_id, created_at, text)
}

if (length(vip_list) > 64) {
  vip_3 <- rtweet::get_timelines(vip_list[65:96],
                         n = 100,
                         token = twitter_token) %>%
    select(screen_name, status_id, created_at, text)
}

if (length(vip_list) > 96) {
  vip_4 <- rtweet::get_timelines(vip_list[97:128],
                         n = 100,
                         token = twitter_token) %>%
    select(screen_name, status_id, created_at, text)
}

vip_df <- bind_rows(vip_1, vip_2, vip_3, vip_4) %>%
  filter(!is.na(screen_name)) %>%
  select(created_at, text)

twitter_df <- tweet_df %>%
  full_join(vip_df) %>%
  group_by(text) %>%
  filter(row_number() == 1) %>% # remove duplicate entries found in both datasets
  ungroup() %>%
  mutate(created_at = lubridate::with_tz(created_at, tzone = "America/New_York"))

write_csv(twitter_df, path = here::here(str_glue("tweets/{sport}-tweets-{main_team_file_format}-{opponent_file_format}-{game_date}.csv")))

rm("twitter_df")
rm("tweet_df")
