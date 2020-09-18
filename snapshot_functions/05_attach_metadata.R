attach_metadata <- function(twitter_timeline) {
  
  user_tweets_df <- twitter_timeline %>%
    mutate(rounded_interval = lubridate::round_date(interval, unit = "2 mins")) %>%
    inner_join(interval_volume_df %>%
                 mutate(interval_volume = ifelse(interval_volume < 10, 10, interval_volume)), by = c("rounded_interval" = "interval")) %>%
    full_join(interval_volume_df) %>%
    arrange(interval) %>%
    fill(interval_volume) %>%
    fill(text, .direction = "down") %>%
    full_join(score_sequence %>% rename(interval = created_at), by = "interval") %>%
    arrange(interval) %>%
    fill(running_score, .direction = "down") %>%
    mutate(running_score = ifelse(is.na(running_score), "0 - 0", running_score)) %>%
    full_join(running_game_score_df) %>%
    arrange(interval) %>%
    fill(running_score, .direction = "downup") %>%
    filter(interval %in% twitter_timeline$interval) %>%
    filter(!is.na(status_id.x)) %>%
    select(status_id = status_id.x, screen_name = screen_name.x, interval, text = text.x, rounded_interval, interval_volume, running_score) %>%
    arrange(interval)
  
  return(user_tweets_df)
}