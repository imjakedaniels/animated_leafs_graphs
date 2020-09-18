main_team_data <- read_csv("team_metadata.csv") %>%
  filter(team_name == main_team)

opponent_team_data <- read_csv("team_metadata.csv") %>%
  filter(team_name == opponent)

game_date <- as.Date(official_game_start)

team_twitter_accounts <- c(main_team_data$twitter_account, opponent_team_data$twitter_account)

main_colour <- main_team_data$colour_primary
main_secondary_colour <- main_team_data$colour_secondary

opponent_colour <- opponent_team_data$colour_primary
opponent_secondary_colour <- opponent_team_data$colour_secondary

main_acronym <- main_team_data$team_acronym
opponent_acronym <- opponent_team_data$team_acronym

vip_list <- main_team_data$vip_list

sport <- main_team_data$sport

if (is.null(game_social_media_tag)) {
  tweet_keywords <- main_team_data$tweet_keywords
} else {
  tweet_keywords <- paste(main_team_data$tweet_keywords, "OR", game_social_media_tag)
}

# make friendly formats to save my files
opponent_file_format <- str_replace_all(tolower(opponent), " ", "-")
main_team_file_format <- str_replace_all(tolower(main_team), " ", "-")


tokenization <- ifelse(data_source == "Twitter", "tweets", "words")
# social_image <-  ifelse(data_source == "Twitter", "social_images/twitter_icon_small.png", "social_images/reddit_icon_small.png")
# social_colour <- ifelse(data_source == "Twitter", "#1DA1F2", "#FF7B4D")
# subtitle <- ifelse(data_source == "Twitter", "Twitter Tweets", "Reddit Game Thread")

# substitute your own information below
twitter_token <- create_token(app = Sys.getenv("CC_APP"), 
                              consumer_key = Sys.getenv("CC_CONSUMER_KEY"), 
                              consumer_secret = Sys.getenv("CC_CONSUMER_SECRET"),
                              access_token = Sys.getenv("CC_ACCESS_TOKEN"), 
                              access_secret = Sys.getenv("CC_ACCESS_TOKEN_SECRET"))

# get my tokens and secrets stored in my .Renviron
user_agent_reddit <- Sys.getenv("USER_AGENT_REDDIT")
client_id_reddit <- Sys.getenv("CLIENT_ID_REDDIT")
client_secret_reddit <- Sys.getenv("CLIENT_SECRET_REDDIT")

if (data_source == "Reddit") {
  
  if(file.exists(here::here(str_glue("game-threads/{sport}-gamethread-{main_team_file_format}-{opponent_file_format}-{game_date}.csv"))) == FALSE) {
    
    source_python("reddit_game_thread_comment_scraper.py")
    
    raw_df <- py$topics_data %>%
      mutate(created = ymd_hms(as_datetime(created) - hours(12), tz = "America/New_York")) %>% # adjust the timezone to EST
      rename(text = body,
             created_at = created) %>% # make the date & text variable consistent on reddit with twitter
      arrange(created_at)
    
    write_csv(raw_df, path = here::here(str_glue("game-threads/{sport}-gamethread-{main_team_file_format}-{opponent_file_format}-{game_date}.csv")))
    
  } else {
    
    raw_df <- read_csv(here::here(str_glue("game-threads/{sport}-gamethread-{main_team_file_format}-{opponent_file_format}-{game_date}.csv")))
    
  }
}

if (data_source == "Twitter") {
  
  # set to true if you need to re-run script
  rescrape <- TRUE
  
  source("twitter_tweet_scraper.R")
  
  write_csv(raw_df, path = here::here(str_glue("tweets/{sport}-tweets-{main_team_file_format}-{opponent_file_format}-{game_date}.csv")))
}

# get the recent 100 tweets by the leafs official account
twitter_timeline <- get_timeline(team_twitter_accounts, n = 200, token = twitter_token) %>%
  select(status_id, created_at, text) %>%
  mutate(created_at = with_tz(ymd_hms(created_at), tzone = "America/New_York")) %>%
  filter(created_at >= game_date,
         created_at <= game_date + days(2))

main_team_scoring <- twitter_timeline %>%
  filter(status_id %in% main_team_td | status_id %in% main_team_fg) %>%
  mutate(score_type = ifelse(status_id %in% main_team_td, "touchdown", "field goal"))

opponent_scoring <-  twitter_timeline %>%
  filter(status_id %in% opponent_td | status_id %in% opponent_fg) %>%
  mutate(score_type = ifelse(status_id %in% opponent_td, "touchdown", "field goal"))

game_start_tweet <- twitter_timeline %>%
  filter(status_id == social_game_start) %>%
  mutate(created_at = created_at)

game_end_tweet <- twitter_timeline %>%
  filter(status_id == social_game_end) %>%
  mutate(created_at = created_at)

game_time_df <- bind_rows(game_start_tweet, game_end_tweet)

### REDUCE
min_hour <- game_start_tweet$created_at - minutes(15) # filter 15 mins before game start
max_hour <- game_end_tweet$created_at +  minutes(30) # an extra 30 mins after socials say game ended

filtered_df <- raw_df %>%
  filter(created_at >= min_hour,
         created_at <= max_hour) %>%
  mutate(interval = round_date(created_at, "2 mins"))

### CALCULATE VOLUME
interval_volume <- filtered_df %>% 
  group_by(interval) %>%
  summarize(thread_volume = n()) %>%
  ungroup()

### COUNT WORDS
two_min_tokens <- filtered_df %>%
  unnest_tokens(word, text, token = tokenization, drop = FALSE) %>%
  filter(!word %in% unwanted_words, # drop unwanted words 
         nchar(word) >= 2, # words must be at least 2 characters
         !str_detect(word, "^@"), # no twitter handles
         !str_detect(word, "^#"), # no hashtags
         !str_detect(word, "[0-9]+"), # at least two normal letters
         !str_detect(word, "[^\x01-\x7F]"), # no emojis 
         !str_detect(word, "https")) %>% # no links
  anti_join(stop_words, by = "word") %>% #
  mutate(word = str_remove(word, "'s")) %>% # remove possesives
  # filter(word == "defensa")
  count(word, interval) 

### TF-IDF
important_word_df <- two_min_tokens %>%
  bind_tf_idf(word, interval, n) %>%
  filter(idf < 4) %>%
  filter(n >= min_tfidf_threshold) %>% 
  arrange(interval, desc(tf_idf)) %>%
  distinct(interval, .keep_all = T) %>% 
  arrange(interval)

### COMBINE
if (data_source == "Reddit") {
  
  full_data <- interval_volume  %>%
    full_join(important_word_df, by = "interval") %>% 
    fill(word) %>% 
    fill(word, .direction = "up") %>%
    filter(interval >= game_start_tweet$created_at - minutes(15),
           interval <= game_end_tweet$created_at + minutes(15)) %>% 
    arrange(interval)
  
}

if (data_source == "Twitter"){
  
  full_data <- interval_volume  %>%
    inner_join(important_word_df, by = "interval") %>%
    filter(interval >= min_hour,
           interval <= max_hour) %>%
    arrange(interval)
  
}