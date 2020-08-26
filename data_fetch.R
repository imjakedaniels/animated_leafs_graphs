main_team_data <- read_csv("team_metadata.csv") %>%
  filter(team_name == main_team)

opponent_team_data <- read_csv("team_metadata.csv") %>%
  filter(team_name == opponent)

team_twitter_accounts <- c(main_team_data$twitter_account, opponent_team_data$twitter_account)

main_colour <- main_team_data$colour_primary
main_secondary_colour <- main_team_data$colour_secondary

opponent_colour <- opponent_team_data$colour_primary
opponent_secondary_colour <- opponent_team_data$colour_secondary

main_acronym <- main_team_data$team_acronym
opponent_acronym <- opponent_team_data$team_acronym

game_social_media_tag <- str_glue("#{main_acronym}vs{opponent_acronym}")

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


if (file.exists(here::here(str_glue("game-threads/{sport}-gamethread-{main_team_file_format}-{opponent_file_format}-{game_date}.csv"))) == FALSE) {
  
  tictoc::tic("Scraping Reddit Thread")
  
  source_python("reddit_game_thread_comment_scraper.py")
  
  reddit_df <- py$topics_data %>%
    mutate(created = ymd_hms(as_datetime(created) - hours(12), tz = "America/New_York")) %>% # adjust the timezone to EST
    rename(text = body,
           created_at = created) %>% # make the date & text variable consistent on reddit with twitter
    arrange(created_at) 
  
  write_csv(reddit_df, path = here::here(str_glue("game-threads/{sport}-gamethread-{main_team_file_format}-{opponent_file_format}-{game_date}.csv")))
  
  tictoc::toc()
  
} else {
  
  reddit_df <- read_csv(here::here(str_glue("game-threads/{sport}-gamethread-{main_team_file_format}-{opponent_file_format}-{game_date}.csv")))
  
}

if (file.exists(here::here(str_glue("tweets/{sport}-tweets-{main_team_file_format}-{opponent_file_format}-{game_date}.csv"))) == FALSE | rescrape == TRUE) {
  

  tictoc::tic("Gathering Tweets")
  
  source("twitter_tweet_scraper.R")
  
  write_csv(twitter_df, path = here::here(str_glue("tweets/{sport}-tweets-{main_team_file_format}-{opponent_file_format}-{game_date}.csv")))
  
  tictoc::toc()
  
} else {
  
  twitter_df <- read_csv(here::here(str_glue("tweets/{sport}-tweets-{main_team_file_format}-{opponent_file_format}-{game_date}.csv")))
  
}

raw_df <- read_csv(here::here(str_glue("tweets/{sport}-tweets-{main_team_file_format}-{opponent_file_format}-{game_date}.csv"))) %>%
  bind_rows(read_csv(here::here(str_glue("game-threads/{sport}-gamethread-{main_team_file_format}-{opponent_file_format}-{game_date}.csv"))), .id = "source") %>%
  mutate(created_at = force_tz(created_at, "America/New_York") - hours(4),
         source = ifelse(source == 1, "twitter", "reddit"))

# get the recent 100 tweets by the official account
twitter_timeline <- get_timeline(team_twitter_accounts, n = 200, token = twitter_token) %>%
  select(status_id, created_at, text) %>%
  mutate(created_at = with_tz(ymd_hms(created_at), tzone = "America/New_York")) %>%
  filter(created_at >= game_date,
         created_at <= game_date + days(2))

if (sport == "hockey") {
main_team_goals <- twitter_timeline %>%
  filter(status_id %in% main_team_goal_markers)

opponent_goals <- twitter_timeline %>%
  filter(status_id %in% opponent_goal_markers)
}

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
  mutate(interval = round_date(created_at, "2 mins")) %>%
  arrange(created_at)

### CALCULATE VOLUME
interval_volume <- filtered_df %>% 
  group_by(interval) %>%
  summarize(thread_volume = n()) %>%
  ungroup()

### COUNT WORDS
check_df <- filtered_df %>%
  unnest_tokens(word, text, token = "tweets", drop = FALSE) %>%
  filter(!word %in% unwanted_words, # drop unwanted words 
         nchar(word) >= 2, # words must be at least 2 characters
         !str_detect(word, "^@"), # no twitter handles
         !str_detect(word, "^#"), # no hashtags
         !str_detect(word, "[0-9]+"), # at least two normal letters
         !str_detect(word, "[^\x01-\x7F]"), # no emojis 
         !str_detect(word, "https")) %>% # no links
  anti_join(stop_words, by = "word") %>% #
  mutate(word = str_remove(word, "'s")) 

two_min_tokens <- check_df %>%
  count(word, interval) %>%
  arrange(interval)

### TF-IDF
important_word_df <- two_min_tokens %>%
  bind_tf_idf(word, interval, n) %>%
  filter(idf < 4) %>%
  filter(n >= min_tfidf_threshold) %>% 
  arrange(interval, desc(tf_idf)) %>%
  distinct(interval, .keep_all = T) %>% 
  arrange(interval)

### COMBINE
full_data <- interval_volume  %>%
  full_join(important_word_df, by = "interval") %>%
  filter(interval >= min_hour,
         interval <= max_hour) %>%
  arrange(interval) %>%
  fill(word, .direction = "downup") 