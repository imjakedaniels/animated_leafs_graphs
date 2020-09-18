### LOAD TEAM'S META DATA
load_team_data <- function(main_team, opponent) {
  
  ### load into gloabl environment
  main_team_data <- read_csv("team_metadata.csv") %>%
    filter(team_name == main_team)
  
  opponent_team_data <- read_csv("team_metadata.csv") %>%
    filter(team_name == opponent)
  
  assign("main_team_data", main_team_data, envir = globalenv())
  assign("opponent_team_data", opponent_team_data, envir = globalenv())
  
  if (is.null(game_social_media_tag)) {
    tweet_keywords <- main_team_data$tweet_keywords
  } else {
    tweet_keywords <- paste(main_team_data$tweet_keywords, "OR", game_social_media_tag)
  }
  
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
  assign("tweet_keywords", tweet_keywords, envir = globalenv())
  assign("main_team_file_format", str_replace_all(tolower(main_team), " ", "-"), envir = globalenv())
  assign("opponent_file_format", str_replace_all(tolower(opponent), " ", "-"), envir = globalenv())
  
}


### DATA SCRAPING
scrape_reddit(reddit_url, rescrape = FALSE) {
  
  ### TOKEN
  assign("user_agent_reddit", Sys.getenv("USER_AGENT_REDDIT"), envir = globalenv())
  assign("client_id_reddit", Sys.getenv("CLIENT_ID_REDDIT"), envir = globalenv())
  assign("client_secret_reddit", Sys.getenv("CLIENT_SECRET_REDDIT"), envir = globalenv())
  assign("reddit_url", reddit_url, envir = globalenv())
  
  if (file.exists(here::here(str_glue("game-threads/{sport}-gamethread-{main_team_file_format}-{opponent_file_format}-{game_date}.csv"))) == FALSE | rescrape = TRUE) {
    
    rescrape <- rescrape
    
    source_python("reddit_game_thread_comment_scraper.py")
    
    reddit_df <- py$topics_data %>%
      mutate(created = lubridate::ymd_hms(lubridate::as_datetime(created) - hours(12), tz = "America/New_York")) %>% # adjust the timezone to EST
      rename(text = body,
             created_at = created) %>% # make the date & text variable consistent on reddit with twitter
      arrange(created_at) 
    
    write_csv(reddit_df, path = here::here(str_glue("game-threads/{sport}-gamethread-{main_team_file_format}-{opponent_file_format}-{game_date}.csv")))
    
  } 
}

scrape_twitter(tweet_keywords, game_date, rescrape = FALSE) {
  

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
  
  assign("since", as.Date(game_date), envir = globalenv())
  assign("until", since + lubridate::days(2), envir = globalenv())
  assign(vip_list <- str_split(vip_df, ", ") %>% unlist(), envir = globalenv())
  
  rescrape <- rescrape
  
  if (file.exists(here::here(str_glue("tweets/{sport}-tweets-{main_team_file_format}-{opponent_file_format}-{game_date}.csv"))) == FALSE | rescrape == TRUE) {

    source("twitter_tweet_scraper.R")
    
    write_csv(twitter_df, path = here::here(str_glue("tweets/{sport}-tweets-{main_team_file_format}-{opponent_file_format}-{game_date}.csv")))
  }
} 

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


execute_tfidf <- function(min_tfidf_threshold, bad_phrase, bad_tokens) {
  
  ### ROUND DATES
  min_hour <- game_start_tweet$created_at - lubridate::minutes(15) # filter 15 mins before game start
  max_hour <- game_end_tweet$created_at +  lubridate::minutes(30) # an extra 30 mins after socials say game ended
  
  raw_df <- read_csv(here::here(str_glue("tweets/{sport}-tweets-{main_team_file_format}-{opponent_file_format}-{game_date}.csv"))) %>%
    bind_rows(read_csv(here::here(str_glue("game-threads/{sport}-gamethread-{main_team_file_format}-{opponent_file_format}-{game_date}.csv"))) %>%
                mutate(status_id = NA_integer_,
                       screen_name = NA_character_), .id = "source") %>%
    mutate(created_at = force_tz(created_at, "America/New_York") - hours(4),
           source = ifelse(source == 1, "twitter", "reddit"))
  
  rounded_intervals_df <- raw_df %>%
    filter(created_at >= min_hour,
           created_at <= max_hour) %>%
    mutate(interval = lubridate::round_date(created_at, "2 mins")) %>%
    arrange(created_at)
  
  assign("rounded_intervals_df", rounded_intervals_df, envir = globalenv())
  
  ### CALCULATE VOLUME
  
  interval_volume_df <- rounded_intervals_df %>% 
    count(interval, name = "interval_volume")
  
  assign("interval_volume_df", interval_volume_df, envir = globalenv())
  
  ### TOKENIZE COMMENTS
  
  unwanted_words <- c("#leafsforever", "10u", "t.co", "gotta", "games", "hockey", "dont", "amp", "period", 
                      "region", "https", "10a", "pas", "att", "gonna", "ive", "les", "game", "hes",  "vai", 
                      "ml\U0001f4b5", "lieut", "vous", "weve", "ill", "theyre", "isnt", "youre", "o55", "bla", 
                      "guys", "row", "usa", "och", "temporada", "07mar", "playing", "play", "plays", "taking", 
                      "happen", "people", "teams", "team", "people", "game", "looked", "stream", "10mar", "est", 
                      "amo", "des", "los", "bay", "cuz", "sur", "didnt", "doesnt", "watch", "ssin", "está", "ppl", 
                      "#leafsnation", "watching", "fan", "youve", "nos", "mais", "vmies", "muito", "def", "times", 
                      "cat", "templeofthececi", "een", "tor", "sf","im", "el",  "ha", "thy", "pm", "usanhl", "app", 
                      "sugar", "kim", "de", "la", "offs", "aux", "wasnt", "ela", "jogo", "ot", "na", "doesn", "i’ve", 
                      "teams", "hockey", "team", "people", "game", "dont", "looked", "stream","ago", "aren", 
                      "questrade", "nhl com", "ins", "harvey's", "confirm", "dun", "nbsp", "they’re", "makes", 
                      "periods", "players", "watch", "lot", "didnt", "giphy.gif", "cbc", "def", "im", 
                      "media.giphy.com","tweet", "games", "story", "lander",  "doo",  "jpg", "heres",  "channel", 
                      "person", "snowmen", "played", "dos",  "san", "player", "based", "vis", "don", "pts", "fox", 
                      "dal", "hulu", "renaud", "channels",  "poxa", "ov", "tt", "cest", "raffl", "http://", "hey", 
                      "saadtoewskane", "golden", "ur", "youd", "da", "ct", "couldve", "em", "arent", "id", "gdt", 
                      "tv", "ah", "ms", "guy", "os", "sports", "fx", "hed", "foi", "uma", "gt", "hashtag", "soccer", 
                      "bilasport", "ma", "le", "eu", "ml", "une", "pan", "en", "lavalanche",  "havent", "youtube", 
                      "thatd", "mas", "krugcarlo", "playo", "penalidade", "theyd", "itll", "und", "bit", "ich", 
                      "ont", "ja", "votre", "tu", "mon", "nous", "thee", "leur", "wouldve", "wont", "contre", "shes", 
                      "wouldnt", "por", "buts", "keeping", "quon", "ils",  "mans", "mid", "pic", "penis", "ac", 
                      "dar",  "oven", "ainda", "du", "password", "sourdough", "hasnt", "rn", "essa", "qui", "playoff", 
                      "phi",  "todo", "si", "pra", "meant", "emu", "words",  "mesmo", "vamos", "olha", "nhl", "med", 
                      "nie", "aula", "vpn", "whats", "col", "au", "je",  "snp", "faire", "avg", "twitter", "talking", 
                      "list", "min", "ai", "round", "wheres","left", "song", "homo", "aug", "sn",  "meet", "bos", 
                      "til", "qf", "il", "mine", "shouldnt","aurait", "pc", "con", "bien", "esti", "veux", "veut", 
                      "ni", "account", "spending", "cinq", "jouer", "paypal", "juste", "tick", "buying", "idea", 
                      "sont", "mal", "sir", "suspendu", "retrieved", "arbitres", "chino", "theyve", "para", "allez", 
                      "aint", "gif", "pt", "doit", "east", "cough", "blue", "sandwich", "youll", "kennedy", 
                      "nordstromkuralywagner", "theyll", "ope", "tonights", "se", "australia", "briga", "local", 
                      "day", "wear", "episode", "raiders", "supposed", "sunday", "dbz", "upvotes", "likes", "sap", 
                      "yr", "question", "sc", "cc", "dip", "covering",  "pelechpulock", "page", "description", 
                      "sold",  "season", "convention", "smithd", "week", "rhockey", "sunflower", "las", "gols", 
                      "te",  "bem", "seeds", "word", "pct", "numerique", "shouldve", "fiq", "mount", "cu", "lots", 
                      "told", "lag", "sharks", "heard", "dan", "sv",  "kitchen", "thread", "taxidermy", "nyt",
                      "goina", "tb", "muitos", "||", "tom", "din", "portland",  "ol", "calma", "sexta", "ty", 
                      "pero", "comment", "nao", "avs",  "fim", "van", "mtv", "detroit", "grammar", "tara", 
                      "pritteee", "red", "baba", "lauren", "nhls", "pelechgood", "findinspireinformempower", 
                      "reply", "monday", "vgk", "pee", "knight", "weeks", "dodgers","minutos", "fiverr", "rim", 
                      "utah", "jai", "av", "bon", "gp", "marxist", "jerseys", "download", "vou", "brings", "mil", 
                      "del", "jogos", "emoji", "ca", "alexa", "days", "conf", "tout", "partido", "apr", "sput", 
                      "dem", "ad", "ish", "land", "oc", "hr", "serie", "nbspnbsp", "gtgt", "bir", "mecz", "sigue",
                      "niveau", "ima", "quien", "minute", "minutes", "putting")
  
  unnested_df <- rounded_intervals_df %>%
    filter(!str_detect(text, "#LoveIslandUSA")) %>% # remove any spam accounts here
    filter(!str_detect(text, "#Chatterstats")) %>%
    filter(!str_detect(text, "#LC Report Card")) %>%
    filter(!str_detect(text, "#bucciovertimechallenge")) %>%
    filter(!str_detect(text, bad_phrase)) %>%
    tidytext::unnest_tokens(word, text, token = "tweets", drop = FALSE) %>%
    mutate(word = str_remove(word, "'s")) %>%
    filter(!word %in% unwanted_words, # drop unwanted words 
           !word %in% bad_tokens,
           nchar(word) >= 2, # words must be at least 2 characters
           !str_detect(word, "^@"), # no twitter handles
           !str_detect(word, "^#"), # no hashtags
           !str_detect(word, "[0-9]+"), # at least two normal letters
           !str_detect(word, "[^\x01-\x7F]"), # no emojis 
           !str_detect(word, "https")) %>% # no links
    anti_join(stop_words, by = "word")
    
  ### COUNT TOKENS
  
  processed_df <- unnested_df %>%
    count(word, interval) %>%
    arrange(interval)
  
  
  ### TF-IDF
  
  important_word_df <- processed_df %>%
    tidytext::bind_tf_idf(word, interval, n) %>%
    filter(idf < 4) %>%
    filter(n >= min_tfidf_threshold) %>% 
    arrange(interval, desc(tf_idf)) %>%
    distinct(interval, .keep_all = TRUE) %>% 
    arrange(interval)
  
  
  ### COMBINE
  
  full_data <- interval_volume_df  %>%
    full_join(important_word_df, by = "interval") %>%
    filter(interval >= min_hour,
           interval <= max_hour) %>%
    arrange(interval) %>%
    fill(word, .direction = "downup")
  
  assign("full_data", full_data, envir = globalenv())
  
  beepr::beep(sound = 2)
  message(paste("Total Comments:", sum(full_data$interval_volume)))
  message(paste("Max Volume:", max(full_data$interval_volume)))
  
  return(full_data)
  
}

create_scoring <- function(score_break_1 = NULL, correct_score_1 = NULL,
                           score_break_2 = NULL, correct_score_2 = NULL) {

  if (sport == "hockey") {
  ### FIND GOALS
  goal_df <- main_team_goals  %>%
    bind_rows(opponent_goals, .id = "team") %>%
    mutate(linetype = ifelse(team == 1, 2, 5),
           colour = ifelse(team == 1, main_colour, opponent_colour))
  
  ### CREATE SCORING
  scoring_df <-  main_team_goals  %>%
    bind_rows(opponent_goals, .id = "team") %>%
    mutate(team = ifelse(team == 1, "main_team", "opponent")) %>%
    mutate(linetype = 2,
           colour = ifelse(team == "main_team", main_colour, opponent_colour),
           points = 1,
           size = 0.75) %>%
    group_by(team) %>%
    arrange(created_at) %>%
    mutate(score = cumsum(points)) %>%
    spread(team, score) %>%
    fill(length(.), length(.) - 1)
  
  }
  
  scoring_df[, 9][is.na(scoring_df[, 9])] <- 0
  
  ## OVERTIME FIX
  
  if (length(scoring_df) == 10) {
    
    scoring_df[, 10][is.na(scoring_df[, 10])] <- 0
    
  }
  
  if (length(scoring_df) == 10) {
    
    score_sequence <- scoring_df %>%
      mutate(running_score = str_glue("{main_team} - {opponent}")) 
    
  } else {
    
    if (sum(str_detect(names(scoring_df), "main_team")) == 1) {
      
      score_sequence <- scoring_df %>%
        mutate(running_score = str_glue("{main_team} - 0")) 
      
    } else {
      
      score_sequence <- scoring_df %>%
        mutate(running_score = str_glue("0 - {opponent}")) 
      
    }
  }
  
  assign("score_sequence", score_sequence, envir = globalenv())
  assign("scoring_df", scoring_df, envir = globalenv())
  
  ### SCOREBOARD
  running_game_score_df <- score_sequence %>%
    rename(interval = created_at) %>%
    mutate(running_score = as.character(running_score)) %>%
    bind_rows(data.frame(interval = score_sequence$created_at + lubridate::minutes(2), # iser
                         running_score = as.character(score_sequence$running_score))) %>%
    mutate(board_size = 15) %>%
    full_join(full_data, by = "interval") %>%
    arrange(interval) %>%
    mutate(board_size = ifelse(is.na(running_score), 12, board_size)) %>%
    mutate(board_size = ifelse(interval %in% lubridate::round_date(scoring_df$created_at + lubridate::minutes(1), "2 minutes"), 15, board_size)) %>%
    arrange(interval) %>%
    fill(running_score, .direction = "down") %>%
    mutate(running_score = ifelse(is.na(running_score), "0 - 0", running_score)) %>%
    mutate(board_size = ifelse(running_score == "0 - 0", 12, board_size)) %>%
    select(interval, running_score, board_size) 
  
  if (!is.null(score_break_1)) {
    running_game_score_df %>% 
      mutate(running_score = ifelse(interval %in% score_break_1, correct_score_1, running_score))
  
  }
  
  if (!is.null(score_break_2)) {
    running_game_score_df %>% 
      mutate(running_score = ifelse(interval %in% score_break_2, correct_score_2, running_score))
    
  }
  
  assign("running_game_score_df", running_game_score_df, envir = globalenv())
  
  ### "FINAL" ON SCOREBOARD
  final_score_text <- running_game_score_df %>%
    mutate(text = ifelse(interval >= game_end_tweet$created_at, "Final", ""))
  
  assign("final_score_text", final_score_text, envir = globalenv())
  
  ### TEAM GOAL LABELS
  team_scoring_markers <- scoring_df %>%
    select(created_at, colour) %>%
    mutate(team_acronym = ifelse(colour == main_team_data$colour_primary,  main_team_data$team_acronym, opponent_team_data$team_acronym),
           team_bg_fill =  ifelse(colour == main_team_data$colour_primary,  main_team_data$colour_primary, opponent_team_data$colour_primary),
           team_text_colour = ifelse(colour == main_team_data$colour_primary,  "white", "white"),
           y_location = ifelse(colour == main_team_data$colour_primary, ifelse(max(full_data$interval_volume) < 1000, 1000, max(full_data$interval_volume)), 3)) %>%
    rename(interval = created_at) %>%
    inner_join(running_game_score_df, by = "interval") %>%
    rename(created_at = interval)
  
  assign("team_scoring_markers", team_scoring_markers, envir = globalenv())
  
}

token_eda <- function(sketchy_word) {
  unnested_df <- rounded_intervals_df %>%
    filter(!str_detect(text, "#LoveIslandUSA")) %>% # remove any spam accounts here
    filter(!str_detect(text, "#Chatterstats")) %>%
    filter(!str_detect(text, "#LC Report Card")) %>%
    filter(!str_detect(text, "#bucciovertimechallenge")) %>%
    filter(!str_detect(text, bad_phrase)) %>%
    unnest_tokens(word, text, token = "tweets", drop = FALSE) %>%
    mutate(word = str_remove(word, "'s")) %>%
    filter(!word %in% unwanted_words, # drop unwanted words 
           !word %in% bad_tokens,
           nchar(word) >= 2, # words must be at least 2 characters
           !str_detect(word, "^@"), # no twitter handles
           !str_detect(word, "^#"), # no hashtags
           !str_detect(word, "[0-9]+"), # at least two normal letters
           !str_detect(word, "[^\x01-\x7F]"), # no emojis 
           !str_detect(word, "https")) %>% # no links
    anti_join(stop_words, by = "word")
  
  token_df <- unnested_df %>% 
    filter(word == sketchy_word) %>%
    select(text, created_at)
  
  return(token_df)
  
}