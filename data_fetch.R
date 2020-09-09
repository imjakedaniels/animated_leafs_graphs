
### LOAD TEAM'S META DATA

main_team_data <- read_csv("team_metadata.csv") %>%
  filter(team_name == main_team)

opponent_team_data <- read_csv("team_metadata.csv") %>%
  filter(team_name == opponent)

### CREATE VARAIBLES
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

# friendly format to save files
opponent_file_format <- str_replace_all(tolower(opponent), " ", "-")
main_team_file_format <- str_replace_all(tolower(main_team), " ", "-")


### TOKENS

# get my tokens and secrets stored in my .Renviron
twitter_token <- create_token(app = Sys.getenv("CC_APP"), 
                              consumer_key = Sys.getenv("CC_CONSUMER_KEY"), 
                              consumer_secret = Sys.getenv("CC_CONSUMER_SECRET"),
                              access_token = Sys.getenv("CC_ACCESS_TOKEN"), 
                              access_secret = Sys.getenv("CC_ACCESS_TOKEN_SECRET"))

user_agent_reddit <- Sys.getenv("USER_AGENT_REDDIT")
client_id_reddit <- Sys.getenv("CLIENT_ID_REDDIT")
client_secret_reddit <- Sys.getenv("CLIENT_SECRET_REDDIT")


### DATA SCRAPING

if (file.exists(here::here(str_glue("game-threads/{sport}-gamethread-{main_team_file_format}-{opponent_file_format}-{game_date}.csv"))) == FALSE) {
  
  tictoc::tic("Scraped Reddit")
  
  source_python("reddit_game_thread_comment_scraper.py")
  
  reddit_df <- py$topics_data %>%
    mutate(created = ymd_hms(as_datetime(created) - hours(12), tz = "America/New_York")) %>% # adjust the timezone to EST
    rename(text = body,
           created_at = created) %>% # make the date & text variable consistent on reddit with twitter
    arrange(created_at) 
  
  write_csv(reddit_df, path = here::here(str_glue("game-threads/{sport}-gamethread-{main_team_file_format}-{opponent_file_format}-{game_date}.csv")))
  
  tictoc::toc()
  
}

if (file.exists(here::here(str_glue("tweets/{sport}-tweets-{main_team_file_format}-{opponent_file_format}-{game_date}.csv"))) == FALSE | rescrape == TRUE) {
  
  tictoc::tic("Scraped Tweets")
  
  source("twitter_tweet_scraper.R")
  
  write_csv(twitter_df, path = here::here(str_glue("tweets/{sport}-tweets-{main_team_file_format}-{opponent_file_format}-{game_date}.csv")))
  
  tictoc::toc()
  
} 

raw_df <- read_csv(here::here(str_glue("tweets/{sport}-tweets-{main_team_file_format}-{opponent_file_format}-{game_date}.csv"))) %>%
  bind_rows(read_csv(here::here(str_glue("game-threads/{sport}-gamethread-{main_team_file_format}-{opponent_file_format}-{game_date}.csv"))), .id = "source") %>%
  mutate(created_at = force_tz(created_at, "America/New_York") - hours(4),
         source = ifelse(source == 1, "twitter", "reddit"))

### CREATE GAME MARKERS

# get  recent tweets by the official accounts
twitter_timeline <- get_timeline(team_twitter_accounts, n = 3500, token = twitter_token) %>%
  select(status_id, created_at, text) %>%
  mutate(created_at = with_tz(ymd_hms(created_at), tzone = "America/New_York")) %>%
  filter(created_at >= game_date,
         created_at <= game_date + days(2))

# goals
if (sport == "hockey") {
  
  main_team_goals <- twitter_timeline %>%
    filter(status_id %in% main_team_goal_markers)
  
  opponent_goals <- twitter_timeline %>%
    filter(status_id %in% opponent_goal_markers)
  
}

if (sport == "football") {
  
  main_team_scoring <- twitter_timeline %>%
    filter(status_id %in% main_team_td | status_id %in% main_team_fg) %>%
    mutate(score_type = ifelse(status_id %in% main_team_td, "touchdown", "field goal"))
  
  opponent_scoring <-  twitter_timeline %>%
    filter(status_id %in% opponent_td | status_id %in% opponent_fg) %>%
    mutate(score_type = ifelse(status_id %in% opponent_td, "touchdown", "field goal"))
  
}

# game time
game_start_tweet <- twitter_timeline %>%
  filter(status_id == social_game_start) 

game_end_tweet <- twitter_timeline %>%
  filter(status_id == social_game_end)

game_time_df <- bind_rows(game_start_tweet, game_end_tweet)


### ROUND DATES

min_hour <- game_start_tweet$created_at - minutes(15) # filter 15 mins before game start
max_hour <- game_end_tweet$created_at +  minutes(30) # an extra 30 mins after socials say game ended

rounded_intervals_df <- raw_df %>%
  filter(created_at >= min_hour,
         created_at <= max_hour) %>%
  mutate(interval = round_date(created_at, "2 mins")) %>%
  arrange(created_at)


### CALCULATE VOLUME

interval_volume_df <- rounded_intervals_df %>% 
  count(interval, name = "interval_volume")


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
                    "dem", "ad", "ish", "land", "oc", "hr", "serie")

unnested_df <- rounded_intervals_df %>%
  filter(!str_detect(text, "#ThePressRowShow")) %>% # remove any spam accounts here
  filter(!str_detect(text, "#Chatterstats")) %>%
  filter(!str_detect(text, "#LC Report Card")) %>%
  filter(!str_detect(text, "@CIA")) %>%
  filter(!str_detect(text, "@StapeAthletic")) %>%
  unnest_tokens(word, text, token = "tweets", drop = FALSE) %>%
  filter(!word %in% unwanted_words, # drop unwanted words 
         !word %in% unwanted_words_again,
         nchar(word) >= 2, # words must be at least 2 characters
         !str_detect(word, "^@"), # no twitter handles
         !str_detect(word, "^#"), # no hashtags
         !str_detect(word, "[0-9]+"), # at least two normal letters
         !str_detect(word, "[^\x01-\x7F]"), # no emojis 
         !str_detect(word, "https")) %>% # no links
  anti_join(stop_words, by = "word") %>% #
  mutate(word = str_remove(word, "'s")) 


### COUNT TOKENS

processed_df <- unnested_df %>%
  count(word, interval) %>%
  arrange(interval)


### TF-IDF

important_word_df <- processed_df %>%
  bind_tf_idf(word, interval, n) %>%
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
