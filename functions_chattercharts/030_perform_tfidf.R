perform_tfidf <- function(min_tfidf_threshold, bad_phrase, bad_tokens) {
  
  ### ROUND DATES
  min_hour <- game_start_tweet$created_at - lubridate::minutes(15) # filter 15 mins before game start
  max_hour <- game_end_tweet$created_at +  lubridate::minutes(30) # an extra 30 mins after socials say game ended
  
  raw_df <- read_csv(here::here(str_glue("tweets/{sport}-tweets-{main_team_file_format}-{opponent_file_format}-{game_date}.csv"))) %>%
    bind_rows(read_csv(here::here(str_glue("game-threads/{sport}-gamethread-{main_team_file_format}-{opponent_file_format}-{game_date}.csv"))) %>%
                mutate(status_id = NA_integer_,
                       screen_name = NA_character_), .id = "source") %>%
    mutate(created_at = lubridate::force_tz(created_at, "America/New_York") - lubridate::hours(4),
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
    anti_join(tidytext::stop_words %>% 
                filter(lexicon %in% c("SMART", "snowball")), 
              by = "word")
  
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