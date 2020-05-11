
# Animating Twitter Chatter throughout the course of a Leafs Game

[![Example](https://github.com/imjakedaniels/animated_leafs_graphs/blob/master/animations/twitter_reddit-leafs-tampa-bay-lightning-2020-03-10.gif)](https://www.reddit.com/r/leafs/comments/e80uu4/twitter_chatter_leafs_vs_blues_december_07_2019/)

I post these animations to [r/Leafs](https://www.reddit.com/r/leafs/comments/e80uu4/twitter_chatter_leafs_vs_blues_december_07_2019/)
---
title: "reddit"
output: html_document
---

# Animation Notebook

In this notebook, I use R & Python to scrape Leafs comments off Twitter and Reddit, scrape relevant game metadata from various sources (ex. goal timestamps, team colours, start of game timestamps) by cleaning website data using regular expressions, then I combine the comments with the metadata and create an animated graph that shows what both audiences are talking about every two mins as they watch the hockey game unfold.

# Packages

You'll need to have python installed. I [followed this article](https://docs.python-guide.org/starting/install3/win/) on how to install it on Windows 10 using `choco`.

```r
library(tidyverse) # for general data manipulation in R
library(reticulate) # to convert from Python
library(rtweet) # to get tweets
library(lubridate) # for date manipulation
library(tidytext) # for tokenizing text
library(gganimate) # for animation
library(rvest) # for scraping websites
library(png) # for image manipulation
library(grid) # for custom plot manipulation
library(extrafont) # for nice fonts
library(ggtext) # for adding colour in plot titles
theme_set(theme_light(base_family = "Montserrat ExtraBold"))
# py_install("pandas") # we want pandas
# py_install("praw") # and praw
```

# Inputs

```r
# when was the game played?
game_date <- "2020-03-10"

# What should the leafs tweets contain?
tweet_keywords <- "#TMLTalk OR #LeafsForever OR @MapleLeafs OR leafs OR #leafs OR #GoLeafsGo OR #mapleleafs"

# what is the url to the reddit game thread?
reddit_url <- r_to_py("https://www.reddit.com/r/leafs/comments/fgl1tq/game_thread_tampa_bay_lightning_43206_at_toronto/")
```

# Gather metadata about the game

First, I scraped the Leafs upcoming schedule back in November 2019 off [CBS sports](https://www.cbssports.com/nhl/teams/TOR/toronto-maple-leafs/schedule/regular/). The website has two tables, one of past games and one of upcoming games. 

Those tables contain different information, like game start time for upcoming games and game result in past games, So once a game is played, it replaces game start time for game result. So I join the post-game data into the pre-game data I stored for the complete gane metadata I can use throughout this workbook.

```r
page_url <- "https://www.cbssports.com/nhl/teams/TOR/toronto-maple-leafs/schedule/regular/"

# check if it's legal to scrape
robotstxt::paths_allowed(page_url)

# scrape tables off the page
event_info <- read_html(page_url) %>%
  html_table()

# clean up the post-game table
completed_game_schedule <- event_info[[1]] %>% # this is the post-game table, [[2]] is upcoming games
  janitor::clean_names() %>%
  mutate(date_label = date,
         date = mdy(date),
         opponent = str_trim(str_remove(opp, ".*\\\n"))) %>%
  select(date, result, record)

# get the base schedule stored in the repo that has the pre-game data saved to join
team_schedule <- read_csv(str_glue("{here::here()}/team_schedules/leafs_game_schedule.csv"))

# join results table into the original leafs schedule and determine win and game location
team_schedule_results <- team_schedule %>%
  select(date, game_start, opponent = opp, home_game) %>%
  left_join(completed_game_schedule, by = "date") %>%
  mutate(location_marker = ifelse(home_game == FALSE, "@", "vs."),
         win = ifelse(str_detect(result, "^W"), TRUE, FALSE))

# filter for the game of interest using input at top
game_data <- team_schedule_results %>%
  filter(date == game_date)
```

# Get our Data from Reddit

## Establish connection to Reddit

You'll also need to visit the developer reddit page and create an app to get details for the token below.

[https://www.reddit.com/prefs/apps](https://www.reddit.com/prefs/apps)

```python
import pandas as pd # to create panda data frame
import praw # to connect to Reddit app

# replace with your own info
reddit = praw.Reddit(client_id='xxxxxxxxxxxxx', 
  client_secret='xxxxxxxxxxxxxx-xxxxxxx',  # replace with your own
  user_agent='datajake_app')
```

## Scrape reddit game thread

Using `{reticulate}`, I can use `r_to_py()` in my R chunks so regular R objects can be converted and fetched in Python chunks using `r.` so when I call `r.reddit_url` it has the string we defined in the R chunk `# Inputs`.

This will take a few minutes.

```python
# grab the thread that you put in the url arguement in the Inputs section
submission = reddit.submission(url=r.reddit_url)

# create an empty dictionary to insert our for-loop results in
topics_dict = {"body":[], "created":[]}

# request the body (text) and created (time of post) elements from our submission (the url we provided above)
submission.comments.replace_more(limit=None)
for comment in submission.comments.list():
  topics_dict["body"].append(comment.body)
  topics_dict["created"].append(comment.created)

# convert the dictionary into a data frame
topics_data = pd.DataFrame(topics_dict)
```

Inversely, I can use `{reticulate}` to call `py$` and examine a python object in an R chunk. When I call `py$topics_data` it is converted from a pandas dataframe into an R dataframe.

```r
tml_full_reddit_set <- py$topics_data %>%
  mutate(created = ymd_hms(as_datetime(created)) - hours(12)) %>% # adjust the timezone to EST
  rename(text = body,
         created_at = created) # make the date & text variable consistent on reddit with twitter

# make friendly formats to save my files
opponent_file_format <- str_replace_all(tolower(game_data$opponent), " ", "-")

# save the comments under the opponent's name and date of the game
write_csv(tml_full_reddit_set, str_glue("reddit_posts/{opponent_file_format}-{game_date}.csv"))
```

# Get our data from Twitter

You'll need to apply for a Twitter API Token. More details at: [https://developer.twitter.com/](https://developer.twitter.com/).

Please consult [rtweet's vignette](https://rtweet.info/index.html) for more info on setting up the token.

```r
# substitute your own information below
twitter_token <- create_token(app = "datacriticsScraper", 
                              consumer_key = "xxxxxxxxxxxxxxxxxxxxxxx", 
                              consumer_secret = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",  
                              set_renv = FALSE)
```

## Get Tweets that use our keywords

Scrape all tweets from that day that include the keywords we specified above. These do not include retweets so popular tweets do not heavily influence the TF-IDF formula.

```r
# if you've already scraped the data, you won't need to do it again if you run all chunks
if(file.exists(str_glue("{here::here()}/tweets/tml_talk-{opponent_file_format}-toronto-{game_date}.csv")) == FALSE) {

since <- as.Date(game_date) #get the data since today and
until <- since + 2 # until yesterday 

# save tweets locally
search_tweets(tweet_keywords,
              since = since,
              until = until,
              include_rts = FALSE,
              retryonratelimit = TRUE,
              token = twitter_token,
              type = "recent") %>%
  mutate_if(is.list, as.character) %>%
  write_csv(
    here::here(str_glue("tweets/tml_talk-{opponent_file_format}-toronto-{game_date}.csv")) # save
  )

}

# read the saved data
tml_talk <- read_csv(
  here::here(str_glue("tweets/tml_talk-{opponent_file_format}-toronto-{game_date}.csv"))
)
```

## Add influencers

Sometimes general keywords miss valuable conversations. 

I chose some people who commonly tweet about the Leafs during games to add into the data.

```r
tmls <- get_timelines(c("domluszczyszyn", "Steve_Dangle",
                        "IanGraph", "JeffVeillette",
                        "DownGoesBrown", "3rdPeriodSuits",
                        "ThatsCappy", "duarteelauraa",
                        "rahef_issa", "_marlanderthews", 
                        "LeafFan1917", "TheLeafsIMO", 
                        "TheOakLeafs", "TheFlintor", 
                        "MarkUkLeaf", "LeafsMaz20",
                        "karlandtheleafs", "Buds_All_Day",
                        "mirtle", "jonassiegel",
                        "kristen_shilton", "reporterchris",
                        "51Leafs", "ATFulemin",
                        "draglikepull", "TLNdc",
                        "TicTacTOmar", "PPPLeafs",
                        "MatthewsIsALeaf", "LeafsAllDayy",
                        "NickDeSouza_", #"SteveBurtch",
                        "thejustinfisher", "HardevLad",
                        "RyanDHobart", "jpolly22",
                        "jxcquelineoh", "jakebeleafs",
                        "account4hockey", "briancrd", 
                        "team_keefe", "Dylan_Morrow",
                        "LeafsFansUnited", "aigemac",
                        "mostlyleafies", "Its_Mr_Clutch"), 
                      n = 100,
                      token = twitter_token) %>% # 100 tweets each
  select(created_at, text)
```

## Combine the tweets

```r
# join the timelines of popular leaf accounts into the "leafs" tweet corpus
tml_full_tweet_set <- tml_talk %>%
  full_join(tmls) %>%
  select(created_at, text) %>%
  group_by(created_at, text) %>%
  filter(row_number() == 1) %>% # remove duplicate entries found in both datasets
  ungroup() %>%
  arrange(created_at)
```

# Processing the data from both sources

## Step 1: Cut data into two minute intervals

Reduce the data into the time frame we are interested in. 

We will count the words in the tweets and apply tf-idf every two minute interval.

```r
# get puck drop time from our schedule data
puck_drop <-  ymd_hms(game_data$game_start, tz = "America/New_York")

min_hour <- puck_drop - 900 # start chart 15 mins before scheduled game start
max_hour <- puck_drop + 10800 # end 15 mins after typical game end (this may need to be adjusted)

# cut our data into two-minute intervals
tml_reddit_intervals <- tml_full_reddit_set %>%
    mutate(created_at = ymd_hms(created_at, tz = "America/New_York")) %>%
  filter(created_at > min_hour,
         created_at < max_hour) %>%
  mutate(interval = round_date(created_at, "2 mins")) # round into 2 minute intervals

tml_tweet_intervals <- tml_full_tweet_set %>%
  mutate(created_at = ymd_hms(created_at)) %>%
  filter(created_at > min_hour,
         created_at < max_hour) %>%
  mutate(interval = with_tz(round_date(created_at, "2 mins"), 
                            tzone = "America/New_York"))
```

## Step 2: Define unwanted words

These are specifically unspecific words and keywords I use in my twitter query. I add more to these lists if they are boring words.

```r
# Some unwanted_words and the search terms used
twitter_unwanted_words <- c("10u", "t.co", "gotta", "#leafsforever", "games", "leafs", "#tmltalk", 
                            "hockey", "dont", "amp", "period", "region", "https", "10a", "pas", "att", 
                            "gonna", "ive", "les", "game", "hes", "#leafs", "#goleafsgo", "leaf's", "vai", 
                            "ml\U0001f4b5", "lieut", "maple", "vous", "weve", "ill", "theyre", "isnt", 
                            "youre", "o55", "bla", "guys", "row", "usa", "och", "temporada", "07mar",  
                            "playing", "play", "plays", "taking", "happen", "people", "leafs", "teams", 
                            "team", "people", "game", "gonna", "looked", "stream", "literally", "arent", 
                            "played", "10mar", "bay", "cuz", "sur", "didnt")

reddit_unwanted_words <- c("leafs", "teams", "team", "people", "game", "playing", "play", "plays", "taking", 
                           "happen", "people", "gonna", "looked", "stream", "literally", "arent", "played", "guys", 
                           "period", "dun")
```

# Functions for processing tweets & reddit comments

We do the same processing to both data sets. So we should make some functions. Details are commented in the code.

```r
# tokenize the data into intervals
create_two_min_tokens <- function(interval_df, unwanted_words) {
  interval_df %>%
    unnest_tokens(word, text) %>% # this breaks them into single words
    mutate(word = str_replace_all(word, "[,_;\\.?!]", " "),
           word = str_replace_all(word, ":$", " "),
           word = str_replace_all(word, "\\\n", " "),
           word = str_remove_all(word, '[”"“]'),
           word = str_remove_all(word, "’$"),
           word = str_remove_all(word, "’s$"),
           word = str_remove_all(word, "’t$"), # lots of random cleaning from bad apostrophes
           word = str_remove_all(word, "’m$"),
           word = str_remove_all(word, "'$"),
           word = str_trim(word)) %>%
    filter(!word %in% unwanted_words, # drop unwanted words 
           nchar(word) > 2) %>% # words must be longer than 2 characters
    anti_join(stop_words, by = "word") %>% # removes noisy words
    count(word, interval) # count words per interval
}

# supply the interval dataframe and the unwanted word vector
reddit_two_min_tokens <- create_two_min_tokens(interval_df = tml_reddit_intervals, 
                                               unwanted_words = reddit_unwanted_words)

twitter_two_min_tokens <- create_two_min_tokens(interval_df = tml_tweet_intervals, 
                                                unwanted_words = twitter_unwanted_words)
```

```r
# apply tf-idf to the data prepared by the above function
find_important_words <- function(tokenized_df){
  tokenized_df %>%
    bind_tf_idf(word, interval, n) %>%
    filter(n >= 3) %>% # word must appear 3 times in the interval to qualify
    arrange(interval, desc(tf_idf)) %>% # arrange by tf_idf
    distinct(interval, .keep_all = T) %>% # get the top result
    arrange(interval)
}

# use the results above to bind tf-idf and get the top results
tml_top_words_reddit <- find_important_words(reddit_two_min_tokens)

tml_top_words_tweets <- find_important_words(twitter_two_min_tokens)
```

```r
# find the interval volume
calculate_interval_volume <- function(interval_df) {
  interval_df %>% 
    group_by(interval) %>%
    summarize(thread_volume = n()) %>%
    ungroup()
}

# use the interval dataframe again to count the volume in each interval
tml_reddit_volume <- calculate_interval_volume(tml_reddit_intervals)

tml_tweet_volume <- calculate_interval_volume(tml_tweet_intervals)
```

# Final data

## Combine Reddit

```r
full_reddit_data <- tml_reddit_volume  %>%
  full_join(tml_top_words_reddit, by = "interval") %>% # full join because tf-idf can fail to find a word and we want to keep NA
  fill(word) %>% # forward fill words if it fails tf-idf minimum condition
  mutate(word = replace_na(word, ""),
         word = ifelse(thread_volume < 5, "", word))  # if there isn't an adequate volume, print nothing
```

## Combine Twitter

```r
full_tweet_data <- tml_tweet_volume  %>%
  inner_join(tml_top_words_tweets, by = "interval") 
```

# Extract goals from @MapleLeafs' timeline 

Ok, now we have our two sources of comments, let's focus on getting our goal events and game start/end timestamps for the chart.

## Scrape the Leafs timeline

First off, the Leafs' social media manager tweets in a similar fashion game-to-game.

That means We can reliably regex the approximate game start, game end, and goal announcements in real time instead of using game time.

```r
# get the recent 100 tweets by the leafs official account
leafs_timeline <- get_timeline("@MapleLeafs", n = 100, token = twitter_token) %>%
  mutate_if(is.list, as.character) %>%
  mutate(created_at = with_tz(ymd_hms(created_at), tzone = "America/New_York")) %>%
  filter(created_at >= game_data$date)
```

## Find Goals

MapleLeafs always tweet "GOAL" when they score.

They very often mention the city/team name of an opponent when they score. 

But not always, they commonly just say  "Empty net goal" if one is scored.

```r
# time of leafs goals
leafs_goals <- leafs_timeline %>%
  filter(str_detect(text, "GOAL")) # detect tweets that contain the word GOAL

# get the full opponent's name
opponent <- game_data$opponent

# regex the opponent's city name (ex. Calgary scores) and team name (ex. Flames score) to help search for their goal announcements
opponent_city <- str_extract(str_glue("{opponent}"), "^[a-zA-Z]+") # get first word of opponent
opponent_name <- str_extract(str_glue("{opponent}"), "[a-zA-Z]+$") # get last word iof opponent

# find those tweets
opponent_goals_text <- leafs_timeline %>%
  select(created_at, text) %>%
  filter(str_detect(text, opponent_city) | str_detect(text, opponent_name) | str_detect(text, "Empty net goal"), # tweet contains one of these phrases
         str_detect(text, "score") | str_detect(text, "Empty net goal")) # and either of these phrases
```

You'll need to manually add the goals the regex misses. This can happen with teams like LA or Habs. I recommend you explore the leafs_timeline object and remove the filter statement above and hardcode something like `filter(status_id == "1236126442288709633")`.

## Game start and end

The Leafs' social media manager also usually announces the game start in their tweet with a set of phrases. 

If we win, they always say "LEAFS WIN". Otherwise, there's a mixute of other phrases when we lose. 

We will build some regex to find these most times. Worst case we need to search for the correct posts and hardcode.

```r
# Find the game start tweet using these keywords
game_start_tweet <- leafs_timeline %>%
  filter(str_detect(text, "[Aa]ction") | str_detect(text, "[Uu]nder way") | str_detect(text, "[Tt]une in") | str_detect(text, "(go time)")) %>%
  arrange(created_at) %>%
  head(1) %>%
  select(created_at, text)

# They always say "LEAFS WIN" if they win, and a mixute of other phrases when we lose. Adjust when needed.
if (game_data$win == TRUE) {
  end_of_game <- leafs_timeline %>%
    filter(str_detect(text, "LEAFS WIN")) %>%
    select(created_at, text) 
} else {
  end_of_game <- leafs_timeline %>%
    filter(str_detect(text, "[Tt]ough") | str_detect(text, "[Bb]attle") | str_detect(text, "[Ff]inal")) %>%
    select(created_at, text) %>%
    head(1)
}
```

# Make the chart

## Fetch the background logos

I add transparent logos to the background image I scraped all them from Wikipedia profile images using the code in the setup_notebook repo.

```r
# leafs colour
leafs_blue <- "#00205B"

# read the leafs logo
l <- readPNG(str_glue("{here::here()}/team_images/220px-Toronto_Maple_leafs_2016_logo.svg.png"))

# I make them transparent by multiplying the RGB values by 0.2.
f <- matrix(rgb(l[,,1],l[,,2],l[,,3], l[,,4] * 0.4), nrow=dim(l)[1]) # This makes it transparent - 0.2 is alpha
leafs_logo <- rasterGrob(f, interpolate=TRUE)
```

```r
# lookup all team image files in the repo
files <- file.info(list.files(str_glue("{here::here()}/team_images"), 
                              full.names = TRUE))

image_paths <- data.frame(path = rownames(files))

# look for the opponent's image 
opponent_image <- image_paths %>%
  mutate_if(is.factor, as.character) %>%
  filter(str_detect(path, str_replace_all(game_data$opponent, " ", "_")))

# opponent logo
m <- readPNG(opponent_image$path)

# make transparent
w <- matrix(rgb(m[,,1],m[,,2],m[,,3], m[,,4] * 0.4), nrow=dim(m)[1]) 
opponent_logo <- rasterGrob(w, interpolate=TRUE)
```

## Lookup opponent's colour (deprecated)

The Leafs are always blue. I use [teamcolorcodes.com](teamcolorcodes.com) to lookup the opponent's colour. But if it's Blue or Navy, use the secondary colour. This can break at times (ex. Tampa Bay) and I'll just sub another colour in.

```r
# build url
team_url <- paste0("https://teamcolorcodes.com/",  tolower(opponent_file_format), "-color-codes/")

# check if legal to scrape
robotstxt::paths_allowed(team_url)

# get text from the website
page_text <- read_html(team_url) %>%
  html_nodes("body") %>%
  html_nodes("div") %>%
  html_text() 

# find the hexcodes, but don't use blue ones
if (str_detect(page_text[9], "BLUE") | str_detect(page_text[9], "Navy")) {
  # if the first colour is blue, use the alternative hex-code #000000
  opponent_colour <- str_extract(page_text[10], "#[a-zA-Z0-9]+")
} else {
  # if not blue, use the first hex-code #000000
  opponent_colour <- str_extract(page_text[9], "#[a-zA-Z0-9]+")
}
```

## Make clean x-axis breaks

```r
# x-axis time stamps
hourly_ranges <- data.frame(time = seq(min_hour + 900, max_hour + 1800, by = 3600)) # every hour

# convert into human readable labels
time_breaks <- hourly_ranges %>%
  mutate(time = format(strptime(hourly_ranges$time, "%F %H:%M:%S"), format = "%I:%M %p"),
         time = str_remove(time, "^0"),
         time = str_remove(time, " "),
         time = toupper(time))
```

## Base plot

```r
base_plot <-full_reddit_data %>%
  ggplot(aes(x = interval, y = thread_volume)) +
  
  # add logos (in the background)
  annotation_custom(leafs_logo, 
                    xmin = game_start_tweet$created_at + 4250,  # leafs to the left
                    xmax = game_start_tweet$created_at + 100, 
                    ymin=-Inf, ymax=Inf) +
  annotation_custom(opponent_logo,
                    xmin = end_of_game$created_at - 4250, # opponent to the right
                    xmax = end_of_game$created_at - 100,
                    ymin=-Inf, ymax=Inf) +
  
  # add our metadata events as lines like start time and goals
  geom_vline(xintercept = game_start_tweet$created_at, size = 1) +
  geom_vline(xintercept = end_of_game$created_at, size = 1) +
  geom_vline(xintercept = leafs_goals$created_at,
             linetype = 5,
             size = 0.85,
             colour = leafs_blue) +
  geom_vline(xintercept = opponent_goals_text$created_at,
             linetype = 2,
             size = 0.85,
             colour = opponent_colour) +
  
  # draw the tweet volume lines
  geom_line(data = full_tweet_data, aes(y = thread_volume), color = "lightblue", size = 2) +
  geom_line(color = "#FF7B4D", size = 2) +
  
  # draw the words next
  geom_text(data = full_tweet_data, aes(y = thread_volume, label = word), 
            size = 10, colour = "black", family = "Inter SemiBold", vjust = -0.5) + # twitter
  geom_text(aes(label = word), 
            size = 12, colour = "black", family = "Oswald", vjust = 0.5) + # reddit
  
  # styling
  scale_x_datetime(breaks = hourly_ranges$time, 
                   labels = time_breaks$time) +
  
  scale_y_log10() +
  coord_cartesian(clip = "off") +
  labs(title = str_glue("<b style='color:{leafs_blue};font-size:22px'>MAPLE LEAFS</b> <br> <b style='font-size:27px'>CHATTER CHARTS"),
       subtitle = str_glue("<b style='color:#FF4301'>— Reddit Game Thread</b> <b style='color:#1DA1F2'> — Twitter Tweets</b>"),
       x= "", y = "Chat Volume Every 2 Mins (log scale)") +
  theme(text = element_text(lineheight = 1),
        panel.grid = element_blank(),
        panel.border = element_rect(fill = NA, color = grey(0.2), size = 1),
        axis.title.y = element_markdown(size = 8),
        plot.title = element_markdown(size = 12, face = "bold",
                                      lineheight = 1, vjust = 1, family = "Montserrat ExtraBold"),
        plot.subtitle = element_markdown(size = 10, family = "Inter ExtraBold", vjust = -20),
        plot.caption = element_text(size = 6, family = "Inter ExtraBold"),
        plot.margin = margin(0.5, 1, 0.5, 1, "cm")) +
  expand_limits(y = 1000)
```


```r
# Add animation
animated_plot <- base_plot +
  transition_reveal(interval) + # follow the interval
  ease_aes("linear") # fixed speed
```

# Save animation

This takes about 20 minutes. It'll beep for you.

```r
options(gganimate.dev_args = list(height = 4, width = 4*1.8, units = 'in', type = "cairo", res = 144))

animate(plot = animated_plot,
        fps = 20, duration = 75,
        type = "cairo",
        renderer = av_renderer(str_glue("animations/twitter_reddit-leafs-{opponent_file_format}-{game_data$date}.mp4")))

beepr::beep()
```
