# Animating Twitter Chatter throughout the course of a Leafs Game

[![Example](animations/leafs-st.-louis-blues-2019-12-07.gif)](https://www.reddit.com/r/leafs/comments/e80uu4/twitter_chatter_leafs_vs_blues_december_07_2019/)

I post these animations to [r/Leafs](https://www.reddit.com/r/leafs/comments/e80uu4/twitter_chatter_leafs_vs_blues_december_07_2019/)

## Details

This project gathers tweets with `rtweet` that contains #TMLTalk, #LeafsForever, Leafs, leaf's, #leafs, or #goleafsgo.

I use `lubridate` to round the tweet's timestamp into 2 minute intervals.

Then I use `tidytext` to tokenize all the tweets into individual words then apply [TF-IDF](https://en.wikipedia.org/wiki/Tf%E2%80%93idf) to find the most important word at each interval.

I calculate the twitter volume per interval and bind the most important word to this with basic `dplyr`.

I then use `rvest` to scrape websites for a lot of miscellenous information. The team logos were scraped off Wikipedia, and the Leafs schedule was scraped off CBS Sports.

I applied transparency to the images with `png` and added colour to the plot title using `ggtext`. 

Finally, `gganimate` brings it to life with `transition_reveal()`

```
# chart
base_plot <- five_min_volume  %>%
  inner_join(tml_five_min_top_words, by = "interval") %>%
  ggplot(aes(x = interval, y = tweet_volume)) +
  geom_line(color = "lightblue", size = 2) +
  geom_text(aes(label = word), size = 14, colour = leafs_blue) +
  geom_vline(xintercept = game_start_tweet$created_at, size = 1.5) +
  geom_vline(xintercept = end_of_game$created_at, size = 1.5) +
  geom_vline(xintercept = leafs_goals$created_at,
             linetype = 5,
             size = 0.75,
             colour = leafs_blue) +
  geom_vline(xintercept = opponent_goals_text$created_at,
             linetype = 5,
             size = 0.75,
             colour = opp_colour) +
  scale_x_datetime(breaks = seq(min_hour, max_hour, by = 3600), 
                   labels = time_breaks$time) +
  scale_y_log10() +
  labs(title = str_glue("Toronto Maple Leafs {game_data$location_marker} <b style='color:{opp_colour}'>{opponent}</b><br>Final: {game_data$result}"),
       x= "", y = "Tweet Volume - log") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_markdown(size = 20, face = "bold", colour = leafs_blue,
                                      lineheight = 1, vjust = 1)) +
  annotation_custom(leafs_logo, 
                    xmin = game_start_tweet$created_at + 4250, 
                    xmax = game_start_tweet$created_at + 100, 
                    ymin=-Inf, ymax=Inf) +
  annotation_custom(opponent_logo, 
                    xmin = end_of_game$created_at - 4250, 
                    xmax = end_of_game$created_at - 100, 
                    ymin=-Inf, ymax=Inf) 

# Add animation
animated_plot <- base_plot +
  transition_reveal(interval) +
  ease_aes('linear')
```

## Requirements

You'll need to apply for a Twitter API Token to get tweets in the first place. More details at: https://developer.twitter.com/. 

Please consult [rtweet's vignette](https://rtweet.info/index.html) for more info on setting up the token.

```
library(tidyverse) # for the basics
library(rtweet) # for collecting tweets
library(lubridate) # for date manipulation
library(tidytext) # for tokenizing tweets
library(gganimate) # for animation and gifs
library(rvest) # for scraping data from websites
library(png) # for image manipulation
library(grid) # for custom plot manipulation
library(ggtext) # for colouring the plot title
library(extrafont) # for nicer fonts
```

## See more 

See more animations in the /animations folder.
