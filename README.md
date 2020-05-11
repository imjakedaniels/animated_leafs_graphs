
# Animating Twitter Chatter throughout the course of a Leafs Game

[![Example](https://github.com/imjakedaniels/animated_leafs_graphs/blob/master/animations/twitter_reddit-leafs-tampa-bay-lightning-2020-03-10.gif)](https://www.reddit.com/r/leafs/comments/e80uu4/twitter_chatter_leafs_vs_blues_december_07_2019/)

I post these animations to [r/Leafs](https://www.reddit.com/r/leafs/comments/e80uu4/twitter_chatter_leafs_vs_blues_december_07_2019/)

## Details

This project gathers tweets that contain #TMLTalk, #LeafsForever, @MapleLeafs, Leafs, #leafs, or #goleafsgo. using {rtweet}.
I also scrape about 40 user timelines that are active Leaf fans who don't always using these tags always to add more meaningful signal.

Then I use {lubridate} to group tweets into 2 minute intervals. I calculate tweet volume here based on tweets per 2 mins.

I use {tidytext} to tokenize all the tweets into individual words then apply [TF-IDF](https://en.wikipedia.org/wiki/Tf%E2%80%93idf) to find the most important word at each interval.

I finally bind the most important word to the twitter volume per interval.

The team logos were scraped off Wikipedia, and the Leafs schedule was scraped off CBS Sports using {rvest}.

Finally, {gganimate} brings it to life with `transition_reveal()`

```
# chart
base_plot <- two_min_volume  %>%
  inner_join(tml_two_min_top_words, by = "interval") %>%
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
