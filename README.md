# Animating Twitter Chatter throughout the course of a Leafs Game

[![Example](animations/leafs-st.-louis-blues-2019-12-07.gif)](https://www.reddit.com/r/leafs/comments/e80uu4/twitter_chatter_leafs_vs_blues_december_07_2019/)

I post these animations to [r/Leafs](https://www.reddit.com/r/leafs/comments/e80uu4/twitter_chatter_leafs_vs_blues_december_07_2019/)

## Details

This project gathers tweets with `rtweet` that contains Leafs, #TMLTalk, or #LeafsForver.

I use `lubridate` to round the tweet's timestamp to the nearest 5 minute interval.

Then I use `tidytext` to tokenize all the tweets into words and apply TF-IDF to find the most important word at each interval.

I calculate the twitter volume at each interval and bind the most important word to this with basic `dplyr`.

I then use `rvest` to scrape websites for a lot of miscellenous information. The team logos were scraped off Wikipedia, and the Leafs schedule was scraped off CBS Sports.

I applied transparency to the images with `png` and added colour to the plot title using `ggtext`. 

## Requirements

You'll need to apply for a Twitter API Token. More details at: https://developer.twitter.com/. 

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
