library(tidyverse) # for general data manipulation in R
library(reticulate) # to convert from Python
library(rtweet) # to get tweets
library(lubridate) # for date manipulation
library(tidytext) # for tokenizing text & tf-idf
library(gganimate) # for chart animation
library(rvest) # for scraping website html
library(png) # for image manipulation
library(grid) # for custom plot manipulation
library(extrafont) # for nice fonts
library(ggtext) # for adding html styling in plot titles
library(ggimage) # for social media icons
library(shadowtext) # for a drop shadow behind main text
theme_set(theme_light(base_family = "Montserrat ExtraBold"))
# py_install("pandas") # run this to install pandas
# py_install("praw") # and praw

## Team Logos
# lookup all team image files in the repo
files <- file.info(list.files(str_glue("{here::here()}/team_images"), 
                              full.names = TRUE))

image_paths <- data.frame(path = rownames(files))

# find main team
main_image <- image_paths %>%
  mutate_if(is.factor, as.character) %>%
  filter(str_detect(path, str_replace_all(main_team, " ", "_")))

m <- readPNG(main_image$path)
w <- matrix(rgb(m[,,1],m[,,2],m[,,3], m[,,4] * 0.6), nrow = dim(m)[1]) 
main_logo <- rasterGrob(w, interpolate = TRUE)

# find opponent image 
opponent_image <- image_paths %>%
  mutate_if(is.factor, as.character) %>%
  filter(str_detect(path, str_replace_all(opponent, " ", "_")))

m <- readPNG(opponent_image$path)
w <- matrix(rgb(m[,,1],m[,,2],m[,,3], m[,,4] * 0.6), nrow = dim(m)[1]) 
opponent_logo <- rasterGrob(w, interpolate = TRUE)

######

goal_df <- main_team_goals  %>%
  bind_rows(opponent_goals, .id = "team") %>%
  mutate(linetype = ifelse(team == 1, 2, 5),
         colour = ifelse(team == 1, main_colour, opponent_colour))

### SCORING
### CLEANING
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
  fill(main_team, opponent)

scoring_df[, 8:9][is.na(scoring_df[, 8:9])] <- 0

score_sequence <- scoring_df %>%
  mutate(running_score = str_glue("{main_team} - {opponent}"))

### HIGHLIGHT WHEN GOALS ARE SCORED
running_game_score_df <- score_sequence %>%
  rename(interval = created_at) %>%
  bind_rows(data.frame(interval = .$interval + minutes(2), # iser
                       running_score = .$running_score)) %>%
  mutate(board_size = 15) %>%
  full_join(full_data, by = "interval") %>%
  arrange(interval) %>%
  mutate(board_size = ifelse(is.na(running_score), 12, board_size)) %>%
  mutate(board_size = ifelse(interval %in% round_date(scoring_df$created_at + minutes(1), "2 minutes"), 15, board_size)) %>%
  arrange(interval) %>%
  fill(running_score, .direction = "down") %>%
  mutate(running_score = ifelse(is.na(running_score), "0 - 0", running_score)) %>%
  mutate(board_size = ifelse(running_score == "0 - 0", 12, board_size)) %>%
  select(interval, running_score, board_size)

final_score_text <- running_game_score_df %>%
  mutate(text = ifelse(interval >= game_end_tweet$created_at, "Final", ""))

team_scoring_markers <- scoring_df %>%
  select(created_at, colour) %>%
  mutate(team_acronym = ifelse(colour == main_team_data$colour_primary,  main_team_data$team_acronym, opponent_team_data$team_acronym),
         team_bg_fill =  ifelse(colour == main_team_data$colour_primary,  main_team_data$colour_primary, opponent_team_data$colour_primary),
         team_text_colour = ifelse(colour == main_team_data$colour_primary,  "white", "white"),
         y_location = ifelse(colour == main_team_data$colour_primary,  1000, 3)) %>%
  rename(interval = created_at) %>%
  inner_join(running_game_score_df, by = "interval") %>%
  rename(created_at = interval)

## Sponsor Banner (future)
reddit_png <- readPNG(here::here("social_images/reddit_brand.png"))

reddit_brand <- rasterGrob(reddit_png, interpolate = TRUE, 
                           width=unit(4,'cm'),
                           x = unit(1,"npc"), y = unit(1,"npc"),
                           hjust = 0.9, vjust= 0)

m <-linesGrob(y = c(0, 1.5),x = c(-.015, .015),  gp = gpar(col = I(main_team_data$colour_primary), lwd = 2.5)) 
b <-linesGrob(y = c(0, 1.5),x = c(-.015, .015),  gp = gpar(col = I(opponent_team_data$colour_primary), lwd = 2.5)) 

base_plot <- full_data %>%
  ggplot(aes(x = interval, y = thread_volume)) +
  ### BG LOGOS
  annotation_custom(main_logo, 
                    xmin = mean(c(game_start_tweet$created_at, game_end_tweet$created_at)) - 100,  
                    xmax = game_start_tweet$created_at + 100, 
                    ymin = -Inf, ymax = Inf) +
  annotation_custom(opponent_logo,
                    xmin = mean(c(game_start_tweet$created_at, game_end_tweet$created_at)) + 100, 
                    xmax = game_end_tweet$created_at - 100,
                    ymin = -Inf, ymax = Inf) +
  
  #annotation_custom(m, xmin = mean(c(game_start_tweet$created_at, game_end_tweet$created_at)) - minutes(15),
  #                  xmax = mean(c(game_start_tweet$created_at, game_end_tweet$created_at)),
  #                  ymin = 1000,
  #                  ymax = Inf) +
  #
  #annotation_custom(b, 
  #                  xmin = mean(c(game_start_tweet$created_at, game_end_tweet$created_at)) + minutes(15),
  #                  xmax = mean(c(game_start_tweet$created_at, game_end_tweet$created_at)) + minutes(15), 
  #                  ymin = 1000,
  #                  ymax = Inf) +

  ### GAME DURATION MARKERS
  geom_vline(data = game_time_df, 
             aes(xintercept = created_at)) +
  
  ### GOALS
  geom_vline(data = scoring_df, 
             aes(xintercept = created_at,
                 colour = I(colour)),
             linetype = 2,
             size = 0.75) +
  
  ### TEAM LABELS
  geom_label(data = team_scoring_markers, 
             aes(x = created_at, 
                 y = I(y_location),
                 colour = I(team_text_colour),
                 fill = I(team_bg_fill),
                 label = team_acronym),
             label.padding = unit(0.15, "lines"), 
             label.r = unit(0.12, "lines"),
             label.size = 0.1, size = 3, family = "Oswald", ) +
  
   geom_text(data = final_score_text,
          aes(x = mean(c(game_start_tweet$created_at, game_end_tweet$created_at)),
               y = 1000,
               label = text), 
           size = 3.5, vjust = -5, colour = "black", family = "Quantico", show.legend = FALSE) +
  
  ### SCOREBOARD
  geom_shadowtext(data = running_game_score_df,
            aes(x = mean(c(game_start_tweet$created_at, game_end_tweet$created_at)),
                y = 1000,
                label = running_score, 
                size = I(board_size)), 
            colour = "white", family = "Quantico", vjust = -0.7, show.legend = FALSE) +


  ### LINE
  geom_line(color = main_colour, size = 2) +
  
  ### LINE SOCIAL ICON
  geom_image(aes(image = "emoji/hockey_puck.png")) +
  
  ### WORDS
  geom_shadowtext(aes(label = word), 
                  size = 10, colour = "black", family = "Inter SemiBold", vjust = -0.5, bg.colour = "white") + # reddit
  
  ### SCALES
  scale_x_datetime(breaks = scales::breaks_width(width = "1 hour"),
                   labels = scales::label_date(format = "%I:%M %p", tz = "America/New_York")) +
  scale_y_log10() +
  expand_limits(y = 10:1000, x = c(game_start_tweet$created_at - 1800, game_end_tweet$created_at + 1800)) +
  coord_cartesian(clip = "off") +
  
  ### TITLE 
  labs(title = str_glue("<b style='color:black;font-size:20px'>NHL</b>
                         <b style='color:#b5b7bb;font-size:16px'>2019-20 Playoffs</b>
                         <br>
                         <b style='font-size:20px'>CHATTER CHARTS"),
                        
       caption = str_glue("Showing the most statistically important word every two-minutes using {scales::comma(sum(full_data$thread_volume))} real-time social media comments."),
       x= "",
       y = "") +
  
  ### OTHER
  theme(text = element_text(lineheight = 1),
        panel.grid = element_blank(),
        axis.title.y = element_markdown(size = 8, colour = grey(0.2), lineheight = 1),
        axis.ticks = element_line(colour = grey(0.2)),
        axis.text.x = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = NA, colour = grey(0.1), size = 0.75),
        panel.ontop = FALSE,
        plot.background = element_rect(fill = NA, colour = NA),
        plot.title = element_markdown(lineheight = 0.5, #linetype = 1, 
                                      family = "Montserrat ExtraBold", 
                                      padding = margin(0,10, 2, 0), margin = margin(0,0,2,0)),
        plot.subtitle = element_markdown(size = 6, family = "Inter",),
        plot.caption = element_text(size = 8, family = "Inter", hjust = 0, vjust = 8),
        plot.margin = margin(0.5, 1.5, 0.1, 0.5, "cm")) 


### ANIMATE
animated_plot <- base_plot +
  transition_reveal(interval) + # follow the interval
  ease_aes("linear") # fixed speed

# Use #HockeyTwitter
# Use #StanleyCup
# Say "follow me on Twitter/Instagram @chattercharts"

# spreadsheet for people to enter themselves into their team's vip list

animate(plot = animated_plot,
        fps = 25, duration = 35,
        height = 608, width = 1080,  units = 'px', type = "cairo", res = 144,
        renderer = av_renderer(str_glue("animations/hockey-{data_source}-{main_team_file_format}-{opponent_file_format}-{game_date}.mp4")))

beepr::beep(sound = 2)  
