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

reddit_png <- readPNG(here::here("social_images/reddit_brand.png"))

reddit_brand <- rasterGrob(reddit_png, interpolate = TRUE, 
                           width=unit(4,'cm'),
                           x = unit(1,"npc"), y = unit(1,"npc"),
                           hjust = 0.9, vjust= 0)
## lookup all team image files in the repo
files <- file.info(list.files(str_glue("{here::here()}/team_images"), 
                              full.names = TRUE))

image_paths <- data.frame(path = rownames(files))

## HOME TEAM
main_image <- image_paths %>%
  mutate_if(is.factor, as.character) %>%
  filter(str_detect(path, str_replace_all(main_team, " ", "_")))

m <- readPNG(main_image$path)
w <- matrix(rgb(m[,,1],m[,,2],m[,,3], m[,,4] * 1), nrow = dim(m)[1]) 
main_logo <- rasterGrob(w, interpolate = TRUE)

## OPPONENT
opponent_image <- image_paths %>%
  mutate_if(is.factor, as.character) %>%
  filter(str_detect(path, str_replace_all(opponent, " ", "_")))

m <- readPNG(opponent_image$path)
w <- matrix(rgb(m[,,1],m[,,2],m[,,3], m[,,4] * 1), nrow = dim(m)[1]) 
opponent_logo <- rasterGrob(w, interpolate = TRUE)

### SCORING
### CLEANING
scoring_df <- main_team_scoring  %>%
  bind_rows(opponent_scoring, .id = "team") %>%
  mutate(team = ifelse(team == 1, "main_team", "opponent")) %>%
  mutate(linetype = ifelse(score_type == "touchdown", 1, 1),
         colour = ifelse(team == "main_team", main_colour, opponent_colour),
         points = ifelse(score_type == "touchdown", 7, 3),
         size = ifelse(score_type == "touchdown", 2.5, 0.75)) %>%
  group_by(team) %>%
  arrange(created_at) %>%
  mutate(score = cumsum(points)) %>%
  spread(team, score) %>%
  fill(main_team, opponent)

scoring_df[, 8:9][is.na(scoring_df[, 8:9])] <- 0

score_sequence <- scoring_df %>%
  mutate(main_team = ifelse(nchar(main_team) == 1, paste0("0", main_team), main_team),
         opponent =  ifelse(nchar(opponent) == 1, paste0("0", opponent), opponent)) %>%
  mutate(running_score = str_glue("{main_team} - {opponent}"))

main_scoring_df <- score_sequence %>%
  filter(colour == main_team_data$colour_primary)

opponent_scoring_df <- score_sequence %>%
  filter(colour == opponent_team_data$colour_primary)

### HIGHLIGHT WHEN GOALS ARE SCORED
main_game_score_df <- main_scoring_df %>%
  rename(interval = created_at) %>%
  mutate(running_score = as.character(running_score)) %>%
  bind_rows(data.frame(interval = main_scoring_df$created_at + minutes(2), # iser
                       running_score = as.character(main_scoring_df$running_score))) %>%
  mutate(main_board_size = 14) %>%
  full_join(full_data, by = "interval") %>%
  arrange(interval) %>%
  mutate(main_board_size = ifelse(is.na(running_score), 12, main_board_size)) %>%
  arrange(interval) %>%
  fill(running_score, .direction = "down") %>%
  mutate(running_score = ifelse(is.na(running_score), "00 - 00", running_score)) %>%
  mutate(main_board_size = ifelse(running_score == "00 - 00", 12, main_board_size)) %>%
  select(interval, running_score, main_board_size) 

opponent_game_score_df <- opponent_scoring_df %>%
  rename(interval = created_at) %>%
  mutate(running_score = as.character(running_score)) %>%
  bind_rows(data.frame(interval = opponent_scoring_df$created_at + minutes(2), # iser
                       running_score = as.character(opponent_scoring_df$running_score))) %>%
  mutate(opponent_board_size = 14) %>%
  full_join(full_data, by = "interval") %>%
  arrange(interval) %>%
  mutate(opponent_board_size = ifelse(is.na(running_score), 12, opponent_board_size)) %>%
  arrange(interval) %>%
  fill(running_score, .direction = "down") %>%
  mutate(running_score = ifelse(is.na(running_score), "00 - 00", running_score)) %>%
  mutate(opponent_board_size = ifelse(running_score == "00 - 00", 12, opponent_board_size)) %>%
  select(interval, running_score, opponent_board_size) 

main_score_df <- main_game_score_df %>%
  mutate(main_score = str_extract(running_score, "^[0-9]+")) %>%
  mutate(main_board_size = ifelse(interval %in% round_date(main_scoring_df$created_at + minutes(1), "2 minutes"), 14, main_board_size)) %>%
  select(interval, main_score, main_board_size)

opponent_score_df <- opponent_game_score_df %>%
  mutate(opponent_score = str_extract(running_score, "[0-9]+$")) %>%
  mutate(opponent_board_size = ifelse(interval %in% round_date(opponent_scoring_df$created_at + minutes(1), "2 minutes"), 14, opponent_board_size)) %>%
  select(interval, opponent_score, opponent_board_size)

final_score_text <- main_game_score_df %>%
  mutate(text = ifelse(interval >= game_end_tweet$created_at, "FINAL", ""))

team_scoring_markers <- scoring_df %>%
  select(created_at, score_type, colour) %>%
  mutate(team_acronym = ifelse(colour == main_team_data$colour_primary,  main_team_data$team_acronym, opponent_team_data$team_acronym),
         team_bg_fill =  ifelse(colour == main_team_data$colour_primary,  main_colour, opponent_colour),
         team_text_colour = ifelse(colour == main_team_data$colour_primary,  main_secondary_colour, opponent_secondary_colour),
         # team_text_colour = ifelse(colour == main_team_data$colour_primary,  "white", "white"),
         y_location = ifelse(colour == main_team_data$colour_primary,  ifelse(max(full_data$interval_volume) < 10000, 10000, max(full_data$interval_volume)), 10))

time_spacer <- as.numeric((time_diff <- (game_end_tweet$created_at - game_start_tweet$created_at) / 9) * 60 * 60)

custom_labels <-c("Game Start", "", "", "", "", "", "", "", "", "Game End")



bg_img <- readPNG(here::here("backgrounds/grass_bg_1.png"))
background_img <- rasterGrob(bg_img, interpolate = TRUE)


base_plot <- full_data %>%
  ggplot(aes(x = interval, y = interval_volume)) +
  
  ### Field
  annotation_custom(background_img, 
                    xmin = -Inf,  
                    xmax = Inf, 
                    ymin = -Inf, ymax = Inf) +
  
  ### PRE/POST GAME LABELS
  annotate("text", x = ymd_hms(game_start_tweet$created_at, tz = "America/New_York") - 1250, 
           y = 12, size = 3,  label = "Pre-Game", colour = "white", family = "Inter Medium") +
  annotate("text", x = ymd_hms(game_end_tweet$created_at, tz = "America/New_York") + 1350, 
           y = 12, size = 3, label = "Post-Game", colour = "white", family = "Inter Medium")  +
  annotate("text", x = ymd_hms(game_start_tweet$created_at, tz = "America/New_York") - 1250, 
           y = 9000, size = 3,  label = "Pre-Game", colour = "white", family = "Inter Medium") +
  annotate("text", x = ymd_hms(game_end_tweet$created_at, tz = "America/New_York") + 1350, 
           y = 9000, size = 3, label = "Post-Game", colour = "white", family = "Inter Medium") +

  
  ### GAME START/END MARKERS
  
 # geom_vline(data = data.frame(xintercept = seq(as.POSIXct(ymd_hms(game_start_tweet$created_at, tz = "America/New_York")), as.POSIXct(game_end_tweet$created_at), time_spacer)),
 #            aes(xintercept = xintercept),
 #            colour = "white", size = 0.5) +
  
  geom_vline(data = game_time_df,
            aes(xintercept = created_at),
            colour = "white") + 

  ### SCORING MARKERS
  geom_vline(data = scoring_df %>%
               mutate(size = ifelse(size == 2.5, size + 0.75, size + 0.5),
                      colour_alt = ifelse(colour == main_team_data$colour_primary, main_team_data$colour_secondary, colour),
                      colour_alt = ifelse(colour == opponent_team_data$colour_primary, opponent_team_data$colour_secondary, colour_alt)), 
             aes(xintercept = created_at,
                 linetype = I(linetype),
                 colour = I(colour_alt),
                 size = I(size))) +
  
  geom_vline(data = scoring_df, 
             aes(xintercept = created_at,
                 linetype = I(linetype),
                 colour = I(colour),
                 size = I(size))) +
  
  #geom_segment(data = scoring_df, 
  #             aes(x = created_at, y = 10, xend = created_at, yend = 10000, 
  #                 colour = I(colour), size = I(size),
  #                 linetype = I(linetype))) +
  #

  
  ### BG LOGOS
  annotation_custom(main_logo, 
                    xmin = mean(c(game_start_tweet$created_at, game_end_tweet$created_at)) - 100,  
                    xmax = game_start_tweet$created_at + 3400, 
                    ymin = -Inf, ymax = Inf) +
  
  annotation_custom(opponent_logo,
                    xmin = mean(c(game_start_tweet$created_at, game_end_tweet$created_at)) + 100, 
                    xmax = game_end_tweet$created_at - 3400,
                    ymin = -Inf, ymax = Inf) +
  
  
  ### SCOREBOARD
  geom_shadowtext(aes(x = mean(c(game_start_tweet$created_at, game_end_tweet$created_at)),
                y = ifelse(max(full_data$interval_volume) < 10000, 10000, max(full_data$interval_volume)),
                label = "-"), 
            size = 8, vjust = -1.5, colour = "white", family = "Quantico", show.legend = FALSE) +
  
  geom_text(data = final_score_text,
            aes(x = mean(c(game_start_tweet$created_at, game_end_tweet$created_at)),
                y = ifelse(max(full_data$interval_volume) < 10000, 10000, max(full_data$interval_volume)),
                label = text), 
            size = 3, vjust = -7.5, colour = "white", family = "Inter SemiBold", show.legend = FALSE) +

  geom_shadowtext(data = main_score_df,
                  aes(x = mean(c(game_start_tweet$created_at, game_end_tweet$created_at)),
                      y = ifelse(max(full_data$interval_volume) < 10000, 10000, max(full_data$interval_volume)),
                      label = main_score, 
                      size = I(main_board_size)), 
                  family = "Quantico", show.legend = FALSE,
                  #colour = main_team_data$colour_primary, bg.colour = main_team_data$colour_secondary,
                  vjust = -0.8, hjust = 1.25) +
  
  geom_shadowtext(data = opponent_score_df,
                  aes(x = mean(c(game_start_tweet$created_at, game_end_tweet$created_at)),
                      y = ifelse(max(full_data$interval_volume) < 10000, 10000, max(full_data$interval_volume)),
                      label = opponent_score, 
                      size = I(opponent_board_size)), 
                  family = "Quantico", show.legend = FALSE,
                  #colour = opponent_team_data$colour_primary, bg.colour = opponent_team_data$colour_secondary,
                  vjust = -0.8, hjust = -0.25) +

  ### LINE
  geom_line(color = "#FFB612", size = 2) +
  
  ### LINE SOCIAL ICON
  geom_image(aes(image = "emoji/american-football.png"),
             position = "identity",
             size = 0.04) +
  
  ### TEAM LABELS
  geom_label(data = team_scoring_markers %>%
               mutate(label_size = ifelse(score_type == "touchdown", 5, 5)), 
             aes(x = created_at, 
                 y = I(y_location),
                 colour = I(team_text_colour),
                 fill = I(team_bg_fill),
                 size = I(label_size),
                 label = team_acronym),
             label.padding = unit(0.15, "lines"), 
             label.r = unit(0, "lines"),
             label.size = 0.2, size = 4, family = "Oswald") +
  
  ### WORDS
  geom_shadowtext(aes(label = word), 
                  size = 11, colour = "black", family = "Inter SemiBold", vjust = -0.5, bg.colour = "white") + 
  
  ### SCALES
  scale_x_datetime(breaks = seq(as.POSIXct(ymd_hms(game_start_tweet$created_at, tz = "America/New_York")), as.POSIXct(game_end_tweet$created_at), time_spacer),
                   #limits = as.POSIXct(c(ymd_hms(official_game_start, tz = "America/New_York"), game_end_tweet$created_at)), 
                   labels = custom_labels) +
  scale_y_log10(labels = scales::comma_format()) +
  scale_alpha(range = c(0.9, 0.7)) + 
  expand_limits(y = 10:10000, x = as.POSIXct(c(ymd_hms(game_start_tweet$created_at, tz = "America/New_York") - 1800, game_end_tweet$created_at + 1800))) +
  coord_cartesian(clip = "off") +
  
  ### TITLE
  labs(title = str_glue("NFL – WEEK 1\nCHATTER CHARTS"),
       caption = str_glue("Showing the most statistically important word every two-minutes using {scales::comma(sum(full_data$interval_volume))} real-time social media comments."),
       x= "",
       y = "") +
  
  ### OTHER
  theme(text = element_text(lineheight = 1),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = NA, colour = "white", size = 2.5),
        panel.ontop = FALSE,
        plot.background = element_rect(fill = "#1b4d0d", colour = NA),
        axis.title.y = element_markdown(size = 8, colour = grey(0.2), lineheight = 1),
        axis.text = element_text(colour = "white"),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_shadowtext(size = 15, colour = "white", lineheight = 0.75,
                                      family = "Montserrat ExtraBold",
                                      margin = margin(0,0,30,0)),
        plot.caption = element_text(size = 8, family = "Inter SemiBold", hjust = 0, vjust = 8, colour = "white"),
        plot.margin = margin(0.5, 1.5, 0.1, 0.5, "cm")) 

### ANIMATE
  animated_plot <- base_plot +
  transition_reveal(interval) + # follow the interval
  ease_aes("linear") # fixed speed

animate(plot = animated_plot,
        fps = 10, duration = 5,
        height = 608, width = 1080,  units = 'px', type = "cairo", res = 144,
        renderer = av_renderer(str_glue("animations/{sport}-{main_team_file_format}-{opponent_file_format}-{game_date}.mp4")))


animate(plot = animated_plot,
        fps = 25, duration = 40,
        height = 608, width = 1080,  units = 'px', type = "cairo", res = 144,
        renderer = av_renderer(str_glue("animations/{sport}-{main_team_file_format}-{opponent_file_format}-{game_date}.mp4")))

beepr::beep(sound = 2)
