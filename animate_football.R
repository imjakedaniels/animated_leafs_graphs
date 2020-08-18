reddit_png <- readPNG(here::here("social_images/reddit_brand.png"))

reddit_brand <- rasterGrob(reddit_png, interpolate = TRUE, 
                           width=unit(4,'cm'),
                           x = unit(1,"npc"), y = unit(1,"npc"),
                           hjust = 0.9, vjust= 0)

### SCORING
### CLEANING
scoring_df <- main_team_scoring  %>%
  bind_rows(opponent_scoring, .id = "team") %>%
  mutate(team = ifelse(team == 1, "main_team", "opponent")) %>%
  mutate(linetype = ifelse(score_type == "touchdown", 5, 2),
         colour = ifelse(team == "main_team", main_colour, opponent_colour),
         points = ifelse(score_type == "touchdown", 7, 3),
         size = ifelse(score_type == "touchdown", 1.5, 0.75)) %>%
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

### HIGHLIGHT WHEN GOALS ARE SCORED
running_game_score_df <- score_sequence %>%
  rename(interval = created_at) %>%
  bind_rows(data.frame(interval = .$interval + minutes(2), # iser
                       running_score = .$running_score)) %>%
  mutate(board_size = 12) %>%
  full_join(full_data, by = "interval") %>%
  arrange(interval) %>%
  mutate(board_size = ifelse(is.na(running_score), 10, board_size)) %>%
  mutate(board_size = ifelse(interval %in% round_date(scoring_df$created_at + minutes(1), "2 minutes"), 12, board_size)) %>%
  arrange(interval) %>%
  fill(running_score, .direction = "down") %>%
  mutate(running_score = ifelse(is.na(running_score), "00 - 00", running_score)) %>%
  mutate(board_size = ifelse(running_score == "00 - 00", 10, board_size)) %>%
  select(interval, running_score, board_size)

final_score_text <- running_game_score_df %>%
  mutate(text = ifelse(interval >= game_end_tweet$created_at, "Final", ""))

team_scoring_markers <- scoring_df %>%
  select(created_at, score_type, colour) %>%
  mutate(team_acronym = ifelse(colour == "#203731",  "GB", "DET"),
         team_bg_fill =  ifelse(colour == "#203731",  main_colour, opponent_colour),
         team_text_colour = ifelse(colour == "#203731",  main_secondary_colour, opponent_secondary_colour),
         y_location = ifelse(colour == "#203731",  10000, 10)) %>%
  rename(interval = created_at) %>%
  inner_join(running_game_score_df, by = "interval") %>%
  rename(created_at = interval)


time_spacer <- as.numeric((time_diff <- (game_end_tweet$created_at - game_start_tweet$created_at) / 9) * 60 * 60)

custom_labels <-c("Game Start", "", "", "", "", "", "", "", "", "Game End")

base_plot <- full_data %>%
  ggplot(aes(x = interval, y = thread_volume)) +
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
  geom_vline(data = game_time_df, 
             aes(xintercept = created_at),
             colour = "white", size = 1.25) +
  
  ### SCORING MARKERS
  geom_vline(data = scoring_df, 
             aes(xintercept = created_at,
                 linetype = I(linetype),
                 colour = I(colour),
                 size = I(size))) +
  
  ### SCOREBOARD
  geom_shadowtext(data = running_game_score_df,
                  aes(x = mean(c(game_start_tweet$created_at, game_end_tweet$created_at)),
                      y = 10000,
                      label = running_score, 
                      size = I(board_size)), 
                  family = "Quantico", show.legend = FALSE, vjust = -0.7, bg.colour = "#203731") +
  
  # geom_text(data = final_score_text,
  #        aes(x = mean(c(game_start_tweet$created_at, game_end_tweet$created_at)),
  #             y = 9000,
  #             label = text), 
  #         size = 5, alpha = 0.7, colour = "white", family = "Quantico", show.legend = FALSE) +
  
  ### LINE
  geom_line(color = "#FFB612", size = 2) +
  
  ### LINE SOCIAL ICON
  geom_image(aes(image = "emoji/american-football.png")) +
  
  ### TEAM LABELS
  geom_label(data = team_scoring_markers, 
             aes(x = created_at, 
                 y = I(y_location),
                 colour = I(team_text_colour),
                 fill = I(team_bg_fill),
                 label = team_acronym),
             label.padding = unit(0.2, "lines"), 
             label.r = unit(0.1, "lines"),
             label.size = 0.5, family = "Oswald") +
  
  ### WORDS
  geom_shadowtext(aes(label = word), 
                  size = 10, colour = main_colour, family = "Inter SemiBold", vjust = -0.5, bg.colour = "white") + 
  
  ### SCALES
  scale_x_datetime(breaks = seq(as.POSIXct(ymd_hms(game_start_tweet$created_at, tz = "America/New_York")), as.POSIXct(game_end_tweet$created_at), time_spacer),
                   #limits = as.POSIXct(c(ymd_hms(official_game_start, tz = "America/New_York"), game_end_tweet$created_at)), 
                   labels = custom_labels) +
  scale_y_log10(labels = scales::comma_format()) +
  scale_alpha(range = c(0.9, 0.7)) + 
  expand_limits(y = 10:10000, x = as.POSIXct(c(ymd_hms(official_game_start, tz = "America/New_York") - 1800, game_end_tweet$created_at + 1800))) +
  coord_cartesian(clip = "off") +
  
  ### TITLE
  labs(title = str_glue("<b style='color:white;font-size:38px'>NFL</b>
                        <b style='color:#003a70;font-size:24px'> 2020–21 SEASON</b>
                        <br>
                        <b style='color:white;font-size:31px'>CHATTER CHARTS</b>
                        <br>
                        <b style='color:white;font-size:12px;font-family:Roboto Mono'>Fan-fueled Football Recaps</b>"),
       x= "",
       y = "") +
  
  ### OTHER
  theme(text = element_text(face = "bold", lineheight = 2),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = NA, colour = "white", size = 2),
        panel.ontop = FALSE,
        plot.background = element_rect(fill = "#4a7f3b", colour = NA),
        axis.title.y = element_markdown(size = 8, colour = grey(0.2), lineheight = 1),
        axis.text = element_text(colour = "white"),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_markdown(lineheight = 0.8, vjust = -2, #linetype = 1, 
                                      family = "Montserrat ExtraBold", #fill = "white", colour = "#003a70", 
                                      padding = margin(0,10,5,-15), margin = margin(0,0,25,0)),
        plot.subtitle = element_markdown(size = 8, family = "Raleway", colour = "black", lineheight = 0.5),
        plot.margin = margin(0.5, 1.5, 0.1, 0.5, "cm"))

### ANIMATE
animated_plot <- base_plot +
  transition_reveal(interval) + # follow the interval
  ease_aes("linear") # fixed speed

options(gganimate.dev_args = list(height = 4, width = 7.2, units = 'in', type = "cairo", res = 144))

animate(plot = animated_plot,
        fps = 25, duration = 45,
        type = "cairo",
        renderer = av_renderer(str_glue("animations/{sport}-{data_source}-{main_team_file_format}-{opponent_file_format}-{game_date}.mp4")))
