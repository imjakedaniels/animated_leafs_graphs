create_snapshot_animation <- function(df) {
  
  ### TEAM LOGOS
  main_logo <- grid::rasterGrob(png::readPNG(main_image$path), interpolate = TRUE)
  opponent_logo <- grid::rasterGrob(png::readPNG(opponent_image$path), interpolate = TRUE)
  
  ### PLOT
  base_plot <- df %>%
    ggplot(aes(x = interval, y = interval_volume)) +
    
    ### PRE/POST GAME LABELS
    annotate("text", x = lubridate::ymd_hms(game_start_tweet$created_at, tz = "America/New_York") - 1250, 
             y = 12, size = 3,  label = "Pre-Game", colour = "black", family = "Inter Medium") +
    annotate("text", x = lubridate::ymd_hms(game_end_tweet$created_at, tz = "America/New_York") + 1350, 
             y = 12, size = 3, label = "Post-Game", colour = "black", family = "Inter Medium")  +
    annotate("text", x = lubridate::ymd_hms(game_start_tweet$created_at, tz = "America/New_York") - 1250, 
             y = 550, size = 3,  label = "Pre-Game", colour = "black", family = "Inter Medium") +
    annotate("text", x = lubridate::ymd_hms(game_end_tweet$created_at, tz = "America/New_York") + 1350, 
             y = 550, size = 3, label = "Post-Game", colour = "black", family = "Inter Medium") +
    
    ### INTERMISSION MARKERS
    geom_rect(data = intermission_df, 
              aes(NULL, NULL, 
                  xmin = start, xmax = end, 
                  ymin = 10, ymax = 600) , 
              fill = 'grey', alpha = 0.2) +
    
    ### TEAM LOGOS
    annotation_custom(main_logo, 
                      xmin = mean(c(game_start_tweet$created_at, game_end_tweet$created_at)) - 1200,  
                      xmax = game_start_tweet$created_at + 4500, 
                      ymin = -Inf, ymax = Inf) +
    
    annotation_custom(opponent_logo,
                      xmin = mean(c(game_start_tweet$created_at, game_end_tweet$created_at)) + 1200, 
                      xmax = game_end_tweet$created_at - 4500,
                      ymin = -Inf, ymax = Inf) +
    
    ### TWEETs
    ggimage::geom_image(aes(x = mean(c(game_start_tweet$created_at, game_end_tweet$created_at)),
                   y = 18000,
                   image = image_path),
               size = 0.45, by = "height") +
    
    ### GAME DURATION MARKERS
    geom_segment(data = game_time_df, 
                 aes(x = created_at, xend = created_at,
                     y = 10, yend = 600),
                 size = 0.5) +
    
    ### GOALS
    geom_segment(data = scoring_df, 
                 aes(x = created_at, xend = created_at,
                     y = 10, yend = 600,
                     colour = I(colour)),
                 linetype = 5,
                 size = 0.4) +
    
    ### TEAM LABELS
    geom_label(data = team_scoring_markers %>%
                 mutate(y_location = ifelse(y_location == 3, 10, 600)), 
               aes(x = created_at, 
                   y = I(y_location),
                   colour = I(team_text_colour),
                   fill = I(team_bg_fill),
                   label = team_acronym),
               label.padding = unit(0.15, "lines"), 
               label.r = unit(0.12, "lines"),
               label.size = 0.1, size = 3, family = "Oswald") +
    
    ### SCOREBOARD  
    shadowtext::geom_shadowtext(aes(x = mean(c(game_start_tweet$created_at, game_end_tweet$created_at)),
                        y = 600,
                        label = running_score), 
                    size = 10, colour = "white", family = "Quantico", vjust = -0.7, show.legend = FALSE) +
    
    ### LINE
    geom_line(data = full_data %>% 
                rename(created_at = interval) %>%
                mutate(interval_volume = ifelse(interval_volume < 10, 10, interval_volume)), 
              aes(x= created_at, y = interval_volume), 
              color = main_colour, size = 2.5) +
    
    ### TEAM NAMES
    geom_text(aes(x = mean(c(game_start_tweet$created_at, game_end_tweet$created_at)) - 3200,
                  y = 1000,
                  label = main_acronym),
              family = "Racing Sans One",
              size = 10, colour = main_colour) +
    
    geom_text(aes(x = mean(c(game_start_tweet$created_at, game_end_tweet$created_at)) + 3200,
                  y = 1000,
                  label = opponent_acronym),
              family = "Racing Sans One",
              size = 10, colour = opponent_colour) +
    
    ### LINE ICON
    geom_point(size = 12, colour = main_colour, alpha = 0.3) +
    geom_point(size = 8, colour = main_colour, alpha = 0.5) +
    geom_point(size = 3, colour = "white") +
    
    expand_limits(y = 10:100000, x = c(game_start_tweet$created_at - 1800, game_end_tweet$created_at + 1800)) +
    coord_cartesian(clip = "off") +
    scale_y_log10(breaks = c(10, 100, 1000, 10000, 100000),
                  labels = c("10 -", "100 -", "1000 -", "", "")) +
    
    ### TITLE 
    labs(caption = paste0("SNAP: A Personalized Highlight Reel | Built by @ChatterCharts"),
         x= "",
         y = "") +
    
    theme_light(base_family = "Montserrat ExtraBold") +
    
    ### OTHER
    theme(text = element_text(lineheight = 1),
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.border = element_blank(),
          axis.line.x.bottom = element_line(),
          panel.background = element_blank(),
          panel.ontop = FALSE,
          plot.background = element_blank(),
          plot.caption = element_text(size = 10, family = "Inter", hjust = 0.5),
          plot.margin = margin(0, 0.5, 0.5, 0, "cm")) 
  
  ### ANIMATE
  animated_plot <- base_plot +
    gganimate::transition_manual(frame) # follow the interval
  
  file_account_name <- str_replace_all(tolower(account_name), " ", "-")
  
  gganimate::animate(plot = animated_plot, nframes = nrow(final_df), fps = 0.5,
          height = 1080, width = 1080,  units = 'px', type = "cairo", res = 144,
          renderer = gganimate::gifski_renderer("outfile.gif"))
  
  beepr::beep(sound = 2)
  
  temp_files <- file.info(list.files(here::here("custom_pov"), 
                                     full.names = TRUE))
  
  file.remove(rownames(temp_files))
  
}
