make_base_plot <- function() {
  
  ### SPONSOR BANNER
  #reddit_png <- readPNG(here::here("social_images/reddit_brand.png"))
  #
  #reddit_brand <- rasterGrob(reddit_png, interpolate = TRUE, 
  #                           width=unit(4,'cm'),
  #                           x = unit(1,"npc"), y = unit(1,"npc"),
  #                           hjust = 0.9, vjust= 0)
  
  
  bg_img <- png::readPNG(here::here("backgrounds/hockey_bg.png"))
  background_img <- grid::rasterGrob(bg_img, interpolate = TRUE)
  
  
  
  ### TEAM LOGOS
  ## lookup all team image files in the repo
  files <- file.info(list.files(str_glue("{here::here()}/team_images"), 
                                full.names = TRUE))
  
  image_paths <- data.frame(path = rownames(files))
  
  ## HOME TEAM
  main_image <- image_paths %>%
    mutate_if(is.factor, as.character) %>%
    filter(str_detect(path, str_replace_all(main_team, " ", "_")))
  
  m <- png::readPNG(main_image$path)
  w <- matrix(rgb(m[,,1],m[,,2],m[,,3], m[,,4] * 0.9), nrow = dim(m)[1]) 
  main_logo <- grid::rasterGrob(w, interpolate = TRUE)
  
  ## OPPONENT
  opponent_image <- image_paths %>%
    mutate_if(is.factor, as.character) %>%
    filter(str_detect(path, str_replace_all(opponent, " ", "_")))
  
  m <- png::readPNG(opponent_image$path)
  w <- matrix(rgb(m[,,1],m[,,2],m[,,3], m[,,4] * 0.9), nrow = dim(m)[1]) 
  opponent_logo <- grid::rasterGrob(w, interpolate = TRUE)
  
  
  ### PLOT
  base_plot <- full_data %>%
    # filter(interval < "2020-09-03 18:58:00") %>%
    ggplot(aes(x = interval, y = interval_volume)) +
    
    ggimage::geom_bgimage(here::here("backgrounds/hockey_bg.png")) +
  
    #### Ice
   #annotation_custom(grob = background_img, 
   #                  xmin = -Inf,  
   #                  xmax = Inf, 
   #                  ymin = -Inf, ymax = Inf) +
   
    geom_rect(data = intermission_df, 
              aes(NULL, NULL, xmin=start, xmax=end), ymin=-Inf, ymax=Inf, 
              fill = 'grey', alpha = 0.3) +
    
    ### BG LOGOS
    annotation_custom(main_logo, 
                      xmin = mean(c(game_start_tweet$created_at, game_end_tweet$created_at)) - 100,  
                      xmax = game_start_tweet$created_at + 100, 
                      ymin = -Inf, ymax = Inf) +
    annotation_custom(opponent_logo,
                      xmin = mean(c(game_start_tweet$created_at, game_end_tweet$created_at)) + 100, 
                      xmax = game_end_tweet$created_at - 100,
                      ymin = -Inf, ymax = Inf) +
    
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
               label.size = 0.1, size = 3, family = "Oswald") +
    
    ### SCOREBOARD  
    geom_text(data = final_score_text,
              aes(x = mean(c(game_start_tweet$created_at, game_end_tweet$created_at)),
                  y = ifelse(max(full_data$interval_volume) < 1000, 1000, max(full_data$interval_volume)),
                  label = text), 
              size = 3.5, vjust = -5, colour = "black", family = "Quantico", show.legend = FALSE) +
    
    shadowtext::geom_shadowtext(data = running_game_score_df,
                    aes(x = mean(c(game_start_tweet$created_at, game_end_tweet$created_at)),
                        y = ifelse(max(full_data$interval_volume) < 1000, 1000, max(full_data$interval_volume)),
                        label = running_score, 
                        size = I(board_size)), 
                    colour = "white", family = "Quantico", vjust = -0.7, show.legend = FALSE) +
    
    ### LINE
    geom_line(color = main_colour, size = 2) +
    
    ### LINE ICON
    ggimage::geom_image(aes(image = "emoji/hockey_puck.png")) +
    
    ### WORDS
    shadowtext::geom_shadowtext(aes(label = word), 
                                size = 10, colour = "black", family = "Inter SemiBold", vjust = -0.5, bg.colour = "white") + 
    
    ### SCALES
    scale_x_datetime(breaks = scales::breaks_width(width = "1 hour"),
                     labels = scales::label_date(format = "%I:%M %p", tz = "America/New_York")) +
    scale_y_log10() +
    expand_limits(y = 10:1000, x = c(game_start_tweet$created_at - 1800, game_end_tweet$created_at + 1800)) +
    coord_cartesian(clip = "off") +
    
    ### TITLE 
    labs(title = str_glue("<b style='color:black;font-size:20px'>NHL</b>
                         <b style='color:#b5b7bb;font-size:17px'>2020 PLAYOFFS</b>
                         <br>
                         <b style='font-size:20px'>CHATTER CHARTS"),
         
         caption = str_glue("Showing the most statistically important word every two-minutes using {scales::comma(sum(full_data$interval_volume))} real-time social media comments."),
         x= "",
         y = "") +
    
    theme_light(base_family = "Montserrat ExtraBold") +
    
    ### OTHER
    theme(text = element_text(lineheight = 1),
          panel.grid = element_blank(),
          axis.title.y = ggtext::element_markdown(size = 8, colour = grey(0.2), lineheight = 1),
          axis.ticks = element_line(colour = grey(0.2)),
          axis.text.x = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = NA, colour = grey(0.1), size = 0.75),
          panel.ontop = FALSE,
          plot.background = element_rect(fill = NA, colour = NA),
          plot.title = ggtext::element_markdown(lineheight = 0.5, #linetype = 1, 
                                        family = "Montserrat ExtraBold", 
                                        padding = margin(0,10, 2, 0), margin = margin(0,0,2,0)),
          plot.caption = element_text(size = 8, family = "Inter", hjust = 0, vjust = 8),
          plot.margin = margin(0.5, 1.5, 0.1, 0.5, "cm")) 
  
  assign("base_plot", base_plot, envir = globalenv())
  return(base_plot)
  
}