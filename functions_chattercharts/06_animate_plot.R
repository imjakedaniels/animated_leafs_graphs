library(tidyverse) # for general data manipulation in R
library(extrafont) # for nice fonts
library(ggtext) # for adding html styling in plot titles
######

function(base_plot) {
  ### ANIMATE
  animated_plot <- base_plot +
    gganimate::transition_reveal(interval) + # follow the interval
    
    gganimate::animate(plot = animated_plot,
                       fps = 25, duration = round(nrow(full_data) * 0.38, digits = 0),
                       height = 608, width = 1080,  units = 'px', type = "cairo", res = 144,
                       renderer = gganimate::av_renderer(str_glue("animations/{sport}-{main_team_file_format}-{opponent_file_format}-{game_date}.mp4")))
  
  beepr::beep(sound = 2)
}