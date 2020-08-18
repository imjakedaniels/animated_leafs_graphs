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
w <- matrix(rgb(m[,,1],m[,,2],m[,,3], m[,,4] * 0.8), nrow = dim(m)[1]) 
main_logo <- rasterGrob(w, interpolate = TRUE)

# find opponent image 
opponent_image <- image_paths %>%
  mutate_if(is.factor, as.character) %>%
  filter(str_detect(path, str_replace_all(opponent, " ", "_")))

m <- readPNG(opponent_image$path)
w <- matrix(rgb(m[,,1],m[,,2],m[,,3], m[,,4] * 0.8), nrow = dim(m)[1]) 
opponent_logo <- rasterGrob(w, interpolate = TRUE)

## Sponsor Banner (future)
reddit_png <- readPNG(here::here("social_images/reddit_brand.png"))

reddit_brand <- rasterGrob(reddit_png, interpolate = TRUE, 
                           width=unit(4,'cm'),
                           x = unit(1,"npc"), y = unit(1,"npc"),
                           hjust = 0.9, vjust= 0)


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
  
  ### GAME DURATION MARKERS
  geom_vline(data = game_time_df, 
             aes(xintercept = created_at),
             colour = grey(0.8)) +
  
  ### LINE
  geom_line(color = "white", size = 2) +
  
  ### LINE SOCIAL ICON
  geom_image(aes(image = "emoji/basketball.png"), size = 0.08) +
  
  ### WORDS
  geom_shadowtext(aes(label = word), 
                  size = 10, colour = "white", family = "Inter SemiBold", vjust = -0.5, bg.colour = "black") +
  
  ### SCALES
  scale_x_datetime(breaks = scales::breaks_width(width = "1 hour"),
                   labels = scales::label_date(format = "%I:%M %p", tz = "America/New_York")) +
  scale_y_log10() +
  expand_limits(y = 10:1000, x = c(game_start_tweet$created_at - 1800, game_end_tweet$created_at + 1800)) +
  coord_cartesian(clip = "off") +

  ### TITLE 
  labs(title = str_glue("<b style='color:white;font-size:20px'>NBA</b>
                         <b style='color:#b5b7bb;font-size:16px'>2019-20 Playoffs</b>
                         <br>
                         <b style='font-size:20px;color:white'>CHATTER CHARTS"),
       
       caption = str_glue("Showing the most statistically important word every two-minutes using {scales::comma(sum(full_data$thread_volume))} real-time social media comments."),
       x= "",
       y = "") +
  
 # ### OTHER
 # theme(text = element_text(lineheight = 1),
 #       panel.grid = element_blank(),
 #       axis.title.y = element_markdown(size = 8, colour = grey(0.2), lineheight = 1),
 #       axis.ticks = element_line(colour = grey(0.2)),
 #       axis.text.x = element_blank(),
 #       panel.border = element_blank(),
 #       panel.background = element_rect(fill = NA, colour = grey(0.1), size = 0.75),
 #       panel.ontop = FALSE,
 #       plot.background = element_rect(fill = NA, colour = NA),
 #       plot.title = element_markdown(lineheight = 0.5, #linetype = 1, 
 #                                     family = "Montserrat ExtraBold", 
 #                                     padding = margin(0,10, 2, 0), margin = margin(0,0,2,0)),
 #       plot.subtitle = element_markdown(size = 6, family = "Inter",),
 #       plot.caption = element_text(size = 8, family = "Inter", hjust = 0, vjust = 8),
 #       plot.margin = margin(0.5, 1.5, 0.1, 0.5, "cm")) 
#
theme(text = element_text(lineheight = 1),
      panel.grid = element_blank(),
      axis.line = element_line(colour = grey(0.8), size = 1),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(colour = grey(0.8)),
      panel.border = element_blank(),
      panel.background = element_rect(fill = NA, colour = grey(0.8), size = 0.75),
      panel.ontop = FALSE,
      plot.background = element_rect(fill = "black", colour = "black"),
      axis.ticks = element_line(colour = grey(0.8)),
      plot.title = element_markdown(lineheight = 0.5, #linetype = 1, 
                                    family = "Montserrat ExtraBold", 
                                    padding = margin(0,10, 2, 0), margin = margin(0,0,2,0)),
      plot.caption = element_text(colour = "white", size = 8, family = "Inter", hjust = 0, vjust = 8),
      plot.margin = margin(0.5, 1.5, 0.1, 0.5, "cm")) 

### ANIMATE
animated_plot <- base_plot +
  transition_reveal(interval) + # follow the interval
  ease_aes("linear") # fixed speed

animate(plot = animated_plot,
        fps = 25, duration = 35,
        height = 608, width = 1080,  units = 'px', type = "cairo", res = 144,
        renderer = av_renderer(str_glue("animations/basketball-{data_source}-{main_team_file_format}-{opponent_file_format}-{game_date}.mp4")))
