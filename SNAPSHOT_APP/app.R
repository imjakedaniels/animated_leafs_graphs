library(tidyverse, quietly = TRUE, warn.conflicts = FALSE, verbose = FALSE)
library(extrafont, quietly = TRUE)
library(ggtext, quietly = TRUE)
library(shiny)
source(here::here("snapshot_functions/00_select_game.R"))
source(here::here("snapshot_functions/01_select_accounts.R"))
source(here::here("snapshot_functions/02_fetch_timelines.R"))
source(here::here("snapshot_functions/03_take_screenshots.R"))
source(here::here("snapshot_functions/04_screenshot_fail_check.R"))
source(here::here("snapshot_functions/05_attach_metadata.R"))
source(here::here("snapshot_functions/06_attach_images.R"))
source(here::here("snapshot_functions/07_create_snapshot_animation.R"))

####### INPUTS


ui <- basicPage(
    imageOutput("plot1"))

server <- function(input, output) {
    
    select_game("New York Islanders", "2020-09-15")
    select_accounts("IslesGirl3", include_team = FALSE)
    
    ####### SCRIPT
    
    ### GET TIMELINE
    twitter_timeline <- fetch_timelines(account_names = account_name)
    
    ### TAKE SCREENSHOTS
    take_screenshots(twitter_timeline)
    
    ### QA FOR FAILED SCREENSHOTS
    R.utils::withTimeout(screenshot_fail_check(twitter_timeline, 63450), timeout = 45)
    R.utils::withTimeout(screenshot_fail_check(twitter_timeline, 60000), timeout = 45)
    
    ### ATTACH DATA TO IMAGES
    final_df <- attach_metadata(twitter_timeline) %>%
        attach_images() 
    
    ### CREATE ANIMATION
    create_snapshot_animation(final_df)
    
    output$plot1 <- renderImage({
        # A temp file to save the output.
        # This file will be removed later by renderImage
        outfile <- tempfile(fileext='.gif')
    
        
        # Return a list containing the filename
        list(src = "outfile.gif",
             contentType = 'image/gif'
             # width = 400,
             # height = 300,
             # alt = "This is alternate text"
        )}, deleteFile = TRUE)}

shinyApp(ui, server)