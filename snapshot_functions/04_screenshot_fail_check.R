screenshot_fail_check <- function(twitter_timeline_df, max_size = 63450) {
  
  # Check if pictures already exist
  snapshots_qa <- file.info(list.files(here::here("custom_pov"), 
                                       full.names = TRUE))
  
  pov_check <- data.frame(image_path = rownames(snapshots_qa),
                          size = snapshots_qa$size)%>%
    mutate_if(is.factor, as.character) %>%
    mutate(status_id = str_extract(image_path, "[0-9]{19}"))  %>%
    filter(size < max_size)
  
  message(paste("Might have taken", nrow(pov_check), "bad screen shots. Recapturing them."))
  
  file.remove(pov_check$image_path)
  
  twitter_timeline_cut <- twitter_timeline_df %>%
    filter(status_id %in% pov_check$status_id)
  
  pb <- progress::progress_bar$new(format = "[:bar] :current/:total (:elapsed)", total = nrow(twitter_timeline_cut))
  
  ### GET SCREENSHOTS
  for (i in 1:nrow(twitter_timeline_cut)) {
    
    tweetrmd::tweet_screenshot(tweetrmd::tweet_url(screen_name = twitter_timeline_cut[i,]$screen_name, 
                                                   status_id = twitter_timeline_cut[i,]$status_id), 
                               scale = 3,
                               maxwidth = 400,
                               hide_thread = FALSE,
                               hide_media = TRUE,
                               file = paste0(here::here("custom_pov"), "/", twitter_timeline_cut[i,]$status_id,".png"))
    
    pb$tick(1)
    
  }
}