attach_images <- function(user_tweets_df) {
  pov_files <- file.info(list.files(str_glue("{here::here()}/custom_pov"), 
                                    full.names = TRUE)) 
  
  pov_paths <- data.frame(image_path = rownames(pov_files)) %>%
    mutate_if(is.factor, as.character) %>%
    mutate(status_id = str_extract(image_path, "[0-9]{19}"))
  
  custom_df <- user_tweets_df %>%
    inner_join(pov_paths, by = "status_id")
  
  final_df <- custom_df %>%
    bind_rows(custom_df %>% tail(1)) %>%
    bind_rows(custom_df %>% tail(1)) %>% # stall on last image
    mutate(frame = row_number()) 
  
  return(final_df)
}