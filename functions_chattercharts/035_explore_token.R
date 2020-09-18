explore_token <- function(sketchy_word) {
  
  token_df <- rounded_intervals_df %>%
    filter(str_detect(text, sketchy_word)) %>%
    select(text, created_at)
  
  return(token_df)
  
}