select_game <- function(main_team, game_date) {
  
  ### MAIN TEAM
  assign("main_team", main_team, envir = globalenv())  
  assign("game_date", as.Date(game_date), envir = globalenv())
  
  ### FIND OPPONENT
  environment_folder_files <- file.info(list.files(str_glue("{here::here()}/saved_environments"), 
                                                   full.names = TRUE)) 
  
  main_team_file_format <- str_replace_all(tolower(main_team), " ", "-")
  
  environment_path <- data.frame(environment_path = rownames(environment_folder_files)) %>%
    filter(str_detect(environment_path, str_glue("CHATTER-{main_team_file_format}")),
           str_detect(environment_path, str_glue("{game_date}"))) %>%
    mutate_if(is.factor, as.character) %>%
    .$environment_path
  
  opponent <- environment_path %>%
    str_remove(str_glue(".*CHATTER-{main_team_file_format}-")) %>%
    str_remove("-[0-9]+.*") %>%
    str_replace_all("-", " ") %>%
    str_to_title()
  
  assign("opponent", opponent, envir = globalenv())
  
  if (file.exists(environment_path)) {
    
    load(environment_path, envir = globalenv())
    
  } else {
    
    message("You might have the wrong date or I don't have this game's data (sorry!)")
    stop()
    
  }
}
