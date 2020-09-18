create_scoring <- function(score_break_1 = NULL, correct_score_1 = NULL,
                           score_break_2 = NULL, correct_score_2 = NULL) {
  
  if (sport == "hockey") {
    ### FIND GOALS
    goal_df <- main_team_goals  %>%
      bind_rows(opponent_goals, .id = "team") %>%
      mutate(linetype = ifelse(team == 1, 2, 5),
             colour = ifelse(team == 1, main_colour, opponent_colour))
    
    ### CREATE SCORING
    scoring_df <-  main_team_goals  %>%
      bind_rows(opponent_goals, .id = "team") %>%
      mutate(team = ifelse(team == 1, "main_team", "opponent")) %>%
      mutate(linetype = 2,
             colour = ifelse(team == "main_team", main_colour, opponent_colour),
             points = 1,
             size = 0.75) %>%
      group_by(team) %>%
      arrange(created_at) %>%
      mutate(score = cumsum(points)) %>%
      spread(team, score) %>%
      fill(length(.), length(.) - 1)
    
  }
  
  scoring_df[, 9][is.na(scoring_df[, 9])] <- 0
  
  ## OVERTIME FIX
  
  if (length(scoring_df) == 10) {
    
    scoring_df[, 10][is.na(scoring_df[, 10])] <- 0
    
  }
  
  if (length(scoring_df) == 10) {
    
    score_sequence <- scoring_df %>%
      mutate(running_score = str_glue("{main_team} - {opponent}")) 
    
  } else {
    
    if (sum(str_detect(names(scoring_df), "main_team")) == 1) {
      
      score_sequence <- scoring_df %>%
        mutate(running_score = str_glue("{main_team} - 0")) 
      
    } else {
      
      score_sequence <- scoring_df %>%
        mutate(running_score = str_glue("0 - {opponent}")) 
      
    }
  }
  
  assign("score_sequence", score_sequence, envir = globalenv())
  assign("scoring_df", scoring_df, envir = globalenv())
  
  ### SCOREBOARD
  running_game_score_df <- score_sequence %>%
    rename(interval = created_at) %>%
    mutate(running_score = as.character(running_score)) %>%
    bind_rows(data.frame(interval = score_sequence$created_at + lubridate::minutes(2), # iser
                         running_score = as.character(score_sequence$running_score))) %>%
    mutate(board_size = 15) %>%
    full_join(full_data, by = "interval") %>%
    arrange(interval) %>%
    mutate(board_size = ifelse(is.na(running_score), 12, board_size)) %>%
    mutate(board_size = ifelse(interval %in% lubridate::round_date(scoring_df$created_at + lubridate::minutes(1), "2 minutes"), 15, board_size)) %>%
    arrange(interval) %>%
    fill(running_score, .direction = "down") %>%
    mutate(running_score = ifelse(is.na(running_score), "0 - 0", running_score)) %>%
    mutate(board_size = ifelse(running_score == "0 - 0", 12, board_size)) %>%
    select(interval, running_score, board_size) 
  
  if (!is.null(score_break_1)) {
    running_game_score_df %>% 
      mutate(running_score = ifelse(interval %in% score_break_1, correct_score_1, running_score))
    
  }
  
  if (!is.null(score_break_2)) {
    running_game_score_df %>% 
      mutate(running_score = ifelse(interval %in% score_break_2, correct_score_2, running_score))
    
  }
  
  assign("running_game_score_df", running_game_score_df, envir = globalenv())
  
  ### "FINAL" ON SCOREBOARD
  final_score_text <- running_game_score_df %>%
    mutate(text = ifelse(interval >= game_end_tweet$created_at, "Final", ""))
  
  assign("final_score_text", final_score_text, envir = globalenv())
  
  ### TEAM GOAL LABELS
  team_scoring_markers <- scoring_df %>%
    select(created_at, colour) %>%
    mutate(team_acronym = ifelse(colour == main_team_data$colour_primary,  main_team_data$team_acronym, opponent_team_data$team_acronym),
           team_bg_fill =  ifelse(colour == main_team_data$colour_primary,  main_team_data$colour_primary, opponent_team_data$colour_primary),
           team_text_colour = ifelse(colour == main_team_data$colour_primary,  "white", "white"),
           y_location = ifelse(colour == main_team_data$colour_primary, ifelse(max(full_data$interval_volume) < 1000, 1000, max(full_data$interval_volume)), 3)) %>%
    rename(interval = created_at) %>%
    inner_join(running_game_score_df, by = "interval") %>%
    rename(created_at = interval)
  
  assign("team_scoring_markers", team_scoring_markers, envir = globalenv())
  
  return(running_game_score_df)
  
}

