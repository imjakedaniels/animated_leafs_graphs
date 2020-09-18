select_accounts <- function(account_names, include_team = TRUE) {
  
  assign("include_team", include_team, envir = globalenv())
  
  if (str_detect(account_names, ",")) {
    
    cleaned_accounts <- account_names %>%
      str_replace_all("@", "") %>%
      str_split(", ") %>%
      unlist()
    
    assign("account_name", cleaned_accounts, globalenv())
    
    message(str_glue("Creating SNAPSHOT of {length(cleaned_accounts)} accounts."))
    
  } else {
    
    assign("account_name", str_replace_all(account_names, "@", ""), globalenv())
    
    message(str_glue("Creating SNAPSHOT of @{account_name}."))
    
  }
}
