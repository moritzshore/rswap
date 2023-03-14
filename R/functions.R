# misc functions to be converted
################################################################################


melt_all_runs <- function(field, data, user.var = "WC"){
  # past runs -----
  run_name <- data[[1]]
  past_run_names <-  list.files(path = paste0(field, "/output/")) %>% str_remove(".csv")

  if(past_run_names %>% is_empty()){warning("no previous runs to compare to");return(NA)}

  # if the currently loaded run has been saved before, remove it from the past
  # runs to prevent duplication
  if(run_name %in% past_run_names) {
    past_run_names_filtered = past_run_names[-c(which(run_name == past_run_names))]
  }else{past_run_names_filtered = past_run_names}

  # convert the run names to the run paths
  past_run_paths = past_run_names_filtered %>% paste0(field, "/output/", .,".csv")
  # load the past run data
  past_run_df <- past_run_paths %>% map_df(., ~ vroom(.x, id = "path", show_col_types = F))
  # tag it
  past_run_df$tag = "past"
  # remove path from path, leaving just the run name
  past_run_df$path <- past_run_df$path %>% str_remove(paste0(field,"/output/")) %>% str_remove(".csv")

  # find the user.var columns
  user_var_cols <- colnames(past_run_df) %>% grepl(., pattern = user.var) %>% which()
  # select the needed columns
  selected_df <- past_run_df %>% select(path, DATE, all_of(user_var_cols), tag)
  # de-select the observed columns
  selected_df_filtered <- selected_df %>% select(-c(colnames(selected_df) %>% grepl(.,pattern="obs") %>% which()))
  # melt by path, id and tag
  past_melted_df <- melt(selected_df_filtered, id.vars = c("path","DATE", "tag")) %>% tibble()

  # current run -----
  # loading the current run data
  current_run_df = data[[2]]
  # tagging it right
  current_run_df$tag = "current"
  # need to add a fake path
  current_run_df$path = run_name

  # find the user.var columns
  user_var_cols <- colnames(current_run_df) %>% grepl(., pattern = user.var) %>% which()
  # select the needed columns
  selected_df <- current_run_df %>% select(path, DATE, all_of(user_var_cols), tag)
  # removing the auto-added observed data
  selected_df_filtered <- selected_df %>% select(-c(colnames(selected_df) %>% grepl(.,pattern="obs") %>% which()))
  # melt by path, id and tag
  current_melted_df <- melt(selected_df_filtered, id.vars = c("path","DATE", "tag")) %>% tibble()

  # loading the observed data
  observed_df = data[[3]]
  # and tagging it
  observed_df$tag = "observed"
  observed_df$path = "observed"

  # find the user.var columns
  user_var_cols <- colnames(observed_df) %>% grepl(., pattern = user.var) %>% which()

  # select the needed columns
  selected_df <- observed_df %>% select(path, DATE, all_of(user_var_cols), tag)

  # melt by path, id and tag
  observed_melted_df <- melt(selected_df, id.vars = c("path","DATE", "tag")) %>% tibble()

  # all together -----

  # if no matching observed data is found, then don't add it
  if (length(user_var_cols) == 0) {
    print(paste0("no matching observed data for ", user.var, "!"))
    full_melt <- rbind(current_melted_df, past_melted_df)
  } else{
    full_melt <-
      rbind(current_melted_df, past_melted_df, observed_melted_df)
  }


  # check to see if the variable is depthwise.
  # if this reuturns NA, then it is NOT depthwise
  depth = full_melt$variable[1] %>%
    str_remove("obs") %>%
    str_remove(paste0(user.var,"_")) %>%
    as.numeric() %>% suppressWarnings()

  if(length(depth)==0){return("error, variable not recognized")}

  # if it is depth wise, then add a column with the correct depth
  if(!is.na(depth)){
    full_melt$depth = full_melt$variable %>% str_remove("obs") %>% str_remove(paste0(user.var,"_")) %>% as.numeric()
    # and remove the depth tag from the variable name
    full_melt$variable = user.var
  }else{
    # otherwise, depth is NA
    full_melt$depth = NA
  }

  return(full_melt)
}

# stat functions

