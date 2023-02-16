#' Runs the SWAP model
#'
#' @param project_path String, path to the project directory.
#' @param swap_path String, path to the swap executable (optional, will try to auto find if none is poath)
#'
#' @return returns name of run (change this!)
#' @export
#'
run_swap <- function(project_path, swap_path){

  # delete previous results, if it exists
  if(file.exists(paste0(field,"/work/*"))){
  unlink(paste0(field,"/work/*"))
  }

  # create the directories, if they dont exist.
  dir.create(paste0(field,"/archive"), showWarnings = F)
  dir.create(paste0(field,"/crop"), showWarnings = F)
  dir.create(paste0(field,"/drain"), showWarnings = F)
  dir.create(paste0(field,"/meteo"), showWarnings = F)
  dir.create(paste0(field,"/observed"), showWarnings = F)
  dir.create(paste0(field,"/output"), showWarnings = F)
  dir.create(paste0(field,"/work"), showWarnings = F)

  # parse the command to run swap
  command = paste0('dependent/swap.exe ',field,'/swap.swp')
  print(command)

  # execute command and store the status in variable "code"
  code = system(command)

  if(code!=100){print('Run failed');return(NULL)}

  # create a run name based off on the current datetime
  run_name = paste0(field,"_", Sys.time() %>%
                      str_replace_all(pattern = " ", replacement = "_") %>%
                      str_replace_all(pattern = ":", replacement = "-"))


  # remove swap.ok and reruns.log since they annoy me (feel free to change)
  unlink("swap.ok");unlink("reruns.log")

  # return the name of the run.
  return(run_name)
}

# this function archives the current model setup, so that you can revert back
# to it on a later date.
archive_run <- function(run_name){

  # path where it will be saved
  dirpath = paste0(field,"/archive/",run_name)

  # archives work and deletes saved log files
  dir.create(dirpath, showWarnings = F)
  dir.create(paste0(dirpath, "/log"), showWarnings = F)
  dir.create(paste0(dirpath,"/work"), showWarnings = F)
  file.copy(from = paste0(field,"/work"), to = paste0(dirpath, "/"), recursive = TRUE, )
  file.copy(from = paste0(field, "/crop"), to = paste0(dirpath,"/"), recursive = TRUE)
  file.copy(from = paste0(field, "/drain"), to = paste0(dirpath,"/"), recursive = TRUE)
  file.copy(from = paste0(field, "/meteo"), to = paste0(dirpath,"/"), recursive = TRUE)
  file.copy(from = paste0(field, "/swap.swp"), to = paste0(dirpath,"/swap.swp"))
  if(file.exists(paste0(field, "/swap_swap.log"))) {
    file.copy(
      from = paste0(field, "/swap_swap.log"),
      to = paste0(dirpath, "/log"),
      recursive = TRUE
    )
    file.remove(paste0(field,"/swap_swap.log"))
  }
  if (file.exists("swap.ok")) {
    file.copy(from = "swap.ok",
              to = paste0(dirpath, "/log"),
              recursive = TRUE)
    file.remove("swap.ok")
  }
  if (file.exists("reruns.log")) {
    file.copy(from = "reruns.log",
              to = paste0(dirpath, "/log"),
              recursive = TRUE)
    file.remove("reruns.log")
  }
  print(paste0("model setup archived in ",dirpath))
  return(dirpath)
}

# reads the observed file and returns in proper format
read_observed <- function(field = NA){

  # error catch if no field was provided
  if (is.na(field)) {
    print("No field site specified, RETURNING EMPTY VALUES AT DEFAULT DEPTH")
    return(tibble(
      DATE = NA,
      obsWC_15 = NA,
      obsWC_40 = NA,
      obsWC_70 = NA
    ))
  }

  # error catch if file does not exist
  if (!file.exists(paste0(field, "/observed/observed_data.xlsx"))) {
    print("NO OBSERVED FILE, RETURNING EMPTY VALUES AT DEFAULT DEPTH")
    return(tibble(
      DATE = NA,
      obsWC_15 = NA,
      obsWC_40 = NA,
      obsWC_70 = NA
    ))
  }

  # read in the file if it exists
  observed_file <- read_xlsx(path = paste0(field,"/observed/observed_data.xlsx"), sheet = 1, na = "")
  # force date into date format
  observed_file$DATE <- as.Date(observed_file$DATE)
  return(observed_file)
}





load_last_run <- function(field){

  last_run = get_last_run(field) %>% str_remove(".csv")

  if(is.na(last_run)){print("No runs saved yet. cannot load.");return(NA)}

  # reads in the daily out of SWAP
  daily_output <- read_daily_output(field)

  if (daily_output %>% length() == 0) {
    print("no model run on file, please run model first!")
    return(NA)
  }
  # reads in the measured data
  observed <- read_observed(field)


  if(is.data.frame(daily_output)==FALSE){
    print("error reading SWAP output")
    return(NA)
  }

  if(is.data.frame(observed)==FALSE){
    print("error reading observed data")
    return(NA)
  }

  # combines the measured data with the SWAP output
  full_df <- left_join(daily_output, observed, by = "DATE")

  # finds out at what depths the measurements were made
  depths = get_depths(full_df)


  # diagnostic plot
  x = daily_output$DATE
  y = daily_output[daily_output %>% colnames() %>% grepl(x=.,"WC") %>% which() %>% min()] %>% pull()
  plot(x,
       y,
       type = "l",
       main = paste("field:",field, "\nrun:",last_run, "\nvar: WC depth=", depths %>% min()),
       ylab = "WC",
       xlab = "DATE")
  print(paste("loaded run:", last_run))
  return(list(last_run, full_df, observed))

}



read_daily_output <- function(field){

  if (list.files(paste0(field, "/work/")) %>% length() == 0) {
    print("no files in work folder")
    return(NA)
  }

  daily_output <- read.table(paste0(field, "/work/result_output.csv"), comment.char = "*", sep = ",", header = T) %>% tibble()
  daily_output$DATETIME<-as.Date(daily_output$DATETIME)
  colnames(daily_output)[1] <- c("DATE")
  # fixing the bad column names
  colnames(daily_output) <- colnames(daily_output) %>% str_replace_all("\\..", "_") %>% str_replace_all("\\.", "")
  return(daily_output)
}

get_depths <- function(results){

  colnames(results)[grepl(colnames(results), pattern =  "WC_")] %>%
    str_remove_all("WC_") %>%
    str_remove_all("obs") %>%
    as.numeric() %>%
    unique() %>%
    return()
}


save_output <- function(field, data){

  print("enter custom name. [enter] for default")
  custom_name = readline()

  if(custom_name==""){run_name=data[[1]]}else{run_name = custom_name}


  if (paste0(run_name,".csv") %in% list.files(paste0(field,"/output/"))){
    list.files(paste0(field,"/output/")) %>% print()
    return("file name alreay exists. please try again")

  }

  full_df = data[[2]]
  observed = data[[3]]


  filepath = paste0(field, "/output/", run_name, ".csv")



  write.table(x = full_df,
              file = filepath,sep = ",", row.names = F, col.names = T)
  print(paste0("formatted output saved to ",filepath))

  archive_run(run_name)

}

# wrapper function for running swap. adds some functionality
# like returning the results in good format
run_model <- function(field){
  # kills the SWAP model so so that it doesnt cause any problems
  system('taskkill /IM  "swap.exe" /F', show.output.on.console = F)
  # runs the model
  last_run = run_swap(field)

  # reads in the daily out of SWAP
  daily_output <- read_daily_output(field)

  # reads in the measured data
  observed <- read_observed(field)


  if(is.data.frame(daily_output)==FALSE){
    return("error reading SWAP output")
  }

  if(is.data.frame(observed)==FALSE){
    return("error reading observed data")
  }
  # combines the measured data with the SWAP output
  full_df <- left_join(daily_output, observed, by = "DATE")

  # finds out at what depths the measurements were made
  depths = get_depths(full_df)

  # diagnostic plot
  x = daily_output$DATE
  y = daily_output[daily_output %>% colnames() %>% grepl(x=.,"WC") %>% which() %>% min()] %>% pull()
  plot(x,
       y,
       type = "l",
      main = paste("field:",field, "\nrun:",last_run, "\nvar: WC depth=", depths %>% min()),
      ylab = "WC",
      xlab = "DATE")

  print(paste("field:",field))
  print(paste("run name:", last_run))
  # returns the data as a list since R cannot return multiple objects at once
  # :(
  return(list(last_run, full_df, observed))
}


# returns the last run name
get_last_run <- function(field){
  # file list
  files = list.files(path = paste0(field, "/output"), full.names = T)
  # file infos used to sort
  details = files %>% file.info()
  # sorting and removing path and returning
  files[order(details$mtime, decreasing = T)][1] %>%
    str_remove(paste0(field,"/output/")) %>%
    return()
}

# returns run statistics in tibble format
get_statistics <- function(field, data, user.var = "WC"){

  # unpack data package
  run_name = data[[1]]
  full_df = data[[2]]
  observed = data[[3]]

  if(data[[3]][1] %>% pull() %>% length() <= 1){
    print("no observed data, cannot display statistics")
    return(NA)
  }

  # observed data depths
  depths = get_depths(full_df)
  # empty DF pre-definition
  stat.df = tibble(depth = depths, NSE = NA, PBIAS = NA, RSR = NA, RMSE = NA)

  # get statistics for each depth
  for (depth in depths) {
    # find out where the column index is for the desired user.var
    # (the amout of times i've written this, it could really use a function..)
    mod_index = colnames(full_df) %>% grepl(x=.,paste0("\\b",user.var,"_",depth,"\\b")) %>% which()
    obs_index = colnames(full_df) %>% grepl(x=.,paste0("\\bobs",user.var,"_",depth,"\\b")) %>% which()
    if(mod_index %>% length() == 0 ) {return(paste0("modelled values not found! ", user.var,"_",depth))}
    if(obs_index %>% length() == 0 ) {return(paste0("observed values not found! ", "obs",user.var, "_",depth))}

    # filter to only data with observed match


    filt_df <- full_df[which(!is.na(full_df[obs_index])),]

    obs = filt_df[[obs_index]]
    mod = filt_df[[mod_index]]

    # NSE

    NSE = NSE(obs, mod) %>% round(x = ., digits = 2)

    # PBIAS
    PBIAS = PBIAS(obs, mod) %>% round(x = ., digits = 2)

    # RMSE
    RMSE = RMSE(obs, mod) %>% round(x = ., digits = 2)

    # RSR
    RSR = RSR(obs, mod) %>% round(x = ., digits = 2)

    stat.df$NSE[which(stat.df$depth==depth)] = NSE
    stat.df$PBIAS[which(stat.df$depth==depth)] = PBIAS
    stat.df$RMSE[which(stat.df$depth==depth)] = RMSE
    stat.df$RSR[which(stat.df$depth==depth)] = RSR
  }

  return(stat.df)
}

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

  # if it is depthwise, then add a column with the correct depth
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

# NSE
NSE <-
  function(obs, mod) {
    return((1 - (sum((obs - mod) ^ 2, na.rm = T
    ) / sum((obs - mean(obs, na.rm = T)) ^ 2, na.rm = T
    ))) %>% round(x = ., digits = 2))
  }

# PBIAS
PBIAS <-
  function(obs, mod) {
    return(((sum(obs - mod, na.rm = T) * 100) / sum(obs, na.rm = T)) %>% round(x = ., digits = 2))
  }

# RMSE
RMSE <-
  function(obs, mod) {
    return(sqrt(sum((obs - mod) ^ 2, na.rm = T) / length(obs)) %>% round(x = ., digits = 2))
  }

# RSR
RSR <-
  function(obs, mod) {
    return(RMSE(obs, mod) / sd(obs, na.rm = T) %>% round(x = ., digits = 2))
  }
