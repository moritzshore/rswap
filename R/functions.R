#' Makes a temporary sub-directory workspace for the package to run in.
#' @param project_path string, path to project directory
#'
#' @importFrom glue glue
#' @importFrom dplyr %>%
#'
#' @keywords internal
#'
build_rswap_directory <- function(project_path){

  temp_directory <- glue("{project_path}/rswap/")
  # create the hidden temp directory
  dir.create(temp_directory, showWarnings = F, mode = "0777")

  # list all the files in the original project directory
  #TODO might want to check the options here, so you get ALL the files and none more
  file_list <- list.files(project_path, full.names = T, recursive = T)

  # vector of all the files i want to copy over
  file_types <- c("*.crp", "*.met", "*.swp", "*.dra", "layer*n.csv")
  match_string <- paste(file_types, collapse = "|")

  # find out the index of the required files and copy only those over to the temp directory
  required_files <- file_list %>% grepl(x = ., match_string) %>% which()
  required_file_list <- file_list[required_files]
  status <- file.copy(from = required_file_list, to = temp_directory)

  # legacy support for old met files:
  # copies over any files with a "numeric" file type. (best way i could think of)
  not_numeric <- str_split(file_list, "[.]", simplify = T)[,2] %>% as.numeric() %>% is.na() %>% suppressWarnings()
  met_files <- file_list[which(not_numeric == FALSE)]
  met_status <- file.copy(from = met_files, to = temp_directory)

  # return the path to the temp directory
  return(temp_directory)
}

#' updates the file paths in the swap main file
#' @param temp_directory path to the temp directory
#' @param swap_file name of the swap file to be modified
#' @importFrom glue glue
#' @importFrom dplyr %>%
#' @keywords internal
update_swp_paths <- function(temp_directory, swap_file, swap_exe){

  swap_file_lines <- read_lines(glue("{temp_directory}/{swap_file}"))

  swap_file_new <- swap_file_lines

  project_line = swap_file_lines %>% grepl(x=., "PROJECT", fixed = T) %>% which()
  pathwork_line = swap_file_lines %>% grepl(x=., "PATHWORK", fixed = T) %>% which()
  pathatm_line = swap_file_lines %>% grepl(x=., "PATHATM", fixed = T) %>% which()
  pathcrop_line = swap_file_lines %>% grepl(x=., "PATHCROP", fixed = T) %>% which()
  pathdrain_line = swap_file_lines %>% grepl(x=., "PATHDRAIN", fixed = T) %>% which()

 # "tetves/rswap/

 path_split = swap_exe %>% str_split("/", simplify = T)
 swap_file_name = path_split[5]
 path_without_swap <-  swap_exe %>% str_remove(swap_file_name)
 swap_main_file_path <- temp_directory %>% str_remove(path_without_swap)

 swap_file_new[pathwork_line] <-  glue("  PATHWORK  = '{swap_main_file_path}' ! changed by rswap {Sys.time()}")
 swap_file_new[pathatm_line] <-   glue("  PATHATM   = '{swap_main_file_path}' ! changed by rswap {Sys.time()}")
 swap_file_new[pathcrop_line] <-  glue("  PATHCROP  = '{swap_main_file_path}' ! changed by rswap {Sys.time()}")
 swap_file_new[pathdrain_line] <- glue("  PATHDRAIN = '{swap_main_file_path}' ! changed by rswap {Sys.time()}")

 new_swap_file_name = glue("{temp_directory}rswap_{swap_file}")

 writeLines(text = swap_file_new, con = new_swap_file_name )

 return(new_swap_file_name)
}

#' Runs the SWAP model
#'
#' @param project_path String, path to the project directory.
#' @param swap_exe String, path to the swap executable (optional, will try to auto find if none is poath)
#' @param string name of the *.swp main file
#' @param verbose logical
#' @param timeout number of seconds before run timeout (optional, unlimited by default)
#'
#' @return returns name of run (change this!)
#'
#' @importFrom glue glue
#' @importFrom processx run
#' @importFrom dplyr %>%
#' @export
#'
run_swap <- function(project_path, swap_exe, swap_file, verbose = T, timeout = Inf){

  # create a temp directory to work in
  temp_directory <- build_rswap_directory(project_path)

  # update the swap main file with the new paths to the input files
  file_path <- update_swp_paths(temp_directory, swap_file, swap_exe)

  # parse the working directory from the given swap path
  swap_path_split = swap_exe %>% str_split("swap.exe", simplify = T)
  swap_wd <- swap_path_split[,1]

  # remove the working directory from the path of the swap main file
  fixed_path <- file_path %>% str_remove(swap_wd)

  # KILL swap before running to avoid file locking issues.
  #system('taskkill /IM  "swap.exe" /F', show.output.on.console = verbose)

  # run the model
  msg <- run( command = "swap.exe", wd = swap_wd, args = fixed_path,
      error_on_status = F,
      timeout = timeout,
      echo_cmd = verbose,
      echo = verbose
    )

  # TODO improve this
  if (msg$status != "100") {
    warning(glue("SWAP error, code {msg$status}"))
  }

  # return status of run
  return(msg$status)
}

#' Save a swap run
#'
#' @param project_path String, path to the project directory.
#' @param save_location String, path to directory where the model files are to
#' be saved. default is "project_directory"/rswap_saved_runs/
#' @param run_name name of run to be saved. default is "rswap_{time,date}"
#' @param verbose logical
#'#'
#' @importFrom glue glue
#' @importFrom dplyr %>%
#' @importFrom stringr str_replace_all
#' @export
#'
save_run <- function(project_path, save_location = NULL, run_name = NULL, verbose = F){

  if(run_name %>% is.null()){
    tad = Sys.time() %>%  str_replace_all(":", "_") %>% str_replace_all(" ","at")
    run_name = glue("rswap_{tad}")
  }

  if(save_location %>% is.null()){
    save_location = glue("{project_path}/rswap_saved")
  }

  # create the save folder of ALL the saves
  dir.create(save_location, showWarnings = F)

  # create the save folder for the individual run
  to_path = glue("{save_location}/{run_name}")
  dir.create(to_path, showWarnings = T)

  from_path = glue("{project_path}/rswap/")

  from_copy = list.files(from_path, full.names = T)
  to_copy = glue("{to_path}/{list.files(from_path)}")

  status = file.copy(from_copy, to_copy)

  if (verbose) {
    if (any(status == FALSE)) {
      cat("some files were not copied!\n")
      cat(to_copy[which(status == FALSE)], sep = "\n")
    } else{
      cat("all files succesfully copied to:\n")
      cat(to_path, "\n")
    }
  }
  #TODO generate a preview plot of the model run in the directory?
}

#' Load observed data (make sure to use the template .xlsx file)
#' @param path String. Path to observed data file (.xlsx)
#' @param verbose Logical. Prints status reports
#' @importFrom readxl read_excel
#' @importFrom stringr str_remove str_replace str_split
#'
#' @export
load_observed <- function(path, verbose = F){

  # TODO: maybe this should be internal
  # TODO: install the template file via package!

  data <- read_excel(path)

  data$DATE <- data$DATE %>% as.Date()

  columns <- colnames(data)

  # find a better way to do this. or remove it
  date_col <- columns %>% grepl(x = ., "DATE") %>% which()

  obs_cols <- columns %>%  grepl(x = ., "obs*")

  col_sep <- columns[obs_cols] %>% str_remove("obs") %>% str_split("_") %>% unlist()

  num_index <- col_sep %>% as.numeric() %>% is.na() %>% which() %>% suppressWarnings()

  obs_vars <- col_sep[num_index] %>% unique() %>% toupper()

  obs_vars <- obs_vars[-which(obs_vars=="NODEPTH")]

  return_df <- list(data = data, observed_variables = obs_vars)

  if(verbose){
    cat("observed data loaded, following variables detected:", sep = "\n")
    cat(obs_vars, "\n", sep = " ")
  }
  return_df %>% return()
}

#' Load last run. Loads the most recently saved run of the project directory.
#' @param project_path String, path to the project directory / or custom save location.
#' @param custom_directory Logical, Set to TRUE if using custom save location.
#' @param verbose Logical. Prints status reports
#'
#' @export
load_last_run <- function(project_path, verbose = F) {
  cat("not implemented yet", sep = "\n")
  # ISSUE: cannot read creation date of directories
}

#' Reads SWAP output of current path
#' @param project_path String, path to the project directory / or custom save location.
#' @importFrom glue glue
#' @importFrom stringr str_replace str_remove
#' @importFrom tibble tibble
#' @export
read_swap_output <-  function(project_path){

  # TODO: pass list of files to read OR read and return all

  read_path <- glue("{project_path}/rswap")

  result_output <- read.table(
    glue("{read_path}/result_output.csv"),
    comment.char = "*",
    sep = ",",
    dec = ".",
    header = T
  ) %>% tibble()

  result_output$DATETIME <- result_output$DATETIME %>% as.Date()

  colnames(result_output)[1] <- "DATE"

  new_cols <- result_output %>% colnames() %>% str_replace("\\..", "_") %>% str_remove("\\.")
  colnames(result_output) <- new_cols

  result_daily <- read.table(
    glue("{read_path}/result_output_tz.csv"),
    comment.char = "*",
    sep = ",",
    dec = ".",
    header = T
  ) %>% tibble()

  # TODO rename these to be more clear
  r_frame <- list(daily_output = result_daily, custom_depth = result_output)

  r_frame %>% return()
 }

#' Get model performance
#'
#' returns the performance indicator for the chosen variable(s) and measure(s)
#' Supported performance indicators are: NSE, PBIAS, RMSE, RSR
#' Supported variables are those provided in the observed data file
#' Please note, passing multiple variables at differing depths will not work!
#' @param project_path **(REQ)** **(string)**
#'  path to the project directory / or custom save location.
#' @param stat (REQ) (string) either "NSE" "PBIAS" "RMSE" "RSR" or "all" for all
#' @param variable (REQ) (string/vector) variable name (ie. WC, H, TEMP). Use "all" for all available.
#' @param depth (REQ) (numeric/vector) depth of variable value (>0). Must exist in SWAP output. Use "all" for all available.
#' @param observed_file_path (OPT) (string) path to observed file. will default to "./project_path/observed_data.xlsx"
#' @param verbose (OPT) (boolean) print status to console?
#' @importFrom tibble %>%
#' @returns (dataframe) value(s) of performance indicator(s) for given variable(s) and depth(s)
#' @export
get_performance <- function(project_path, stat, variable, depth, observed_file_path = NULL, verbose = F) {

  variable <- variable %>% toupper()

  if(observed_file_path %>% is.null()){
    observed_file_path <- glue("{project_path}/observed_data.xlsx")
  }

  observed_data <- rswap::load_observed(path = observed_file_path, verbose = verbose)

  observed_data_filtered <- filter_observed(observed_data, variable, depth)




}

#' filter observed data
#'
#' by variable and depth
#' @param observed_data observed data as given by load_observed()
#' @param var **OPT** name(s) of the variables you would like to select (string). leave blank for "all"
#' @param depth **OPT** value(s) of the depths you would like to select (numeric). leave blank for "all"
#' @param addtional **OPT** if you would like to select a specific column(s), enter
#' them here as a string/vector.
#'
#' @importFrom dplyr %>% select
#' @importFrom stringr str_remove str_split
#' @importFrom stringi stri_extract_all_regex
#' @returns dataframe consisting of DATE column, and desired observed values
#' @export
filter_observed <- function(observed_data, var = NULL, depth = NULL, addtional = NULL){

  colz <- observed_data$data %>% colnames() %>% toupper()

  find <- stri_extract_all_regex(str = colz, pattern = paste(paste0("OBS",var %>% toupper()), collapse = "|")) %>%
    unlist() %>% is.na()

  # which were matched? index.
  relevant_var_cols <- (find == FALSE) %>% which()

  depths = get_depths(observed_data, var) %>% as.character()

  if(depth %>% is.null() == FALSE){
    depth = depth %>% as.character()
    depths <- depths[which(depths == depth)]
  }

  find2 <- stri_extract_all_regex(str = colz, pattern = paste(depths, collapse = "|")) %>%
    unlist() %>% is.na()
  relevant_depth_cols <- (find2 == FALSE) %>% which()


  # switchboard, determining priorty of union
  if(depth %>% is.null() & var %>% is.null()){

    # if both were left blank, then return all, but only unique
    union <- c(relevant_depth_cols, relevant_var_cols) %>% unique()
  }

  if(depth %>% is.null() == FALSE & var %>% is.null()){
    # if depth was given, but variable was not, return all the depth cols
    union <- relevant_depth_cols
  }

  if(depth %>% is.null() & var %>% is.null() == FALSE){
    # if var was given but not depth, return all the var cols
    union <- relevant_var_cols

  }

  if(depth %>% is.null() == FALSE & var %>% is.null() == FALSE){
    # if both depth and var were given, then
    union <- intersect(relevant_var_cols, relevant_depth_cols)
  }


  perf_mod <- observed_data$data %>% select(DATE, all_of(union), all_of(addtional))

  perf_mod %>% return()
}

#' get_depth
#'
#' extract numeric depth values for given observed variable
#' @param observed_data **REQ** (list) as given by load_observed()
#' @param variable **OPT** (string) variable for which depth levels should be given.
#' If no variable is given, all depths will be returned
#' @returns numeric vector of depths
#' @export
get_depths <- function(observed_data, variable = NULL) {

  splitted <- colnames(observed_data$data) %>% str_remove("obs") %>%
    str_split("_") %>% unlist() %>% toupper()

  char_index <-
    splitted %>% as.numeric %>% is.na() %>% which() %>% suppressWarnings()

  char_index = char_index[-which(splitted[char_index] == "NODEPTH")]

  vars <- splitted[char_index]

  if(variable %>% is.null() == FALSE){
    var_cols <- vars %in% variable %>% which()
  }else{
    var_cols <- vars %in% observed_data$observed_variables %>% which()
  }

  depths <- colnames(observed_data$data)[var_cols] %>% str_split("_") %>% unlist() %>% as.numeric() %>% suppressWarnings()
  depths <- depths[which(depths %>% is.na() == FALSE)] # remove the NA values

  if(length(depths)<1){
    depths = NULL
  }
  return(depths)
}



#' NSE
#'
#' Returns NSE value for a paired observed and modeled vector.
#' #TODO: change this function to accept a named vector, or dataframe.
#' @param obs numeric vector of observed values **REQ**
#' @param mod numeric vector of modeled values **REQ**
#' @return numeric NSE value rounded to 2 decimals
#' @export
NSE <- function(obs, mod) {
    return((1 - (sum((obs - mod) ^ 2, na.rm = T
    ) / sum((obs - mean(obs, na.rm = T)) ^ 2, na.rm = T
    ))) %>% round(x = ., digits = 2))
  }

#' PBIAS
#'
#' Returns PBIAS value for a paired observed and modeled vector.
#' #TODO: change this function to accept a named vector, or dataframe.
#' @param obs numeric vector of observed values **REQ**
#' @param mod numeric vector of modeled values **REQ**
#' @return numeric PBIAS value rounded to 2 decimal
#' @export

PBIAS <-function(obs, mod) {
    return(((sum(obs - mod, na.rm = T) * 100) / sum(obs, na.rm = T)) %>% round(x = ., digits = 2))
  }

#' RMSE
#'
#' Returns RMSE value for a paired observed and modeled vector.
#' #TODO: change this function to accept a named vector, or dataframe.
#' @param obs numeric vector of observed values **REQ**
#' @param mod numeric vector of modeled values **REQ**
#' @return numeric RMSE value rounded to 2 digits
#' @export
RMSE <- function(obs, mod) {
    return(sqrt(sum((obs - mod) ^ 2, na.rm = T) / length(obs)) %>% round(x = ., digits = 2))
  }

#' RSR
#'
#' Returns RSR value for a paired observed and modeled vector.
#' #TODO: change this function to accept a named vector, or dataframe.
#' @param obs numeric vector of observed values **REQ**
#' @param mod numeric vector of modeled values **REQ**
#' @return numeric RSR value rounded to 2 digits
#' @export
RSR <- function(obs, mod) {
    return(RMSE(obs, mod) / sd(obs, na.rm = T) %>% round(x = ., digits = 2))
  }

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

