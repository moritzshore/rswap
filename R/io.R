# io functions
# TODO error handling for missing data


# TODO: translate stat plot, sensitivity functionaliy, autocal functionality
# Then: the big autocal update: read in every variable with value, and be able to change the values

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

  # remove any files in directory /rswap_saved/ ... this is prone to failure, should make more
  # robust! TODO
  ignore <- file_list %>% grepl(x=., "\\b/rswap_saved/\\b") %>% which()

  if(length(ignore)>0){
    file_list<-file_list[-ignore]
  }

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

  # copy in a template observed excel
  template_observed = system.file("extdata/rswap_observed_data.xlsx", package="rswap")

  # if the template does not yet exist in the project directory, copy it in there
  if("rswap_observed_data.xlsx" %in% list.files(project_path) == FALSE){
    obs_status <- file.copy(from = template_observed, to = paste0(project_path, "/rswap_observed_data.xlsx"))
    cat("copying template sheet 'rswap_observed_data_xlsx' into project directory\n")
    }


  # return the path to the temp directory
  return(temp_directory)
}

#' Changes a SWAP parameter
#'
#' passed a parameter dataframe, changes the right parameter, and returns the
#' dataframe
#'
#' @param param dataframe consisting of the parameter values
#' @param name name of the parameter to change
#' @param value that the parameter should take on
#'
#' @returns modified parameter dataframe
#'
#' @importFrom glue glue
#'
#' @export
change_swap_par <- function(param, name, value){
  value <- glue(value, " ! changed by rswap {Sys.time()}")
  param$value[which(param$param == name)] = value
  return(param)
}

#' Update swp main file paths
#'
#' updates the file paths in the swap main file
#' @param project_path path to the temp directory
#' @param swap_exe path to swap.exe
#' @param parameters SWAP main file parameters in dataframe format
#' @param verbose print status?
#' @importFrom glue glue
#' @importFrom dplyr %>%
#' @keywords internal
#' @returns swap parameter dataframe
#'
update_swp_paths <-
  function(project_path,
           swap_exe,
           parameters,
           verbose) {

    rswap_dir <- project_path %>% paste0(.,"/rswap/")

    # TODO this could be revamped
    swap_exe_name = swap_exe %>% str_split("/") %>% unlist() %>% tail(n=1)
    path_without_swap <-  swap_exe %>% str_remove(swap_exe_name)

    swap_main_file_path <- rswap_dir %>% str_remove(path_without_swap)

    update_par <- c("PATHWORK","PATHATM", "PATHCROP", "PATHDRAIN")

  for (par in update_par) {
    val = glue("'{swap_main_file_path}'")
    parameters = change_swap_par(parameters, par, val )
  }

  return(parameters)
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
  dir.create(glue("{to_path}/tables/"), showWarnings = F)


  from_path = glue("{project_path}/rswap/")
  from_copy = list.files(from_path, full.names = T, include.dirs = T, recursive = T)

  to_files = list.files(from_path, full.names = F, include.dirs = T, recursive = T)
  to_copy = glue("{to_path}/{to_files}")

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

  # skip 1 to remove the comment line
  data <- read_excel(path, skip = 1)

  data$DATE <- data$DATE %>% as.Date()

  columns <- colnames(data)

  # find a better way to do this. or remove it
  date_col <- columns %>% grepl(x = ., "DATE") %>% which()

  obs_cols <- columns[-date_col]

  col_sep <- obs_cols %>% str_split("_") %>% unlist()

  num_index <- col_sep %>% as.numeric() %>% is.na() %>% which() %>% suppressWarnings()

  obs_vars <- col_sep[num_index] %>% unique() %>% toupper()

  return_df <- list(data = data, observed_variables = obs_vars)

  if(verbose){
    cat("observed data loaded, following variables detected:", sep = "\n")
    cat(obs_vars, "\n", sep = " ")
  }
  return_df %>% return()
}

#' Reads SWAP output of current path
#' @param project_path String, path to the project directory / or custom save location.
#' @importFrom glue glue
#' @importFrom stringr str_replace str_remove
#' @importFrom tibble tibble
#' @export
read_swap_output <-  function(project_path, custom_path = F){

  # TODO: pass list of files to read OR read and return all
  if(custom_path == FALSE){
    read_path <- glue("{project_path}/rswap")
  }else{
    read_path = project_path
  }

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

#' Filter swap data
#'
#' by variable and depth
#' @param data observed/modelled data as given by load_observed()$data or read_swap_output()$custom_depth
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
filter_swap_data <- function(data, var = NULL, depth = NULL, addtional = NULL){

  if(var %>% is.null() == FALSE){
    var <- var %>% toupper()
  }

  colz <- data %>% colnames() %>% toupper()

  if(var %>% is.null() == FALSE){
    find <- stri_extract_all_regex(str = colz, pattern = paste(var, collapse = "|")) %>%
      unlist() %>% is.na()
    # which were matched? index.
    relevant_var_cols <- (find == FALSE) %>% which()

  }else{relevant_var_cols=NULL}


  depths = get_depths(data = data, var) %>% as.character()

  if (depth %>% is.null() == FALSE) {
    depth = depth %>% as.character()
    depths <- depths[which(depth %in% depths)]
    find2 <-
      stri_extract_all_regex(str = colz, pattern = paste(depths, collapse = "|")) %>%
      unlist() %>% is.na()
    relevant_depth_cols <- (find2 == FALSE) %>% which()

    if(relevant_depth_cols %>% length() == 0){stop("Error: no matching column found")}

  } else{
    relevant_depth_cols = NULL
  }

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


  perf_mod <- data %>% select(DATE, all_of(union), all_of(addtional))

  perf_mod %>% return()
}

#' get_depth
#'
#' extract numeric depth values for given observed variable
#' @param data **REQ** (list) as given by load_observed()
#' @param variable **OPT** (string) variable for which depth levels should be given.
#' If no variable is given, all depths will be returned
#' @returns numeric vector of depths
#' @export
get_depths <- function(data, variable = NULL) {

  splitted <- colnames(data) %>% str_remove("obs") %>%
    str_split("_") %>% unlist() %>% toupper()

  char_index <-
    splitted %>% as.numeric %>% is.na() %>% which() %>% suppressWarnings()


  vars <- splitted[char_index]

  if(variable %>% is.null() == FALSE){
    var_cols <- vars %in% variable %>% which()
  }else{
    all_vars <- vars %>% unique()
    all_vars <- all_vars[-which(all_vars == "DATE")]
    var_cols <- vars %in% all_vars %>% which()
  }

  depths <- colnames(data)[var_cols] %>% str_split("_") %>% unlist() %>% as.numeric() %>% suppressWarnings()
  depths <- depths[which(depths %>% is.na() == FALSE)] # remove the NA values

  depths <- depths %>% unique()

  if(length(depths)<1){
    depths = NULL
  }
  return(depths)
}

#' match mod obs
#'
#' internal function: matches observed and modelled dataframes and returns them
#'
#' @param project_path req
#' @param variable req
#' @param observed_file_path req
#' @param depth req
#' @param verbose req
#' @param addtional req
#' @param custom_path in case project path is not default
#'
#' @importFrom tibble %>%
#' @importFrom glue glue
#' @returns list of modelled DF and observed DF, matched
#' @keywords internal
#' @export
match_mod_obs <-
  function(project_path,
           variable,
           observed_file_path,
           depth = NULL,
           verbose = F,
           addtional = NULL,
           custom_path = F) {

  # todo remove export from this function
  if (variable %>% is.null() == FALSE) {
    variable <- variable %>% toupper()
  }

  observed_data <- load_observed(path = observed_file_path, verbose = verbose)
  modelled_data <- read_swap_output(project_path = project_path, custom_path = custom_path)

  observed_data_filtered <-
    filter_swap_data(
      data = observed_data$data,
      var = variable,
      depth = depth,
      addtional = addtional
    )
  modelled_data_filtered <-
    filter_swap_data(
      data = modelled_data$custom_depth,
      var = variable,
      depth = depth,
      addtional = addtional
    )

  # sort them to be in consistent order
  obs_new_order = observed_data_filtered %>% colnames() %>% sort()
  mod_new_order = modelled_data_filtered %>% colnames() %>% sort()

  modelled_data_filtered = modelled_data_filtered[, mod_new_order]

  observed_data_filtered = observed_data_filtered[, obs_new_order]

  obs_col_length = length(observed_data_filtered)
  mod_col_length = modelled_data_filtered %>% length()

  if (obs_col_length != mod_col_length) {
    print("OBS file:")
    print(observed_data_filtered)
    print("mod file:")
    print(modelled_data_filtered)
    stop(
      "Illegal request: obs col length and mod col length differ, likely because observed data attributes does not match modelled"
    )
  }

  if (obs_col_length == 1) {
    warning("No variable selected, returning empty dataframe. ")
    return(observed_data_filtered)
  }
  list(mod = modelled_data_filtered, obs = observed_data_filtered) %>% return()
}

#' Melt all runs
#'
#' Combines the past saved runs, with the current run, and the observed data
#' so that the combination of them is easy to plot using ggplot etc.
#'
#' @param project_path path to project directory
#' @param custom_save_path (OPT) (string) path to the custom save location.
#' leave blank for default
#' @param observed_file_path (OPT) (string) path to custom observed data location.
#' leave blank for default
#' @param variable (REQ) (string) variable to be returned
#' @param depth (OPT) (string) depth of variable. leave blank if variable has
#' no depth
#' @param verbose print status?
#'
#' @importFrom dplyr %>% select
#' @importFrom stringr str_remove_all str_replace_all str_split
#' @importFrom glue glue
#' @importFrom purrr map map_df is_empty
#' @importFrom vroom vroom
#'
#' @returns dataframe with columns "run" "DATE" "tag" "variable" "value"
#'
#' @export
melt_all_runs <-
  function(project_path,
           custom_save_path = NULL,
           observed_file_path = NULL,
           variable,
           depth = NULL,
           verbose = F) {


    if (observed_file_path %>% is.null()) {
      observed_file_path <- glue("{project_path}/rswap_observed_data.xlsx")
    }

    if(custom_save_path %>% is.null()){
      file_path =  paste0(project_path, "/rswap_saved/")
    }else{
      file_path = custom_save_path
    }

    past_run_names <-  list.files(path = file_path)
    past_run_paths <-  list.files(path = file_path, full.names = T)

    if(past_run_names %>% is_empty()){warning("no previous runs to melt!");return(NA)}


    result_files <- paste0(past_run_paths, "/result_output.csv")
    past_run_df <- result_files %>% map_df(., ~ vroom(.x, id = "path", show_col_types = F, delim = ",", comment = "*"))
    new_col_names <- past_run_df %>% colnames() %>% str_remove_all("]") %>% str_remove_all("\\[") %>% str_replace_all("-", "_")
    new_col_names<-new_col_names %>% str_replace_all("DATETIME", "DATE")
    new_col_names<-new_col_names %>% str_replace_all("path", "RUN")
    colnames(past_run_df) <- new_col_names
    run_names <- past_run_df$RUN %>% str_split("/") %>% map(., 7) %>% unlist()
    past_run_df<-past_run_df %>% select(-RUN)

    # todo: fix rain/drain overlap!
    past_run_df<-rswap::filter_swap_data(past_run_df, var = variable, depth = depth)
    past_run_df$tag = "past"
    past_run_df$run = run_names

    present_run_df <- rswap::read_swap_output(project_path)
    present_run_df<-rswap::filter_swap_data(present_run_df$custom_depth, var = variable, depth = depth)
    present_run_df$tag = "present"
    present_run_df$run = "current run"

    observed_data <- load_observed(path = observed_file_path, verbose = verbose)
    observed_data<-rswap::filter_swap_data(observed_data$data, var = variable, depth = depth)
    observed_data$tag = "observed"
    observed_data$run = "observed"

    full_df <- rbind(past_run_df, present_run_df, observed_data)
    full_df$variable = colnames(full_df)[2]
    colnames(full_df)[2] <- "value"

    return(full_df)
  }
