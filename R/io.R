# input/output functions

#' Build rswap Directory
#'
#' Makes a temporary sub-directory workspace for the package to run SWAP in.
#'
#' @param project_path path to project directory (string)
#'
#' @importFrom glue glue
#' @importFrom dplyr %>%
#'
#' @keywords internal
#'
build_rswap_directory <- function(project_path){

  temp_directory <- glue("{project_path}/rswap")

  # list files found in project path (Note, this includes all the saved runs as\
  # well, which we dont want, which is why we need to remove them)
  file_list <- list.files(project_path, full.names = T, recursive = T)
  ignore <- file_list %>% grepl(x=., "\\b/rswap_saved/\\b") %>% which()
  # only remove files if they actually need to be, because if you do this
  # with an empty vector, it will delete ALL the files
  if(length(ignore)>0){
    file_list<-file_list[-ignore]
  }

  # Vector of all the SWAP file types that will be copied into the temp directory
  # It might be a good idea to NOT do this, and just copy every file, incase this
  # vector does not cover every possible filetype...
  file_types <-
    c("*.crp",
      "*.met",
      "*.swp",
      "*.dra",
      "layer*n.csv",
      "*.bbc",
      "*.csv",
      "*.irg",
      "*.ini")
  match_string <- paste(file_types, collapse = "|")
  required_files <- file_list %>% grepl(x = ., match_string) %>% which()
  required_file_list <- file_list[required_files]

  # create the hidden temp directory
  dir.create(temp_directory, showWarnings = F)
  # and copy over the desired files
  status <- file.copy(from = required_file_list, to = temp_directory)

  # legacy support for old met files:
  # copies over any files with a "numeric" file type. (best way i could think of)
  not_numeric <- str_split(file_list, "[.]", simplify = T)[,2] %>%
    as.numeric() %>% is.na() %>% suppressWarnings()
  met_files <- file_list[which(not_numeric == FALSE)]
  met_status <- file.copy(from = met_files, to = temp_directory)

  # if the template does not yet exist in the project directory, copy it in there
  if("rswap_observed_data.csv" %in% list.files(project_path) == FALSE){
    template_observed = system.file("extdata/rswap_observed_data.csv", package="rswap")
    template_instructions = system.file("extdata/instructions_rswap_observed_data.txt", package="rswap")

    obs_status <- file.copy(from = template_observed, to = paste0(project_path, "/rswap_observed_data.csv"))
    obs_status <- file.copy(from = template_instructions, to = paste0(project_path, "/instructions_rswap_observed_data.txt"))

    cat("copying template sheet 'rswap_observed_data.csv' into project directory\n ...along with 'instructions_rswap_observed_data.txt'\n")
    }
  # return the path to the temp directory
  return(temp_directory)
}

#' Update SWAP Main File Paths
#'
#' As `rswap` creates a new directory for the model file, the paths in the those
#' files need to be update to still be correct. This function does just that.
#'
#'
#' @param project_path path to the temp directory (string)
#' @param swap_exe path to swap.exe (string)
#' @param parameters SWAP main file parameters (string)
#' @param verbose print status? (flag)
#'
#' @importFrom glue glue
#' @importFrom dplyr %>%
#'
#' @keywords internal
#'
#' @returns Returns the SWAP parameter dataframe with modified path values
#'
update_swp_paths <- function(project_path, swap_exe,
                             parameters, verbose) {

    version <- packageVersion("rswap") %>% as.character() %>% enc2utf8()

    # parse the various paths
    rswap_dir <- project_path %>% paste0(.,"/rswap/")
    swap_exe_name <- swap_exe %>% str_split("/") %>% unlist() %>% tail(n=1)
    path_without_swap <-  swap_exe %>% str_remove(swap_exe_name)
    swap_main_file_path <- rswap_dir %>% str_remove(path_without_swap)

    # SWCSV needs to be present in the SWAP main file, such that the needed
    # output can be printed. If this parameter already exists within the
    # parameter dataframe, we can simply adjust the value to 1. If it does
    # not exist yet, we need to add it, with the value of 1.
    if ("SWCSV" %in% parameters$param) {
      parameters = change_swap_par(parameters, "SWCSV", "1")
    } else{
      rbind(parameters,
            data.frame(
              param = "SWCSV",
              value = "1",
              comment = glue("added by rswap v{version} @ {Sys.time()}")
            ))
    }
    # The exact same thing goes for SWCSV...
    if ("SWCSV_TZ" %in% parameters$param) {
      parameters = change_swap_par(parameters, "SWCSV_TZ", "1")
    } else{
      rbind(parameters,
            data.frame(
              param = "SWCSV_TZ",
              value = "1",
              comment = glue("added by rswap v{version} {Sys.time()}")
            ))
    }
    # when more output is needed, I will need to add more and more, so this
    # should / will be updated to do as a loop, as it is done below:

    # these are the parameters that need a path to be updated
    update_par <- c("PATHWORK","PATHATM", "PATHCROP", "PATHDRAIN")
    for (par in update_par) {
      val = glue("'{swap_main_file_path}'")
      parameters = change_swap_par(parameters, par, val)
    }

    # Update the SWINCO path if needed. If SWINCO is set to 3, then INIFIL needs
    # to have its path updated.
    # This code is ugly, and can be simplified, but it does work.
    swinco_index <- (parameters$param == "SWINCO") %>% which()
    if ((swinco_index %>% length()) > 0) {
      if (parameters$value[swinco_index] == 3) {
        infil_index <- (parameters$param == "INIFIL") %>% which()
        if ((infil_index %>% length()) > 0) {
          val <-  parameters$value[infil_index] %>% str_remove_all("'")
          newval <- glue("'{swap_main_file_path}{val}' ! Changed by rswap v{version} @ {Sys.time()}")
          parameters = change_swap_par(parameters, "INIFIL", newval )
          if(verbose){
            cat(glue("\n...INIFIL parameter set to\n {parameters$value[infil_index]}\n"))
          }
        }
      }
    }
    # Return the modified parameter set with the updated paths
    return(parameters)
  }

#' Save a SWAP run
#'
#' If you would like to keep the results your last SWAP model run, and/or the
#' setup itself, you should use this function. You need to pass it the
#' `project_path` of the last run project. To give your saved setup a name, pass
#' one via the `run_name` parameter.
#'
#' Once a run has been saved, it can be compared to other saved runs, and/or the
#' next run you perform. This can be done with functions such as
#' `comparative_plot()` or `plot_statistics()`
#'
#' @param project_path path to the project directory (string)
#' @param run_name name of run to be saved. default is "rswap_{time,date}" (string)
#' @param verbose print status? (flag)
#'
#' @importFrom glue glue
#' @importFrom dplyr %>%
#' @importFrom stringr str_replace_all
#'
#' @export
#'
save_run <- function(project_path, run_name = NULL, verbose = F){
  # TODO change the name of this function to save_swap_run()

  # if the run name is not passed, then assign a run name based on the current
  # time and date.
  if(run_name %>% is.null()){
    tad = Sys.time() %>%  str_replace_all(":", "_") %>% str_replace_all(" ","at")
    run_name = glue("rswap_{tad}")
  }

  # create the save folder of ALL the saves, if not already present.
  save_location = glue("{project_path}/rswap_saved")
  dir.create(save_location, showWarnings = F)

  # create the save folder for the individual run
  to_path = glue("{save_location}/{run_name}")
  # leave warnings on, to notify user that they are overwriting.
  # TODO: include a safety for overwriting folders here?
  dir.create(to_path, showWarnings = T)
  dir.create(glue("{to_path}/tables/"), showWarnings = F)

  # copy over the files.
  from_path = glue("{project_path}/rswap/")
  from_copy = list.files(from_path, full.names = T, include.dirs = F, recursive = F)
  to_files = list.files(from_path, full.names = F, include.dirs = F, recursive = F)
  to_copy = to_path
  status = file.copy(from_copy, to_copy, overwrite = T, recursive = T, copy.mode = T, copy.date = T)

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
  #TODO return path of saved run?
}

#' Load Observed Data
#'
#' This function loads your observed data from the template file that was placed
#' in your project directory either by `build_rswap_directory()` or `rswap_init()`
#'
#' It is critical that the template observed file is filled out correctly.
#' Please see the file itself for more information. It should be located in
#' your project directory, and must bear the namer `swap_observed_data.csv`.
#'
#' Please note, the file type will eventually be switched to .csv.
#'
#' @param project_path Path to project directory (string)
#' @param archived set to true if project path in saved in 'rswap_saved' (flag)
#' @param verbose print status? (flag)
#'
#' @importFrom stringr str_remove str_replace str_split
#' @importFrom tibble tibble
#'
#' @returns Returns a list consiting of `.$data`, a dataframe of the observed values,
#' as well as `.$observed_variables`, a vector of the detected observed variables.
#'
#' @export
#'
#'
load_observed <- function(project_path, archived = F, verbose = F){

  #TODO switch to CSV
  #TODO rename to load_swap_observed
  if(archived){
    path <- paste0(project_path, "/rswap_observed_data.csv")
  }else{
    path <- paste0(project_path, "/rswap/rswap_observed_data.csv")

  }
  # SUPPORTED FORMAT: sep=; dec=,
  data <- read.table(file = path, header = T, sep = ";", dec = ",") %>% tibble()
  data$DATE <- data$DATE %>% as.Date() # force date format
  columns <- colnames(data)

  date_col <- columns %>% grepl(x = ., "DATE") %>% which()
  obs_cols <- columns[-date_col]

  col_sep <- obs_cols %>% str_split("_") %>% unlist()
  num_index <- col_sep %>% as.numeric() %>% is.na() %>% which() %>% suppressWarnings()

  # parse the observed varibles names
  obs_vars <- col_sep[num_index] %>% unique() %>% toupper()

  return_df <- list(data = data, observed_variables = obs_vars)

  if(verbose){
    cat("observed data loaded, following variables detected:", sep = "\n")
    cat(obs_vars, "\n", sep = " ")
  }

  return_df %>% return()
}

#' Reads SWAP Model Output
#'
#' This function reads the output of the SWAP model for the last run of the
#' given `project_path`. It can also be used to read the results of saved runs
#' in the "/rswap_saved" folder if `archived` is set to `TRUE`.
#'
#' Currently, the function only returns the data needed by the rest of the
#' package, but this will be expanded over time to return all SWAP output
#'
#' @param project_path path to project directory (string)
#' @param archived needs to be set to true when reading from the rswap_saved archive (flag)
#'
#' @importFrom glue glue
#' @importFrom stringr str_replace str_remove
#' @importFrom tibble tibble
#'
#' @returns Returns a list of dataframes. `.$daily_output` and `.$custom_depth`
#'
#' @export
#'
read_swap_output <-  function(project_path, archived = F){

  #TODO rewrite to return ALL SWAP output.

  if(archived){
    path <- glue("{project_path}/result_output.csv")
  }else{
    path <- glue("{project_path}/rswap/result_output.csv")
  }

  if(archived){
    path_tz <- glue("{project_path}/result_output_tz.csv")
  }else{
    path_tz <- glue("{project_path}/rswap/result_output_tz.csv")
  }

  result_output <- read.table(
    path,
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
    path_tz,
    comment.char = "*",
    sep = ",",
    dec = ".",
    header = T
  ) %>% tibble()

  # TODO rename these to be more clear?
  r_frame <- list(daily_output = result_daily, custom_depth = result_output)

  r_frame %>% return()
  }

#' Filter SWAP Data
#'
#' This is an internal function which filters dataframes of SWAP data by depth
#' and variable. It might be useful for end users as well, which is why its
#' accessible.
#'
#' @param data as given by `load_observed()$data` or `read_swap_output()$custom_depth` (dataframe)
#' @param var name(s) of the variables you would like to select, leave blank for all. (string)
#' @param depth value(s) of the depths you would like to select, leave blank for all. (numeric)
#'
#' @returns Returns a dataframe with a `DATE` column, followed by the desired data.
#'
#' @importFrom dplyr %>% select
#' @importFrom stringr str_remove str_split
#' @importFrom stringi stri_extract_all_regex
#'
#' @export
#'
filter_swap_data <- function(data, var = NULL, depth = NULL){

  if(var %>% is.null() == FALSE){
    var <- var %>% toupper()
  }

  colz <- data %>% colnames() %>% toupper()

  # need to find the desired var columns, if a var was passed
  if (var %>% is.null() == FALSE) {
    # using this function because grepl() cannot handle more than one pattern.
    find <- stri_extract_all_regex(str = colz, pattern = paste(var, collapse = "|")) %>%
      unlist() %>% is.na()
    relevant_var_cols <- (find == FALSE) %>% which()
  } else{
    relevant_var_cols = NULL
  }

  # need to find the desired depth columns, if depth was passed
  if (depth %>% is.null() == FALSE) {
    depth = depth %>% as.character()
    find2 <- stri_extract_all_regex(str = colz, pattern = paste(depth, collapse = "|")) %>%
      unlist() %>% is.na()
    relevant_depth_cols <- (find2 == FALSE) %>% which()
    if (relevant_depth_cols %>% length() == 0) {
      stop("Error: no matching column found")
    }
  } else{
    relevant_depth_cols = NULL
  }

  # switchboard, determining priority of union of depth and var

  # if both were left blank, then return all, but only unique
  if(depth %>% is.null() & var %>% is.null()){
    union <- c(relevant_depth_cols, relevant_var_cols) %>% unique()
  }

  # if depth was given, but variable was not, return all the depth cols
  if(depth %>% is.null() == FALSE & var %>% is.null()){
    union <- relevant_depth_cols
  }

  # if var was given but not depth, return all the var cols
  if(depth %>% is.null() & var %>% is.null() == FALSE){
    union <- relevant_var_cols
  }

  # if both depth and var were given, then return their intersection
  if(depth %>% is.null() == FALSE & var %>% is.null() == FALSE){
    union <- intersect(relevant_var_cols, relevant_depth_cols)
  }

  # build the filtered DF and return it.
  perf_mod <- data %>% select(DATE, all_of(union))
  perf_mod %>% return()
}

#' Get Depths
#'
#' Extracts the numeric depth values found in the observed data, as loaded by
#' `load_observed()`. Can be filtered by `variable`.
#'
#' @param data as loaded by `load_observed()` (dataframe)
#' @param variable only returns depths of the given variable when passed. leave blank for all. (string)
#'
#' @returns Returns a numeric vector of depths
#'
#' @export
get_depths <- function(data, variable = NULL) {

  # TODO: rename to get_swap_depths
  splitted <- colnames(data) %>% str_remove("obs") %>%
    str_split("_") %>% unlist() %>% toupper()

  char_index <- splitted %>% as.numeric %>% is.na() %>% which() %>% suppressWarnings()

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

#' Match Modeled Values To Observed Values
#'
#' This function is used internally to match observed values with modeled values
#' in the same format, such that it is easy to pass them to performance indicators.
#'
#' This function is available since it might be of use to the end user.
#'
#' @param project_path path to project directory (string)
#' @param variable variable to match (string)
#' @param depth an optional depth to select for (numeric)
#' @param verbose print status? (boolean)
#' @param archived is the project in /rswap_saved/?
#'
#' @importFrom tibble %>%
#' @importFrom glue glue
#'
#' @returns Returns a list of two dataframes, `.$mod` and `.$obs`. These two dataframes have identical dimensions and column names.
#'
#' @export
#'
match_mod_obs <- function(project_path, variable, depth = NULL,
                          verbose = F, archived = F) {

    # TODO rename: match_swap_data

  if (variable %>% is.null() == FALSE) {
    variable <- variable %>% toupper()
  }

  if(archived){
    project_path<-project_path %>% str_remove("/rswap/")
  }

  observed_data <- load_observed(project_path = project_path, verbose = verbose, archived = archived)
  modelled_data <- read_swap_output(project_path = project_path, archived = archived)

  observed_data_filtered <-filter_swap_data(data = observed_data$data,
                                            var = variable, depth = depth)

  modelled_data_filtered <-filter_swap_data(data = modelled_data$custom_depth,
                                            var = variable, depth = depth)

  # sort them to be in consistent order
  obs_new_order = observed_data_filtered %>% colnames() %>% sort()
  mod_new_order = modelled_data_filtered %>% colnames() %>% sort()

  modelled_data_filtered = modelled_data_filtered[, mod_new_order]
  observed_data_filtered = observed_data_filtered[, obs_new_order]

  obs_col_length = length(observed_data_filtered)
  mod_col_length = modelled_data_filtered %>% length()

  if (obs_col_length != mod_col_length) {
    stop("Illegal request: obs col length and mod col length differ, likely because observed data attributes does not match modelled",
         "\nOBS file selected columns --> ",
         observed_data_filtered %>% length(),
         "\nMOD file selected columns --> ",
         modelled_data_filtered %>% length())
  }

  if (obs_col_length == 1) {
    warning("No variable selected, returning an empty dataframe.")
    return(observed_data_filtered)
  }

  list(mod = modelled_data_filtered, obs = observed_data_filtered) %>% return()
}

#' Melt All Runs
#'
#' This function combines the modeled and observed data of the current run,
#' along with all the modeled and observed data of all the saved runs
#' (for a single variable) and returns them in tidy format, easy to use for
#' plotting, etc.
#'
#' This is once again an internal function that is made available due to it
#' possibly being useful for the end user.
#'
#' @param project_path path to project directory (string)
#' @param variable variable to be used (string)
#' @param depth optional depth to filter by, leave blank for all/no depth (numeric)
#' @param verbose print status? (flag)
#'
#' @importFrom dplyr %>% select
#' @importFrom stringr str_remove_all str_replace_all str_split
#' @importFrom glue glue
#' @importFrom purrr map map_df is_empty
#' @importFrom vroom vroom
#'
#' @returns Returns dataframe with columns "run" "DATE" "tag" "variable" "value"
#'
#' @export
#'
melt_all_runs <-
  function(project_path,
           variable,
           depth = NULL,
           verbose = F) {

    observed_file_path <- glue("{project_path}/rswap_observed_data.csv")
    file_path <-  paste0(project_path, "/rswap_saved/")
    past_run_names <- list.files(path = file_path)
    past_run_paths <- list.files(path = file_path, full.names = T)

    if (past_run_names %>% is_empty()) {
      warning("no previous runs to melt!")
      return(NULL)
    }

    result_files <- paste0(past_run_paths, "/result_output.csv")
    past_run_df <- result_files %>% map_df(., ~ vroom(.x, id = "path", show_col_types = F, delim = ",", comment = "*"))
    new_col_names <- past_run_df %>% colnames() %>% str_remove_all("]") %>% str_remove_all("\\[") %>% str_replace_all("-", "_")
    new_col_names <- new_col_names %>% str_replace_all("DATETIME", "DATE")
    new_col_names <- new_col_names %>% str_replace_all("path", "RUN")
    colnames(past_run_df) <- new_col_names
    run_names <- past_run_df$RUN %>% str_remove(project_path) %>% str_remove("/rswap_saved/") %>% str_remove("/result_output.csv")
    past_run_df <- past_run_df %>% select(-RUN)

    # We remove RAIN, because it overlaps with "DRAINAGE" but this needs to be
    # fixed properly.. somewhere I need to add the '\bVARNAME\b" flags to fix this
    # TODO!
    past_run_df <- past_run_df %>% select(-RAIN)
    past_run_df <- filter_swap_data(past_run_df, var = variable, depth = depth)

    if(length(past_run_df) == 1){
      stop("something went wrong... no data was selected in:\n",
           "filter_swap_data(past_run_df, var = variable, depth = depth)")
    }

    past_run_df$tag <- "past"
    past_run_df$run <- run_names
    present_run_df <- read_swap_output(project_path)
    present_run_df <- filter_swap_data(present_run_df$custom_depth, var = variable, depth = depth)
    present_run_df$tag = "present"
    present_run_df$run = "current run"
    observed_data <- load_observed(project_path, verbose = verbose)
    observed_data <- filter_swap_data(observed_data$data, var = variable, depth = depth)
    observed_data$tag = "observed"
    observed_data$run = "observed"
    full_df <- rbind(past_run_df, present_run_df, observed_data)
    full_df$variable = colnames(full_df)[2]
    colnames(full_df)[2] <- "value"

    return(full_df)
  }

#' Initialize rswap
#'
#' This function is an optional starting point when using rswap for the first
#' time. What it does is copy in the example swap setup "hupselbrook" into a
#' sample project directory "rswap_init" within the same working directory as
#' your swap.exe executable (hence the need to pass the path to the .exe).
#'
#' The sample directory contains the SWAP input files, as well as a template for
#' observed values. This provides a good starting point for setting up your own
#' rswap project. Just changes the name of the directory, fill in the observed
#' file and start modifying parameters.
#'
#' If this function does not successfully complete, then there is an underlying
#' issue you need to fix before continuing to use rswap.
#'
#' @param swap_exe path to swap.exe (string)
#'
#' @returns Returns the project path of "hupselbrook" SWAP setup.
#'
#' @importFrom stringr str_split str_remove
#' @importFrom dplyr %>%
#' @export
#'
rswap_init <- function(swap_exe){

  pkg_path <- system.file(package = "rswap")
  extdata <- paste0(pkg_path, '/extdata')

  exe_name <-swap_exe %>% str_split("/") %>% unlist() %>% tail(1)
  wd <- swap_exe %>% str_remove(exe_name)

  example_path <- paste0(wd, "hupselbrook")
  unlink(example_path, recursive = T)
  dir.create(example_path, showWarnings = F)

  from_path = list.files(extdata, full.names = T)
  to_path <- from_path %>% str_remove(extdata)
  file.copy(from = from_path, to = paste0(example_path, to_path))

  status = run_swap(example_path, verbose = T, autoset_output = T)

  if(status == 100){
    cat("\nrswap ran successfully!\n")
  }else{
    cat("\nOh no! something went wrong with the running rswap!\n")
    cat("\nERROR CODE:", status, "\n")
  }

  data <- load_observed(example_path, verbose = T)
  mod <- rswap::read_swap_output(example_path)

  if(mod[[1]] %>% is.data.frame()){
    cat("\nloading SWAP output... success!\n")
  }else{
    stop("something went wrong loading the modelled data")
  }

  variable <- data$observed_variables[1:3]
  depth <- get_depths(data = data$data)[1]
  soft_calibration_plot(project_path = example_path, vars = variable, show = c("RAIN", variable[1]))
  cat("\nif you can see the plotly plot, then rswap is plotting successfully\n")
  cat("\nrswap initilization complete, you can find the project folder here:\n")
  cat(example_path)
  return(example_path)
}
