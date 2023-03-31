# Function(s) relating to running the model

#' Run SWAP
#'
#' This function runs the SWAP model for the given project directory, and
#' optionally, a certain `swap_file`.
#' -  If you desire, you can automatically match the output of the SWAP model to the data provided in the `rswap_observed_data.xlsx` file by setting `autoset_output` to `TRUE`.
#' - `verbose` will print not only model running status, but also what rswap is doing
#' - `timeout` allows you to set the maximum runtime of the model
#'
#' This function does more than simply run the model, it does the following, in this order:
#'  1. Build the rswap directory: `build_rswap_directory()`
#'  2. Parses the main swap file: `parse_swap_file()`
#'  3. Updates several parameters in the main file (such as paths, and output settings): `set_swap_output()`
#'  4. Writes the new SWAP main file: `write_swp_file()`
#'  5. Runs the new SWAP main file.
#'
#' @param project_path path to the project directory (string)
#' @param swap_file name of the *.swp main file (leave blank for "swap.swp")
#' @param autoset_output If set to `TRUE`, rswap will automatically detect
#' your observed data provided in the observed file and match it to the SWAP
#' output. if this is set to `FALSE`, then INLIST csv must be set by the user either
#' manually or with `set_swp_output()` or `change_swap_par()` for several other `rswap` function to work
#' @param verbose print status? (flag)
#' @param timeout number of seconds before run timeout (unlimited by default) (numeric)
#'
#' @returns Returns the status code of the run.
#'
#' @importFrom glue glue
#' @importFrom processx run
#' @importFrom dplyr %>%
#'
#' @export
#'
run_swap <- function(project_path,
                     swap_file = "swap.swp",
                     autoset_output = F,
                     verbose = F,
                     timeout = Inf) {


  # Parse paths
  seperated <- project_path %>% str_split("/") %>% unlist()
  project <- seperated %>% tail(1)
  work_dir <-seperated[1:length(seperated)-1] %>% paste(collapse = "/")
  swap_exe <- work_dir %>% paste(collapse = "/") %>% paste0(.,"/swap.exe")
  swap_file_path <- glue("{project}/rswap/{swap_file}")

  if(file.exists(swap_exe)==FALSE){
   stop(glue("swap.exe must be located in parent directory of {project}!\n Required Path: {swap_exe}"))
  }

  # builds a directory for performing package actions, and returns the path
  rswap_directory <- build_rswap_directory(project_path)

  # reads in the swap parameters and tables
  parse_result <- parse_swp_file(project_path = project_path,
                                 swap_file = swap_file,
                                 verbose = verbose)

  # changes the paths in the swap main file to reflect the temporary location
  params <-
    update_swp_paths(
      project_path = project_path,
      swap_exe = swap_exe,
      parameters = parse_result$parameters,
      verbose =  verbose
    )


  # load observed data
  observed_path <- paste0(rswap_directory, "/rswap_observed_data.xlsx")
  if(file.exists(observed_path) == FALSE){
    warning("Observed file not found!\n",observed_path )
  }

  # routine for automatically setting output (could be improved)
  # TODO move all of this to set_swap_output()
  if (autoset_output) {

    obs <- load_observed(rswap_directory)
    variables <- obs$observed_variables
    depths <- get_depths(data = obs$data) %>% sort()

    # add the critical output params if they are not present.
    if("INLIST_CSV" %in% params$param == FALSE){
      add <- data.frame(param = "INLIST_CSV", value = "", comment = glue("added by rswap on {Sys.time()}"))
      params <- rbind(params, add)
    }

    # add the critical output params if they are not present.
    if("SWCSV_TZ" %in% params$param == FALSE){
      add <- data.frame(param = "SWCSV_TZ", value = "1", comment = glue("added by rswap on {Sys.time()}"))
      params <- rbind(params, add)
    }

    # add the critical output params if they are not present.
    if("INLIST_CSV_TZ" %in% params$param == FALSE){
      add <- data.frame(param = "INLIST_CSV_TZ ", value = "'WC,H,TEMP'", comment = glue("added by rswap on {Sys.time()}"))
      params <- rbind(params, add)
    }else{
      params$value[which(params$param=="INLIST_CSV_TZ")] = "'WC,H,TEMP'"
    }

    # Set output wrapper function
    params <- set_swap_output(params, variables, depths, verbose)

    # print
    if (verbose) {
      cat(
        "\n...autosetting SWAP output to match observed files\n")
    }
  }

  # change console output based on verbose flag
  if (verbose) {
    params <- change_swap_par(params, "SWSCRE", 2)
  }else{
    params <- change_swap_par(params, "SWSCRE", 0)

  }

  # location for where the swap file is to be written
  # change this name to "rswap.swp"?
  outpath <- paste0(rswap_directory, "/", swap_file)

  # Write swap file
  rswap_file <- write_swap_file(
    parameters = params,
    table_path = parse_result$table_path,
    outpath = outpath,
    verbose = verbose
  )

  # run the model
  msg <- run(
    command = "swap.exe",
    wd = work_dir,
    args = swap_file_path,
    error_on_status = F,
    timeout = timeout,
    echo_cmd = verbose,
    echo = verbose
  )

  # TODO expand this
  if (msg$status != "100") {
    warning(glue("SWAP error, code {msg$status}"))
    if (msg$status == "2") {
      warning(glue("SWAP model timed out, with timeout {timeout}"))
    }
  }

  # Move reruns.log and swap.ok to the temp directory.
  reruns <- paste0(work_dir, "/reruns.log")
  if (file.exists(reruns)) {
    file.copy(from  = reruns,
              to = paste0(work_dir, "/", project, "/rswap/reruns.log"))
    file.remove(reruns)
    if (verbose) {
      cat("\n...copying reruns.log to rswap directory\n")
    }
  }
  swap_ok <- paste0(work_dir, "/swap.ok")
  if (file.exists(swap_ok)) {
    file.copy(from  = swap_ok,
              to = paste0(work_dir, "/", project, "/rswap/swap.ok"))
    file.remove(swap_ok)
    if (verbose) {
      cat("\n...copying swap.ok to rswap directory\n")
    }
  }

  # return status of run
  return(msg$status)
}
