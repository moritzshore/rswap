# run functions

#' Runs the SWAP model
#'
#' @param project_path String, path to the project directory.
#' @param string name of the *.swp main file
#' @param autoset_output If set to true, rswap will automatically detect
#' your observed data provided in the observed file and match it to the SWAP
#' output. if this is set to false, then INLIST csv must be set by the user either
#' manually or with set_swap_output() or change_swap_par()
#' @param verbose logical, print status?
#' @param quiet logical, turn off warnings?
#' @param timeout number of seconds before run timeout (optional, unlimited by default)
#'
#' @return returns name of run (change this!)
#'
#' @importFrom glue glue
#' @importFrom processx run
#' @importFrom dplyr %>%
#' @export
#'
run_swap <- function(project_path,
                     swap_file = "swap.swp",
                     autoset_output = F,
                     verbose = F,
                     timeout = Inf,
                     quiet = F) {


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
                                 verbose = verbose,
                                 quiet = quiet)

  # changes the paths in the swap main file to reflect the temporary location
  params <-
    update_swp_paths(
      project_path = project_path,
      swap_exe = swap_exe,
      parameters = parse_result$parameters,
      verbose =  verbose
    )


  # load observed data
  observed_path <- paste0(rswap_directory, "rswap_observed_data.xlsx")
  if(file.exists(observed_path) == FALSE){
    warning("Observed file not found!\n",observed_path )
  }

  # routine for automatically setting output (could be improved)
  if (autoset_output) {

    obs <- load_observed(project_path)
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
  outpath <- paste0(rswap_directory, swap_file)

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
