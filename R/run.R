# Function(s) relating to running the model

#' Run SWAP
#'
#' This function runs the SWAP model for the given project directory, and
#' optionally, a certain `swap_file`.
#' -  If you desire, you can automatically match the output of the SWAP model to the data provided in the `rswap_observed_data.csv` file by setting `autoset_output` to `TRUE`.
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
#' @param force If an rswap directory already exists, no new one will be generated unless `force=TRUE`
#' @param verbose print status? (flag)
#' @param timeout number of seconds before run timeout (unlimited by default) (numeric)
#'
#' @returns Returns the status code of the run.
#'
#' @importFrom glue glue
#' @importFrom processx run
#' @importFrom dplyr %>%
#' @importFrom crayon blue magenta bgYellow bold yellow
#'
#' @export
#'
run_swap <- function(project_path,
                     swap_file = "swap.swp",
                     autoset_output = F,
                     force = F,
                     verbose = F,
                     timeout = Inf) {

  # IO timer start
  io_start <- Sys.time()

  # parse the paths needed for running SWAP and updating file paths
  swap_run_paths <- parse_run_paths(project_path = project_path,
                                    verbose = verbose,
                                    swap_file = swap_file)

  # make sure SWAP is in the right place
  check_exe_location(swap_exe = swap_run_paths$swap_exe, verbose = verbose)

  # builds a directory for performing package actions, and returns the path
  rswap_directory <- build_rswap_directory(project_path = project_path,
                                           force = force,
                                           verbose = verbose)

  # reads in the swap parameters and tables
  parse_result <- parse_swap_file(project_path = project_path,
                                 swap_file = swap_file,
                                 verbose = verbose)

  # load in the parameters to be altered
  parameters <- load_swap_parameters(project_path = project_path,
                         swap_file = swap_file,
                         verbose = verbose)

  # changes the paths in the swap main file to reflect the temporary location
  parameters <- update_swap_paths(project_path = project_path,
                                 swap_exe = swap_run_paths$swap_exe,
                                 parameters = parameters,
                                 verbose = verbose)

  # set the output of SWAP
  parameters <- set_swap_output(project_path = project_path,
                               parameters = parameters,
                               autoset_output = autoset_output,
                               verbose = verbose)

  # write the modified parameter set
  write_swap_parameters(project_path = project_path,
                        parameters = parameters,
                        verbose = verbose)

  # Write swap file
  write_swap_file(project_path = project_path,
                  outfile = swap_run_paths$outpath,
                  verbose = verbose)

  # pause the IO timer during the model run
  io_end <- Sys.time()

  # run the actual model
  run_result <- run_swap_base(verbose = verbose,
                swap_run_paths = swap_run_paths,
                timeout = timeout)

  # resume the io timer
  io_start2 <- Sys.time()

  # check SWAP message
  check_swap_message(run_result$status, verbose)

  # move run files to temp dir
  move_run_files(swap_run_paths$work_dir, swap_run_paths$project, verbose)

  # end timer for IO operations
  io_end2 <- Sys.time()

  # report run time
  if (verbose) {
    report_runtime(
      io_end = io_end,
      io_start = io_start,
      io_start2 = io_start2,
      io_end2 = io_end2,
      run_result = run_result
    )
  }

  # return status of run
  return(run_result$status)
}


# Helper functions -----------------------------

#' checks SWAP error message
#' @keywords internal
#' @param msg file from processx
check_swap_message <- function(status, verbose = F){

  if(status == 100){
    if(verbose){
      cat((bold(blue("\u2705",
                   "SWAP run complete! \n"))))
    }
  }

  # TODO expand this
  if (status != "100") {
    warning(glue("SWAP error, code {status}"))
    if (status == "2") {
      warning(glue("SWAP model timed out, with timeout {timeout}"))
    }
  }
}

# check to see if SWAP.exe is in the right place
check_exe_location <- function(swap_exe, verbose) {
  # check if SWAP.exe is in the right place
  if (file.exists(swap_exe) == FALSE) {
    stop(glue("swap.exe must be located in parent directory of {project}!\n Required Path: {swap_exe}"))
  } else{
    if (verbose) {
      cat("\u2714",
          blue(underline("swap.exe")), blue("found"), "\n")
    }
  }
}

parse_run_paths <- function(project_path, verbose, swap_file) {
  # Parse paths
  seperated <- project_path %>% str_split("/") %>% unlist()
  project <- seperated %>% utils::tail(1)
  work_dir <-seperated[1:length(seperated)-1] %>% paste(collapse = "/")
  swap_exe_path <- work_dir %>% paste(collapse = "/")
  swap_exe <- paste0(swap_exe_path,"/swap.exe")
  swap_file_path <- glue("{project}/rswap/{swap_file}")

  outpath <- paste0("rswap/", swap_file)

  # Print run info
  if(verbose){cat(magenta(bold(("Running SWAP project"))), bgYellow(project), bold(magenta("with main file")), bgYellow(swap_file),"\n")}

  swap_run_paths <- data.frame(project, work_dir, swap_exe, swap_file_path, outpath)

  return(swap_run_paths)
}

run_swap_base <- function(verbose, swap_run_paths, timeout) {
  if(verbose){
    cat(blue(bold(">> Running")), yellow(bold("swap.exe")), blue(bold("in working directory: ")))
    cat(underline(swap_run_paths$work_dir),"\n")
    cat(blue(bold(">> Executing the following file: ")))
    cat(underline(swap_run_paths$swap_file_path),"\n")
    cat(blue(bold(glue(">> With max runtime of:"))), underline(glue("{timeout} seconds")),"\n")
  }

  model_start <- Sys.time()
  msg <- processx::run(
    command = "swap.exe",
    wd = swap_run_paths$work_dir,
    args =  swap_run_paths$swap_file_path,
    error_on_status = F,
    timeout = timeout,
    echo_cmd = F,
    echo = verbose
  )

  model_end <- Sys.time()

  model_timer <- (model_end-model_start) %>% round(2)

  run_result <- data.frame(msg, model_timer)

  return(run_result)
}

report_runtime <- function(io_end, io_start, io_start2, io_end2, run_result) {

  io_time <- ((io_end-io_start)+(io_end2-io_start2)) %>% round(2)

  cat("\u23f2",
      blue("I/O duration:"),
      blue(bold(io_time)),
      blue("seconds"),
      "\n")
  cat("\u23f3",
      blue("Model run time:"),
      blue(bold(run_result$model_timer)),
      blue("seconds"),
      "\n")
  cat("\U0001f3c1",
      blue(bold("rswap run routine complete.",
                "\n")))
}

