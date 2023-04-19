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

  # Refesh the temp directory
  unlink(paste0(project_path, "/rswap"), recursive = T)


  # builds a directory for performing package actions, and returns the path
  rswap_directory <- build_rswap_directory(project_path)

  # reads in the swap parameters and tables
  parse_result <- parse_swp_file(project_path, swap_file, verbose)

  # changes the paths in the swap main file to reflect the temporary location
  params <-
    update_swp_paths(
      project_path = project_path,
      swap_exe = swap_exe,
      parameters = load_swap_parameters(project_path, swap_file),
      verbose =  verbose
    )


  # set the output of SWAP
  parameters <- set_swap_output(project_path = project_path,
                               parameters = parameters,
                               autoset_output = autoset_output,
                               verbose = verbose)

  }

  # location for where the swap file is to be written

  outpath <- paste0("rswap/",swap_file)

  write_swap_parameters(project_path, parameters = params, verbose = verbose)
  # Write swap file
  write_swap_file(project_path = project_path,
                  outfile = outpath,
                  verbose = verbose)

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

  # check SWAP message
  check_swap_message(msg, verbose)

  # move run files to temp dir
  move_run_files(work_dir, project, verbose)

  if(verbose){
    cat("🏁",
        blue(bold("rswap run routine complete.",
                  "\n"))
    )
  }

  # return status of run
  return(msg$status)
}

#' checks SWAP error message
#' @keywords internal
#' @param msg file from processx
check_swap_message <- function(msg, verbose = F){

  if(msg$status == 100){
    if(verbose){
      cat((bold(blue("✅",
                   "SWAP run complete! \n"))))
    }
  }

  # TODO expand this
  if (msg$status != "100") {
    warning(glue("SWAP error, code {msg$status}"))
    if (msg$status == "2") {
      warning(glue("SWAP model timed out, with timeout {timeout}"))
    }
  }

}
