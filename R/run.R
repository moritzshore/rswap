# run functions

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
run_swap <- function(project_path, swap_exe, swap_file = "rswap.swp",
                     verbose = T, timeout = Inf) {

    # reads in the swap parameters and tables
    parse_result <- parse_swp_file(project_path = project_path,
                                   swap_file = swap_file, verbose = verbose)

    # changes the paths in the swap main file to reflect the temporary location
    params <- update_swp_paths(project_path, swap_exe,
                                  parse_result$parameters, verbose)


    # builds a directory for performing package actions, and returns the path
    rswap_directory <- build_rswap_directory(project_path)

    # location for where the swap file is to be written
    outpath <- paste0(rswap_directory, swap_file)

    rswap_file <- write_swap_file(parameters = params,
                                  table_path = parse_result$table_path,
                                  outpath = outpath, verbose = verbose)

    # parse the working directory from the given swap path
    swap_path_split = swap_exe %>% str_split("swap.exe", simplify = T)
    swap_wd <- swap_path_split[, 1]

    # remove the working directory from the path of the swap main file
    fixed_path <- rswap_file %>% str_remove(swap_wd)

    # run the model
    msg <- run(
      command = "swap.exe",
      wd = swap_wd,
      args = fixed_path,
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
