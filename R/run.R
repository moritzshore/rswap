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
