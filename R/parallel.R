#' Run SWAP in parallel.
#'
#' This function allows you to execute multiple SWAP runs in parallel.
#'
#' @param project_paths character vectores of all projects which should be run in parallel.
#' @param n_cores (optional) the number of CPU cores to use. Defaults to 2 less than available.
#' @param working_dir (optional) the directory in which SWAP is located. (defaults to parent directory of first `project_path`)
#' @param swap_files (optional) character vector with the same length as `project_paths`. Defines the name of the swap main files to run. defaults to "swap.swp".
#' @param autoset_output (optional) Flag which if set to TRUE, rswap will automatically detect your observed data provided in the observed file and match it to the SWAP output. if this is set to FALSE, then INLIST csv must be set by the user either manually or with set_swp_output() or change_swap_par() for several other rswap function to work
#' @param force (optional) Flag, if an rswap directory already exists, no new one will be generated/reloaded unless force=TRUE defaults to true.
#' @param verbose (optional) Flag, print status?
#' @param timeout (optional) numeric, number of seconds before run timeout (unlimited by default)
#'
#' @importFrom dplyr  %>%
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom crayon magenta bold italic bgYellow underline
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach %dopar%
#'
#'
#' @return Nested list of SWAP status codes for each run (100 = sucess)
#' @export
#'
#' @examples
#'
#' if(FALSE){
#' project_paths <- list.files("C:/somepath/", full.names = T, pattern = "hupsel")
#' run_swap_parallel(project_paths)
#' }
#'
run_swap_parallel <- function(project_paths,
                              n_cores = NULL,
                              working_dir = NULL,
                              swap_files = NULL,
                              autoset_output = F,
                              force = T,
                              verbose = F,
                              timeout = Inf) {

  # start timmer
  t1 <- Sys.time()

  # if no cores are provided, then use 2 less than exist.
  if(is.null(n_cores)){n_cores = parallel::detectCores() - 2}
  if(n_cores < 1){n_cores = 1}

  # if no swap file names are passed, then use the default
  if(is.null(swap_files)){swap_files <- rep("swap.swp", length(project_paths))}

  # if no working dir is defined, use the parent directory of the first path
  if(is.null(working_dir)){
    working_dir <- dirname(project_paths[1])
    log <- paste0(working_dir, "/rswap_parallel.log")
  }

  paralleldir <- paste0(working_dir, "/rswap_parallel")
  if(dir.exists(paralleldir)){warning("rswap parallel: overwriting previous parallel run!")}
  unlink(paralleldir, recursive = T)
  dir.create(paralleldir)


  if (.Platform$OS.type == "windows") {
    swap_exe <- "swap.exe"
  } else if (.Platform$OS.type == "unix") {
    swap_exe <- "swap420"
  } else{
    stop(paste0("operating system not recognized!", .Platform$OS.type,
                "please open a new issue on github!"))
  }


  if(verbose){
    cat(magenta(bold((paste0("Running ",swap_exe)))), underline(bold("in parallel with")), bgYellow(paste0(length(project_paths), " runs")), bold(magenta("using")), bgYellow(n_cores, "cores"),"\n")
    cat(magenta(">>> Working dir:", underline(working_dir), "\n"))
    cat(magenta(">>> Created", underline(paralleldir), "for parallel runs\n"))
    }

  cl <- parallel::makeCluster(n_cores,  outfile=log)
  doParallel::registerDoParallel(cl)
  if(verbose){
    cat(magenta(italic(">>> sucessfully registered cluster, starting runs...\n")))
  }

  # appeasing the R-CMD-CHECK gods
  run_name = NULL
  threadnr = NULL
  swap_file = NULL

  result <-
    foreach::foreach(
      run_name = project_paths,
      swap_file = swap_files,
      threadnr = c(1:length(project_paths))
    ) %dopar% {

      parrundir <- paste0(paralleldir, "/", paste0("thread_", threadnr))
      dir.create(parrundir)
      file.copy(run_name, parrundir, recursive = T)
      file.copy(paste0(working_dir, "/", swap_exe), parrundir)

      ppath <- paste0(parrundir, "/", basename(run_name))
      run_swap(
        project_path = ppath,
        swap_file = swap_file,
        autoset_output = autoset_output,
        force = force,
        verbose = FALSE,
        timeout = timeout
      )
    }
  if(verbose){
    cat(magenta(italic(">>> parallel running completed, closing cluster...\n")))
  }
  parallel::stopCluster(cl)

  if(any(result != 100)){
    warning("rswap parallel running: some runs failed!")
  }

  if(verbose){
    success <- which(result == 100) %>% length()
    cat(bold(magenta("STATUS:")), bgYellow("[", success, "/", length(result),"]"), bold(magenta("runs succeeded\n")))
  }

  if(verbose){cat(magenta(">>> Deleting", swap_exe, "instances \n"))}
  unlink(list.files(paralleldir, pattern = "swap.exe", recursive = T, full.names = T))

  result_dirs <- list.files(paralleldir, full.names = T) %>% list.files(full.names = T)
  result_dir <- paste0(working_dir, "/rswap_parallel_results")
  if(dir.exists(result_dir)){warning("rswap parallel: overwriting previous results!")}
  unlink(result_dir, recursive = T)
  dir.create(result_dir)
  if(verbose){cat(magenta(">>> moving results to", underline(result_dir), "\n"))}
  file.copy(result_dirs, result_dir, overwrite = T, recursive = T)
  unlink(paralleldir, recursive = T)
  if(verbose){cat(magenta(">>> deleting", underline(paralleldir), "\n"))}
  t2 <- Sys.time()
  if(verbose){
    cat(magenta(bold("FINISHED!"), "runtime:\n"))
    print(t2-t1)}

  return(result)
}
