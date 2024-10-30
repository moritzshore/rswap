#' Run SWAP in parallel.
#'
#' This function allows you to execute multiple SWAP runs in parallel.
#'
#' @param project_paths character vector path of all projects which should be run in parallel.
#' @param n_cores (optional) the number of CPU cores to use. Defaults to 2 less than available.
#' @param working_dir (optional) the directory in which model runs will be run, processed, and saved. defaults to parent director of the first project_path
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
#' @seealso [check_swap_sensitivity()] [run_swap()]
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


  # So why are we creating new directories for each SWAP run?

  # Well, in theory we could do it in-place with one directory, however SWAP
  # creates certain files such as "reruns.log" in the working directory, which
  # then get locked during execution. When the other threads get going they
  # cannot write to this file, so only a single SWAP run will succeed. The
  # band-aid fix is then giving each SWAP run its own working directory, which
  # also requires copying all the files and the executable -- inefficient. The
  # best solution would be to contact the SWAP team and see if we can get them
  # to write these files in the same working directory as the swap.swp file for
  # example.

  # Another improvement I considered was automatically deleting the original
  # project folders and replacing them with the ones that were just run. This is
  # not that great because deleting original setups is a dangerous idea. An
  # alternative could be to just copy in the results files, however then you
  # need to figure out which ones those are... But a thing to remember.

  # start timer
  t1 <- Sys.time()

  parallel_project_name <- dirname(project_paths[1]) %>% basename()

  # if no cores are provided, then use 2 less than exist.
  if(is.null(n_cores)){n_cores = parallel::detectCores() - 2}
  if(n_cores < 1){n_cores = 1}

  # if more cores are being used than there are runs, then set them equal
  if(n_cores > length(project_paths)){n_cores = length(project_paths)}

  # if no swap file names are passed, then use the default
  if(is.null(swap_files)){swap_files <- rep("swap.swp", length(project_paths))}

  # if no working dir is defined, use the grandparent directory of the first path
  if(is.null(working_dir)){
    working_dir <- dirname(dirname(project_paths[1]))
  }

  paralleldir <- paste0(working_dir, "/rswap_parallel")
  if(dir.exists(paralleldir)){warning("rswap parallel: overwriting previous parallel run!")}
  unlink(paralleldir, recursive = T)
  dir.create(paralleldir)

  if (.Platform$OS.type == "windows") {
    swap_name <- "swap.exe"
  } else if (.Platform$OS.type == "unix") {
    swap_name <- "swap420"
  } else{
    stop(paste0("operating system not recognized!", .Platform$OS.type,
                "please open a new issue on github!"))
  }

  # default exe location (needs to be here for short file paths)
  # https://github.com/moritzshore/rswap/wiki/SWAP-errors#cut-off-paths
  swap_exe <- paste0(working_dir, "/", swap_name)
  if(file.exists(swap_exe) == FALSE){
    stop(swap_name, " must be located in the working directory:\n",working_dir, "\n")
  }


  if(verbose){
    cat(magenta(bold((paste0("Running ",swap_name)))), underline(bold("in parallel with")), bgYellow(paste0(length(project_paths), " runs")), bold(magenta("using")), bgYellow(n_cores, "cores"),"\n")
    cat(magenta(">>> Working dir:", underline(working_dir), "\n"))
    cat(magenta(">>> SWAP location:", underline(swap_exe), "\n"))
    cat(magenta(">>> Created", underline(paralleldir), "for parallel runs\n"))
    }

  cl <-parallel::makeCluster(n_cores,
                             outfile = paste0(working_dir, "/rswaparallel_", parallel_project_name, ".log"))

  doParallel::registerDoParallel(cl)
  if(verbose){
    cat(magenta(italic(">>> sucessfully registered cluster, starting runs...\n")))
  }

  # appeasing the R-CMD-CHECK gods
  run_name = NULL
  threadnr = NULL
  swap_file = NULL

  result <-
    foreach(
      run_name = project_paths,
      swap_file = swap_files,
      threadnr = c(1:length(project_paths))
    ) %dopar% {

      parrundir <- paste0(paralleldir, "/", paste0("thread_", threadnr))
      dir.create(parrundir)
      file.copy(run_name, parrundir, recursive = T)
      file.copy(swap_exe, parrundir)

      ppath <- paste0(parrundir, "/", basename(run_name))
      print(paste0("!!!!!!!!!", ppath, "!!!!!!!!!!" ))
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

  if(verbose){cat(magenta(">>> Deleting", swap_name, "instances \n"))}
  unlink(list.files(paralleldir, pattern = "swap.exe", recursive = T, full.names = T))

  result_dirs <- list.files(paralleldir, full.names = T) %>% list.files(full.names = T)
  result_dir <- paste0(working_dir, "/", parallel_project_name, "_rswap_parallel_results")

  if (dir.exists(result_dir)) {
    unlink(result_dir, recursive = T)
    warning("rswap parallel: overwriting previous results!")
  }

  dir.create(result_dir)

  if(verbose){cat(magenta(">>> moving results to", underline(result_dir), "\n"))}
  file.copy(result_dirs, result_dir, overwrite = T, recursive = T)
  unlink(paralleldir, recursive = T)
  if(verbose){cat(magenta(">>> deleting", underline(paralleldir), "\n"))}
  t2 <- Sys.time()
  if(verbose){
    cat(magenta(bold("FINISHED!"), "runtime:\n"))
    print(t2-t1)}

  #name the result based off project name
  names(result) <- basename(project_paths)

  return(result)
}
