run_swap_parallel <- function(project_paths,
                              n_cores = NULL,
                              swap_files = NULL,
                              autoset_output = F,
                              force = T,
                              verbose = F,
                              timeout = Inf) {

  # if no cores are provided, then use 2 less than exist.
  if(is.null(n_cores)){
    n_cores = parallel::detectCores() - 2
  }

  # if no swap file names are passed, then use the default
  if(is.null(swap_files)){
    if(get_os() == "windows"){swap_files <- rep("swap.exe", length(project_paths))}
    else if(get_os() == "linux"){swap_files <- rep("swap420", length(project_paths))}
    else{stop("OS not recognized / supported?!")}
  }

  #project_paths <- list.files("C:/Users/mosh/Documents/rswaptesting/paralel/", full.names = T)

  dirname <- dirname(project_paths[1])

  log <- paste0(dirname, "/rswap_parallel.log")
  cl <- parallel::makeCluster(n_cores,  outfile=log)
  doParallel::registerDoParallel(cl)

  batch_info <- foreach(proj = project_paths, swapfile = swap_files) %dopar% {
      run_swap(project_path = proj,
              )
  }
}

# modded from https://www.r-bloggers.com/2015/06/identifying-the-os-from-r/
get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname'][[1]]
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}
