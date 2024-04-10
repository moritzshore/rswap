#' Sensitivity Analysis
#'
#' This function automates a simple sensitivity analysis for your SWAP project.
#'
#' Currently supported output is statistical evalution only. This function is not
#' yet fully finished!
#'
#' @param project_path path to the project directory (string)
#' @param variable SWAP parameter to alter (string)
#' @param values Parameter set to vary your variable across (vector)
#' @param row (optional, numeric) if your parameter is stored in a table you need to pass
#'   the row in which it is in
#' @param statistic (optional, string) if you would like to evaluate the sensitivity of
#'   model performance to the passed parameter set, please specify which
#'   statistical indicator you would like to use (supported by `hydroGOF`)
#' @param obs_variable (optional, string) the observed variable you would like model perfomance to be evaluted with (required if `statistic` is passed)
#' @param depth (optional, numeric) if your `obs_variable` has a respective depth, pass it here (cm)
#' @param cleanup (optional, boolean) delete the model files (results) after function completion?
#' @param swap_file (optional, string) SWAP file to run, by default "swap.swp"
#' @param n_cores (optional, numeric) The number of CPU cores to run the sensitivity anaylysis on.
#' @param autoset_output (optional, boolean) Match model output to observed data?
#' @param force (optional, boolean) If an rswap directory already exists, no new one will be generated/reloaded unless force=TRUE. Defaults to TRUE
#' @param verbose (optional, boolean) Print actions to console?
#' @param timeout (optional, numeric) Maximum model runtime in seconds. Unlimited by default.
#'
#' @importFrom plotly plot_ly layout
#' @importFrom dplyr rename bind_rows
#'
#' @return Prints interactive plot and returns dataframe of the results.
#'
#' @examples
#' if(FALSE){
#' check_swap_sensitivity(
#' project_path = "C:/Users/mosh/Documents/rswaptesting/tetves/",
#' variable = "OSAT",
#' values = seq(0.32, 0.48, by = 0.01),
#' row = 1,
#' statistic = "NSE",
#' obs_variable = "WC",
#' depth = 15,
#' cleanup = TRUE,
#' autoset_output = TRUE,
#' verbose = TRUE
#' )}
#'
#' @export
#'
check_swap_sensitivity <- function(project_path,
                                   variable,
                                   values,
                                   row = NULL,
                                   statistic = NULL,
                                   obs_variable = NULL,
                                   depth = NULL,
                                   cleanup = TRUE,
                                   swap_file = "swap.swp",
                                   n_cores = NULL,
                                   autoset_output = FALSE,
                                   force = TRUE,
                                   verbose = FALSE,
                                   timeout = Inf){


  # if the project has not been parsed yet, parse it here!
  ############
  ### TODO ###
  ############
  base_proj <- paste0(project_path, "/rswap/")
  TODO = dir.exists(base_proj)

  working_dir <- dirname(project_path)
  project_name <- basename(project_path)

  sens_dir <- paste0(working_dir, "/rswap_sensitity_run_", project_name)

  dir.create(sens_dir)

  sensnames <- paste0(sens_dir, "/run_",c(1:length(values)), "/")

  if(verbose){cat("generating scenario files\n")}
  res = lapply(X = sensnames,
    FUN = function(x) {
      dir.create(x, showWarnings = F)
      if(verbose){cat(">>>", x, "\n")}
      status = file.copy(from = base_proj, to = x, recursive = T, overwrite = T)})

  # this does not work yet
  #set_swap_format(parameter = variable, value = .03)

  # modifiy the SWAP files with rswap function
  res <- modify_swap_file(project_path = sensnames,
                   input_file = swap_file, output_file = "rswap/swap_sens.swp",
                   variable = variable, value = values, row = row, fast = T, verbose = verbose)

  # this should eventually be replaced by storing the ".exe" location in the
  # package environment.
  if(file.exists(paste0(working_dir, "/swap.exe"))){
    status = file.copy(paste0(working_dir, "/swap.exe"), to = sens_dir)
  }else{stop("SWAP exe must be here: [", paste0(working_dir, "/swap.exe]"))}

  run_status <- run_swap_parallel(project_paths = sensnames, n_cores = n_cores,
                    working_dir = sens_dir,
                    # this is ugly.
                    swap_files = rep("swap_sens.swp", length(sensnames)),
                    autoset_output = autoset_output,
                    # force NEEDS to be false here because we only copied
                    # in the rswap files, not the whole directory.
                    force = FALSE,
                    verbose = verbose, timeout = timeout)

  # now we can delete the runs and swap exe and only keep the results for
  # analysis.
  unlink(sensnames, recursive = T)
  unlink(paste0(sens_dir, "/swap.exe"))
  #file.remove(list.files(sens_dir, pattern = "*.log", full.names = T))



  # this is the filepath of the results (ugly.. should be fixed)
  sens_res_paths <- paste0(sens_dir, "/rswap_sensitity_run_", project_name,
                           "_rswap_parallel_results/run_",c(1:length(values)))


  # if a statistical evaluation is desired, run this routine
  if(statistic %>% is.null() == FALSE){

    if(obs_variable %>% is.null()){
      stop("[rswap] you need to pass `obs_variable` if you also passed a `statistic`")
    }

    stat_result <- get_swap_performance(sens_res_paths,stat = statistic,
                                        variable = obs_variable,
                                        depth = depth, archived = FALSE,
                                        verbose = verbose)
    result_df <- dplyr::bind_rows(stat_result, .id = "run")
    result_df <- dplyr::rename(result_df, obs_variable = variable)
    result_df <- dplyr::rename(result_df, performance = "value")
    result_df$performance <- result_df$performance %>% round(3)
    result_df$parameter_value = values
    result_df$parameter_name = variable
    result_df$performance_metric = statistic

    fig <- result_df %>% plotly::plot_ly(
      x = ~ parameter_value,
      y = ~ performance,
      color = ~ performance,
      type = "bar",
      name = paste0("(",variable,",",statistic,")")
    )  %>% plotly::layout(
      title = paste0("rswap sensitivity analysis: ", project_name),
      xaxis = list(title =  variable),
      yaxis = list(title = statistic))
    print(fig)


    if(cleanup){
      unlink(sens_dir, recursive = T)
    }
    return(result_df)
  }

  # if the effect on an output variable is desired, run this routine:
  ############
  ### TODO ###
  ############
  if(is.null(statistic)){
    stop("[rswap] sorry, non-statistical sensitivity has not been implemented yet,
         please pass a 'statistic' parameter")
  }
}

## Testing parameter set.
# variable = "OSAT"
# project_path = "C:/Users/mosh/Documents/rswaptesting/tetves/"
# values <- seq(0.32, 0.48, by = 0.01)
# row = 1
# statistic = "NSE"
# swap_file = "swap.swp"
# n_cores = NULL
# autoset_output = T
# depth = 15
# force = T
# verbose = T
# obs_variable = "WC"
# timeout = Inf
# cleanup = TRUE

