#' deprecated, use 'update_swap_paths()' instead
#'
#' @param project_path deprecated
#' @param swap_exe deprecated
#' @param parameters deprecated
#' @param verbose deprecated
#'
#' @return deprecated
#' @export
#'
update_swp_paths <- function(project_path,
                             swap_exe,
                             parameters,
                             verbose = F) {
  warning(
    "this function has been renamed to: 'update_swap_paths()'. Please update your call, legacy support will be ending soon."
  )
  call <- update_swap_paths(project_path, swap_exe,
                            parameters, verbose)
  return(call)
}


#' deprecated: use "save_swap_run()"
#'
#' @param project_path deprecated
#' @param run_name deprecated
#' @param verbose deprecated
#'
#' @return deprecated
#' @export
#'
save_run <- function(project_path,
                     run_name = NULL,
                     verbose = F) {
  warning(
    "this function has been renamed to: 'save_swap_run()'. Please update your call, legacy support will be ending soon."
  )
  call <- save_swap_run(project_path, run_name, verbose)
  return(call)
}

#' deprecated use "load_swap_observed"
#'
#' @param project_path load_swap_observed
#' @param archived load_swap_observed
#' @param verbose load_swap_observed
#'
#' @return load_swap_observed
#' @export
#'
load_observed <- function(project_path,
                          archived = F,
                          verbose = F) {
  warning(
    "this function has been renamed to: 'load_swap_observed()'. Please update your call, legacy support will be ending soon."
  )
  call <- load_swap_observed(project_path, archived, verbose)
  return(call)
}

#' deprecated use "get_swap_depths"
#'
#' @param data deprecated use "get_swap_depths"
#' @param variable deprecated use "get_swap_depths"
#'
#' @return deprecated use "get_swap_depths"
#' @export
#'
get_depths <- function(data, variable = NULL) {
  warning(
    "this function has been renamed to: 'get_swap_depths()'. Please update your call, legacy support will be ending soon."
  )
  call <- get_swap_depths(data, variable)
  return(call)
}

#' deprecated, use match_swap_data()
#'
#' @param project_path  deprecated, use match_swap_data()
#' @param variable  deprecated, use match_swap_data()
#' @param depth  deprecated, use match_swap_data()
#' @param verbose  deprecated, use match_swap_data()
#' @param archived  deprecated, use match_swap_data()
#'
#' @return  deprecated, use match_swap_data()
#' @export
#'
match_mod_obs <-
  function(project_path,
           variable,
           depth = NULL,
           verbose = F,
           archived = F) {
    warning(
      "this function has been renamed to: 'match_swap_data()'. Please update your call, legacy support will be ending soon."
    )
    call <-
      match_swap_data(project_path, variable, depth, verbose, archived)
    return(call)
  }

#' renamed to: "melt_swap_runs"
#'
#' @param project_path renamed to: "melt_swap_runs"
#' @param variable renamed to: "melt_swap_runs"
#' @param depth renamed to: "melt_swap_runs"
#' @param verbose renamed to: "melt_swap_runs"
#'
#' @return renamed to: "melt_swap_runs"
#' @export
#'
melt_all_runs <-
  function(project_path,
           variable,
           depth = NULL,
           verbose = F) {
    warning(
      "this function has been renamed to: 'melt_swap_runs()'. Please update your call, legacy support will be ending soon."
    )
    call <- melt_swap_runs(project_path, variable, depth, verbose)
    return(call)
  }


#' renamed to: "clean_swap_file"
#'
#' @param project_path renamed to: "clean_swap_file"
#' @param swap_file renamed to: "clean_swap_file"
#'
#' @return renamed to: "clean_swap_file"
#' @export
#'
clean_swp_file <- function(project_path, swap_file = "swap.swp"){
    warning(
      "this function has been renamed to: 'clean_swap_file()'. Please update your call, legacy support will be ending soon."
    )
    call <- clean_swap_file(project_path, swap_file)
    return(call)
}


#' renamed to: "parse_swap_file()"
#'
#' @param project_path renamed to: "parse_swap_file()"
#' @param swap_file renamed to: "parse_swap_file()"
#' @param verbose renamed to: "parse_swap_file()"
#'
#' @return renamed to: "parse_swap_file()"
#' @export
#'
parse_swp_file <- function(project_path, swap_file = "swap.swp", verbose = F) {
  warning(
    "this function has been renamed to: 'parse_swap_file()'. Please update your call, legacy support will be ending soon."
  )
  call <- parse_swap_file(project_path, swap_file, verbose)
  return(call)
}

#' renamed to: "change_swap_parameter()"
#'
#' @param param  renamed to: "change_swap_parameter()"
#' @param name  renamed to: "change_swap_parameter()"
#' @param value  renamed to: "change_swap_parameter()"
#' @param verbose  renamed to: "change_swap_parameter()"
#'
#' @return renamed to: "change_swap_parameter()"
#' @export
#'
change_swap_par <- function(param, name, value, verbose = F){
  warning(
    "this function has been renamed to: 'change_swap_parameter()'. Please update your call, legacy support will be ending soon."
  )
  call <- change_swap_parameter(param, name, value, verbose)
  return(call)
}

#' renamed to: "get_swap_performance()"
#'
#' @param project_path renamed to: "get_swap_performance()"
#' @param archived renamed to: "get_swap_performance()"
#' @param stat renamed to: "get_swap_performance()"
#' @param variable renamed to: "get_swap_performance()"
#' @param depth renamed to: "get_swap_performance()"
#' @param verbose renamed to: "get_swap_performance()"
#'
#' @return renamed to: "get_swap_performance()"
#' @export
#'
get_performance <-
  function(project_path, archived = FALSE,
           stat = NULL, variable = NULL,
           depth = NULL, verbose = F) {
    warning(
      "this function has been renamed to: 'get_swap_performance()'. Please update your call, legacy support will be ending soon."
    )
    call <- get_swap_performance(project_path, archived,
                                 stat,variable,
                                 depth, verbose)
    return(call)
  }

#' renamed to: "rswap_plot_performance()"
#'
#' @param project_path renamed to: "rswap_plot_performance()"
#' @param var renamed to: "rswap_plot_performance()"
#' @param depth renamed to: "rswap_plot_performance()"
#' @param graph renamed to: "rswap_plot_performance()"
#' @param stat renamed to: "rswap_plot_performance()"
#' @param verbose renamed to: "rswap_plot_performance()"
#'
#' @return renamed to: "rswap_plot_performance()"
#' @export
#'
plot_statistics <-
  function(project_path, var, depth =  NULL, graph = "default",stat = "NSE",
           verbose = F) {

    warning(
      "this function has been renamed to: 'rswap_plot_performance()'. Please update your call, legacy support will be ending soon."
    )
    call <- rswap_plot_performance(project_path, var, depth, graph, stat, verbose)
    return(call)
  }


#' this function has been renamed to: 'rswap_plot_multi()'
#'
#' @param project_path this function has been renamed to: 'rswap_plot_multi()'
#' @param vars this function has been renamed to: 'rswap_plot_multi()'
#' @param show this function has been renamed to: 'rswap_plot_multi()'
#' @param verbose this function has been renamed to: 'rswap_plot_multi()'
#'
#' @return this function has been renamed to: 'rswap_plot_multi()'
#' @export
#'
soft_calibration_plot <- function(project_path, vars, show = NULL, verbose = F){
  warning(
    "this function has been renamed to: 'rswap_plot_multi()'. Please update your call, legacy support will be ending soon."
  )
  call <- rswap_plot_multi(project_path, vars, show, verbose)
  return(call)
}

#' this function has been renamed to: 'rswap_plot_variable()
#'
#' @param project_path this function has been renamed to: rswap_plot_variable()
#' @param variable this function has been renamed to: rswap_plot_variable()
#' @param depth this function has been renamed to: rswap_plot_variable()
#' @param verbose this function has been renamed to: rswap_plot_variable()
#'
#' @return this function has been renamed to: rswap_plot_variable()
#' @export
#'
plot_over_under <- function(project_path, variable, depth = NULL, verbose = F) {
  warning(
    "this function has been renamed to: 'rswap_plot_variable()'. Please update your call, legacy support will be ending soon."
  )
  call <- rswap_plot_variable(project_path, variable, depth, verbose)
  return(call)
}

#' this function has been renamed to: 'rswap_plot_compare()'
#'
#' @param project_path this function has been renamed to: 'rswap_plot_compare()'
#' @param variable this function has been renamed to: 'rswap_plot_compare()'
#' @param depth this function has been renamed to: 'rswap_plot_compare()'
#' @param verbose this function has been renamed to: 'rswap_plot_compare()'
#'
#' @return this function has been renamed to: 'rswap_plot_compare()'
#' @export
#'
comparative_plot <- function(project_path, variable, depth =  NULL, verbose = F) {
  warning(
    "this function has been renamed to: 'rswap_plot_compare()'. Please update your call, legacy support will be ending soon."
  )
  call <- rswap_plot_compare(project_path, variable, depth, verbose)
  return(call)
}







