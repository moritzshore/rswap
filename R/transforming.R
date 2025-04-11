# Transforming SWAP data

#' Melt SWAP data
#'
#' This function transforms your observed and modeled data to by in tidy format.
#' This makes it easier to use with other r packages such as `ggplot2`.
#'
#' This function will adjust to any variables and depths you give it. If you do
#' not provide any variables, or any depths, all available ones will be used.
#'
#' @seealso [match_swap_data()]
#'
#'
#' @param project_path path to project directory `string`
#' @param variable variables to include in the melt `string`
#' @param depth depths to include in the melt `numeric`
#' @param verbose print status? `flag`
#'
#' @returns Melted dataframe of matched modeled and observed values
#' @export
#'
#' @importFrom dplyr select pull
#' @importFrom purrr map
#' @importFrom stringr str_split
#' @importFrom tidyr pivot_longer
melt_swap_data <- function(project_path, variable = NULL, depth = NULL, verbose = F) {

  obser_dat <- load_swap_observed(project_path, archived = F, verbose = verbose)

  if(variable %>% is.null()){
    variable <- get_swap_variables(swap_data = obser_dat, verbose = verbose)
  }

  if(depth %>% is.null()){
    depths <- get_swap_depths(data = obser_dat, variable = variable)
  }else{
    available_depths <- get_swap_depths(data = obser_dat, variable = variable)
    if (depth %in% available_depths == FALSE) {
      stop(
        "Depth ",
        depth,
        " is not available for variable ",
        variable,
        " in the observed data! Available depths include: ",
        paste0(available_depths, collapse = ", ")
      )
    }
  }
  matched <- match_swap_data(project_path, variable, depth, verbose, archived = F)

  mod_melt <- matched$mod %>% tidyr::pivot_longer(cols = colnames(matched$obs)[-1])
  obs_melt <- matched$obs %>% tidyr::pivot_longer(cols = colnames(matched$obs)[-1])

  var <- obs_melt %>% dplyr::select("name") %>% dplyr::pull() %>% stringr::str_split("_") %>% purrr::map(1) %>% unlist()
  depth <- obs_melt %>% dplyr::select("name") %>% dplyr::pull() %>% stringr::str_split("_") %>% purrr::map(2) %>% unlist() %>% as.numeric()

  obs_melt$var <- var
  obs_melt$depth <- depth
  obs_melt <- obs_melt %>% dplyr::select(-"name")
  obs_melt$type = "obs"

  # repeat above code for mod
  mod_melt <- matched$mod %>% tidyr::pivot_longer(cols = colnames(matched$obs)[-1])

  var <- mod_melt %>% dplyr::select("name") %>% dplyr::pull() %>% stringr::str_split("_") %>% purrr::map(1) %>% unlist()
  depth <- mod_melt %>% dplyr::select("name") %>% dplyr::pull() %>% stringr::str_split("_") %>% purrr::map(2) %>% unlist() %>% as.numeric()

  mod_melt$var <- var
  mod_melt$depth <- depth
  mod_melt <- mod_melt %>% dplyr::select(-"name")
  mod_melt$type = "mod"

  full_melt <- rbind(obs_melt, mod_melt)

  return(full_melt)
}
