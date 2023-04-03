# Functions relating to statistics

#' Get Model Performance
#'
#' Returns the performance for the chosen variable(s) and performance indicator(s).
#' Supported performance indicators are: NSE, PBIAS, RMSE, RSR.
#' Supported variables are those provided in the observed data file.
#'
#' Please note, passing multiple variables at differing depths will not work! (yet)
#'
#' @param project_path path to project directory (string)
#' @param archived set to `TRUE` if project is saved in "rswap_saved/" (flag)
#' @param stat statistical performance indicator (ie. "NSE") (string)
#' @param variable variable name. leave blank for all. (string)
#' @param depth depth of variable if it is depth-wise. leave blank for all (numeric)
#' @param verbose print status? (flag)
#'
#' @importFrom dplyr %>% nth
#' @importFrom tibble tibble
#' @importFrom glue glue
#'
#' @returns dataframe value(s) of performance indicator(s) for given variable(s) and depth(s)
#'
#' @export
get_performance <-
  function(project_path, archived = FALSE,
           stat = NULL, variable = NULL,
           depth = NULL, verbose = F) {

    # TODO rename to get_swap_performance()

    if (variable %>% is.null()) {
      obs <- load_observed(project_path)
      variable = obs$observed_variables
    }

    rlist <- match_mod_obs(
        project_path = project_path,
        variable = variable,
        depth = depth,
        verbose = verbose,
        archived = archived
      )

    # unpack the list
    modelled_data_filtered = rlist$mod
    observed_data_filtered = rlist$obs

    obs_col_length = length(observed_data_filtered)

    return_df <- data.frame()
    for (i in c(2:obs_col_length)) {  # from 2 because skip date col

      obs_join = data.frame(DATE=observed_data_filtered$DATE, obs = observed_data_filtered[i])
      mod_join = data.frame(DATE=modelled_data_filtered$DATE, obs = modelled_data_filtered[i])
      name = obs_join %>% colnames() %>% nth(2)

      joined_df <- data.frame(DATE = observed_data_filtered$DATE)
      joined_df <- left_join(joined_df, obs_join, by = "DATE")
      joined_df <- left_join(joined_df, mod_join, by = "DATE")
      colnames(joined_df)<-c("DATE", "obs", "mod")

      nse_val = NSE(obs = joined_df$obs, mod = joined_df$mod)
      pbias_val = PBIAS(obs = joined_df$obs, mod = joined_df$mod)
      rmse_val = RMSE(obs = joined_df$obs, mod = joined_df$mod)
      rsr_val = RSR(obs = joined_df$obs, mod = joined_df$mod)

      row = data.frame(var = name, NSE = nse_val, PBIAS = pbias_val, RMSE = rmse_val, RSR = rsr_val)
      return_df = rbind(return_df, row)
    }

    # filter by stat if given
    if(stat %>% is.null()==FALSE){
      return_df<-return_df %>% select(var, all_of(stat))
    }

    return(return_df %>% tibble())
  }

#' NSE
#'
#' Calculates the NSE value for a paired observed and modeled vector.
#'
#' @param obs numeric vector of observed values
#' @param mod numeric vector of modeled values
#'
#' @returns numeric NSE value rounded to 2 decimals
#'
#' @export
#'
NSE <- function(obs, mod) {
  #TODO: change this function to accept a named vector, or dataframe.
  return((1 - (sum((obs - mod) ^ 2, na.rm = T
  ) / sum((obs - mean(obs, na.rm = T)) ^ 2, na.rm = T
  ))) %>% round(x = ., digits = 2))
}

#' PBIAS
#'
#' Returns PBIAS value for a paired observed and modeled vector.
#'
#' @param obs numeric vector of observed values
#' @param mod numeric vector of modeled values
#'
#' @returns numeric PBIAS value rounded to 2 decimal
#'
#' @export

PBIAS <-function(obs, mod) {
  #TODO: change this function to accept a named vector, or dataframe.
  return(((sum(obs - mod, na.rm = T) * 100) / sum(obs, na.rm = T)) %>% round(x = ., digits = 2))
}

#' RMSE
#'
#' Returns RMSE value for a paired observed and modeled vector.
#'
#' @param obs numeric vector of observed values
#' @param mod numeric vector of modeled values
#'
#' @returns numeric RMSE value rounded to 2 digits
#'
#' @export
RMSE <- function(obs, mod) {
  #TODO: change this function to accept a named vector, or dataframe.
  return(sqrt(sum((obs - mod) ^ 2, na.rm = T) / length(obs)) %>% round(x = ., digits = 2))
}

#' RSR
#'
#' Returns RSR value for a paired observed and modeled vector.
#'
#' @param obs numeric vector of observed values
#' @param mod numeric vector of modeled values
#'
#' @returns numeric RSR value rounded to 2 digits
#'
#' @export
RSR <- function(obs, mod) {
  # TODO: change this function to accept a named vector, or dataframe.
  return(RMSE(obs, mod) / sd(obs, na.rm = T) %>% round(x = ., digits = 2))
}
