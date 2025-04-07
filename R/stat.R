# Functions relating to statistics

#' Get Model Performance
#'
#' Returns the performance for the chosen variable(s) and performance
#' indicator(s). performance indicators are sourced form package
#' [hydroGOF](https://cran.r-project.org/web/packages/hydroGOF/hydroGOF.pdf)
#' Supported variables are those provided in the observed data file.
#'
#' The performance indicator will be the result of a function passed as a string
#' in the `stat` parameter as called from the `hydroGOF` package.
#'
#' The following are supported for `stat`:
#'
#' *br2 cp d KGE mae md me mNSE mse nrmse NSE pbias rd rmse rNSE rPearson rSD rsr ssq VE*
#'
#' Please note, passing multiple variables at differing depths will not work!
#' (yet)
#'
#' @param project_path path to project directory (string)
#' @param archived set to `TRUE` if project is saved in "rswap_saved/" (flag)
#' @param stat statistical performance indicator from package `hydroGOF`
#'   (string)
#' @param variable variable name. leave blank for all. (string)
#' @param depth depth of variable if it is depth-wise. leave blank for all
#'   (numeric)
#' @param verbose print status? (flag)
#'
#' @importFrom dplyr %>% nth
#' @importFrom tibble tibble
#' @importFrom glue glue
#' @importFrom hydroGOF br2 cp d KGE mae md me mNSE mse nrmse NSE pbias rd rmse rNSE rPearson rSD rsr ssq VE
#' @importFrom crayon blue underline red bold italic
#'
#' @returns dataframe value(s) of performance indicator(s) for given variable(s)
#'   and depth(s)
#'
#' @export
get_swap_performance <-
  function(project_path,
           stat,
           archived = FALSE,
           variable = NULL,
           depth = NULL,
           verbose = F) {

    # vectorized behavior (recursive)
    if(length(project_path)>1){
      result = lapply(X = project_path, FUN = function(x){
        get_swap_performance(project_path = x, stat = stat, archived = archived,
                             variable = variable, depth = depth, verbose = verbose)
      }
      )
      return(result)
    }

    # Supported functions from hydroGOF:
    hydroGOF_support <- c("br2", "cp", "d", "KGE", "mae", "md", "me", "mNSE", "mse",
                          "nrmse", "NSE", "pbias", "rd", "rmse", "rNSE", "rPearson", "rSD",
                          "rsr", "ssq", "VE")

    # check to see if the stat variable is an exported function of HydroGOF
    if (stat %in% hydroGOF_support == FALSE) {
      stop(
        bold(red("Unsupported 'stat'")),
        " Supported are the following performance functions from package ",
        bold(blue("'hydroGOF' \n")),
        italic(blue("stat = ",
        paste0("'", hydroGOF_support, "'", collapse = ", "))),
        "\n For help on these functions please enter ",
        blue(underline('"?hydroGOF"')),
        " into the console, or check the help page of this function."
      )
    } else{
      # if the function is supported, it is extracted from the namespace:
      # TODO find a way to pass args such as for KGE or cor
      hydroGOF_namespace <- loadNamespace("hydroGOF")
      hydroGOF_function <- get(stat, envir = hydroGOF_namespace)
    }

    # if no variable is passed, ALL variables are used
    if (variable %>% is.null()) {
      obs <- load_swap_observed(project_path)
      variable = get_swap_variables(swap_data = obs, verbose = verbose)
    }

    # filters and matched the desired vars and depths
    rlist <- match_swap_data(
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
    for (i in c(2:obs_col_length)) {
      # from 2 because skip date col

      obs_join = data.frame(DATE = observed_data_filtered$DATE,
                            obs = observed_data_filtered[i])
      mod_join = data.frame(DATE = modelled_data_filtered$DATE,
                            obs = modelled_data_filtered[i])
      name = obs_join %>% colnames() %>% dplyr::nth(2)

      joined_df <- data.frame(DATE = observed_data_filtered$DATE)
      joined_df <- dplyr::left_join(joined_df, obs_join, by = "DATE")
      joined_df <- dplyr::left_join(joined_df, mod_join, by = "DATE")
      colnames(joined_df) <- c("DATE", "obs", "mod")

      # Error handling for if there is no overlapping values.
      if (joined_df$obs %>% is.na() %>% all()) {
        error_col <- observed_data_filtered %>% colnames() %>% nth(i)
        warning("No observations within modelled timeframe! Cannot compute statistics for: \n",
                error_col, " @ (", project_path, ")\n")
        stat_val <- NA
      }else{
        stat_val <- hydroGOF_function(sim = joined_df$mod,
                                      obs = joined_df$obs,
                                      na.rm = TRUE)
      }
      row = data.frame(variable = name, value =  stat_val)
      return_df = rbind(return_df, row)
    }
    return(return_df %>% tibble::tibble())
  }
