#' Run SWAP Scenario
#'
#' tbc
#'
#' tbc
#'
#' @param scen_dir (string) path to directory containing scenario directories (must contain a `SWAP.exe`, see details)
#' @param scen_name (string) name of the directory containing the various SWAP scenarios (see details)
#' @param vars (vector) variables to return from output (ie. `vars <- c("WC_15", "drainage")`)
#' @param rerun (logical) rerun the swap projects?
#' @param verbose (logical) print to console?
#'
#' @importFrom dplyr %>%
#' @importFrom stringr str_split
#' @importFrom crayon blue italic
#'
#' @returns dataframe of the scenario results for `var`
#' @export
#'
#' @examples
#'
#' #tbc
run_swap_scenario <- function(scen_dir, scen_name, vars, rerun, verbose) {
  # force DATE to be the first in INLIST
  vars2 <- c("DATE", vars)
  INLIST_CSV <- paste(vars2, collapse = ",")
  # parse the scenarios to inlcuded.
  mgt_scen = list.dirs(paste0(scen_dir, "/", scen_name), recursive = F)
  if (rerun) {
    # setting all the management scenarios to have the correct variable output.
    ## HACK: to set the output of the scenario analysis, we just write in a fake
    ## observed data file and let the "AUTOSET_OUTPUT" flag do the heavy lifting:
    hack_obs_path = paste0(mgt_scen, "/rswap_observed_data.csv")
    hack_ncols = length(INLIST_CSV %>% stringr::str_split(",", simplify = T))
    hack_numerics = rep(-99, hack_ncols)

    write_hack <- function(hacpathlist) {
      write(INLIST_CSV, file = hacpathlist, append = FALSE)
      write(hack_numerics, file = hacpathlist, append = TRUE,
            sep = ",", ncolumns = hack_ncols)
    }

    lapply(X = hack_obs_path, FUN = write_hack)
    par_runs <- run_swap_parallel(
      project_paths = mgt_scen,
      verbose = verbose,
      autoset_output = TRUE,
      force = TRUE
    )
    # test if all runs work=ed
    if (all(par_runs == 100)){if(verbose){cat(blue(italic(">> all runs suceeded << \n")))}
    } else{stop("RSWAP SCENARIO RUN: Error! some runs failed!")}
  } else{if(verbose){cat(blue(italic(">> not re-running scenarios <<")))}}

  out_dir <- paste0(scen_dir, scen_name, "_rswap_parallel_results/")
  # remove the extra swap.exe (TODO: why is this needed? shouldn't this be performed in the run swap paralel function?)
  file.remove(paste0(out_dir, "swap.exe"))
  scen_names <- list.files(path = out_dir)
  # SWAP output is all uppercase? always?
  VAR = toupper(vars)
  # custom function to extract the variable of interest
  scen_get_out <- function(scen_1) {
    output <- load_swap_output(paste0(out_dir, scen_1), verbose = verbose)$custom_depth %>% select("DATE", all_of(VAR))
    output$Scenario <- scen_1
    return(output)
  }
  # extract and combine the output.
  listofresults <- lapply(X = scen_names, scen_get_out)
  res_df <- do.call("rbind", listofresults)
  return(res_df)
}
