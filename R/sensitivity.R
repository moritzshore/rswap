
variable = "OSAT"

values <-

check_swap_sensitivity <- function(project_path,
                                   variable,
                                   values,
                                   row = NULL,
                                   statistic = NULL,
                                   swap_file = "swap.swp",
                                   autoset_output = F,
                                   force = T,
                                   verbose = F,
                                   timeout = Inf){


  # if the project has not been parsed yet, parse it here!
  ############
  ### TODO ###
  ############
  base_proj <- paste0(project_path, "/rswap")
  dir.exists()

  working_dir <- dirname(project_path)
  project_name <- basename(project_path)

  sens_dir <- paste0(working_dir, "/rswap_sensitity_run_", project_name)

  file.copy()

  modify_swap_file(project_path = project_path,
                   input_file = swap_file, output_file = "swap_mod.swp",
                   variable = variable, value = values, row = row)


  rswap::get_swap_performance(project_path,stat = "NSE", variable = "WC")


}
