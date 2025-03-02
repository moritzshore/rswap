



require(dplyr)
require(ggplot2)

# testing parameter set:
verbose = TRUE
var <- "WC_40"
scen_dir <- "dotnuvele/scen/"
scen_name <- "managementscenarios"
rerun = FALSE

# TODO: standarzed status QUO with an extra flag
# TODO: relative change to standard quo with extra flag
# TODO: Make sure it works for all the optain output:

# Parse out the scenarios
mgt_scen = list.dirs(paste0(scen_dir, "/", scen_name), recursive = F)
if(rerun){
  # setting all the management scenarios to have the correct variable output.

  !!! Stopped here !!!

  par_runs <- run_swap_parallel(
    project_paths = mgt_scen,
    verbose = verbose,
    autoset_output = FALSE,
    force = T
  )
  # test if all runs work=ed
  if (all(par_runs == 100)) {
    if (verbose) {
      cat(blue(italic(">> all runs suceeded << \n")))
    }
  } else{
    stop("RSWAP SCENARIO RUN: Error! some runs failed!")
  }
}else{
  if(verbose){cat(blue(italic(">> not re-running scenarios <<")))}
}


out_dir <- paste0(scen_dir, scen_name, "_rswap_parallel_results/")
# remove the extra swap.exe (TODO: why is this needed? shouldnt this be performed in the run swap paralel function?)
file.remove(paste0(out_dir, "swap.exe"))
scen_names <- list.files(path = out_dir)

# custom function to extract the variable of interest
scen_get_out <- function(scen_1) {
  output <- load_swap_output(paste0(out_dir, scen_1), verbose = verbose)$custom_depth %>% select("DATE", all_of(var))
  # TODO WARNING unstable re-naming, might break for different use-cases?!
  colnames(output) <- c("DATE", "SCEN_VAR")
  output$Scenario <- scen_1
  return(output)
}

# extract and combine the output.
listofresults <- lapply(X = scen_names, scen_get_out)
res_df <- do.call("rbind", listofresults)

# custom rswap color pallete ;)
# might remove ...
rswap_palette <- colorRampPalette(colors = c(
  "#61bbff",
  "#1f6abf",
  "#ea7000",
  "#9f2b00",
  "#00b900",
  "#375700"
))(length(mgt_scen))


p <- ggplot(res_df, aes(x = Scenario, y = SCEN_VAR, fill = Scenario)) +
  geom_boxplot() + ylab(var) +
  ggtitle(
    "rswap scenario analysis",
    paste0(var, " under following scenarios: ~", scen_dir, scen_name)
  ) +
  scale_fill_manual(values = rswap_palette) + theme(legend.position = "none") +
  theme(axis.title.x = element_blank())
p %>% print()

res_df

