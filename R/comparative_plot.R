# comparative plot

# SWAPUI comparative plot
# Author: Moritz Shore | moritz.shore@nibio.no | 9.1.23
# Contents: this graph shows the current model run compared to the previous saved
# runs. It highlights the differences between changed parameters etc.

#' Comparative Plot
#' this graph shows the current model run compared to the previous saved
#' runs. It highlights the differences between changed parameters etc.
#'
#' @param project_path (REQ) (string) path to project directory
#' @param observed_file_path (OPT) (string) path to observed file, in case it is
#' not located in the default location
#' @param vars (REQ) (string) variable compare
#' @param depth (OPt) (numeric) depth of variable. leave blank if variable has
#' no depth
#' @param verbose (OPT) (logical) print status?
#'
comparative_plot <- function(project_path, observed_file_path, vars, depth =  NULL, verbose = NULL) {

  # supported non-depth variables
  NODEPTH  = user.var %in% c("DRAINAGE")

  if(NODEPTH==FALSE & is.na(user.depth)){
    return("please enter depth for a depthwise variable!")
  }


  # a custom color pallette
  color_palette<-colorRampPalette(c("red","blue","green" ), )

  # grab data from model run
  run_name = data[[1]]
  full_df = data[[2]]

  # combine the past runs, the current run
  master_df <- melt_all_runs(field, data, user.var)

  # return NA if there was an issue creating a master data frame
  if(!is.data.frame(master_df)){return(NA)}

  # find out how many total runs there are, and then create a color palette
  # with that amount of colors.
  custom_colors = master_df$path %>% unique() %>% length() %>% color_palette(.)

  # create a base plot with the custom color palette
  plot <- plot_ly(colors = custom_colors)

  # if depth is specified, then filter by it.
  if(!is.na(user.depth)){
    master_df = master_df %>% filter(depth == user.depth)
  }

  # add observed WC Trace
  plot <-
    plot %>% add_trace(
      data = master_df %>% filter(tag == "observed"),
      x = ~ DATE,
      y = ~ value,
      name = "observed",
      opacity = 1,
      mode = "lines+markers",
      type = "scatter",
      line = list(color = "black"),
      marker = list(
        color = "blue",
        line = list(color = "black", width = 1)
      )
    )

  # add the most recent run
  plot <-
    plot %>% add_trace(
      data = master_df %>% filter(tag == "current"),
      x = ~ DATE,
      y = ~ value,
      name = run_name,
      opacity = 1,
      mode = "lines+marker",
      type = "scatter",
      line = list(color = "orange", width = 2)
    )

  # add all the previous runs
  plot <-
    plot %>% add_trace(
      data = master_df %>% filter(tag == "past"),
      x = ~ DATE,
      y = ~ value,
      name = ~ path,
      opacity = 1,
      color = ~ path,
      mode = "lines",
      line = list(
        color = ~ path,
        width = 1,
        dash = 'dot'
      ),
      type = "scatter",
      visible = "legendonly"
    )

  plot %>% layout(
    autosize = T,
    title = paste("Comparative plot:", "<b>", run_name, "</b>", user.var, user.depth, "cm"),
    plot_bgcolor = "white",
    hovermode = "x unified",
    xaxis = list(title = 'Date'),
    yaxis = list(title = user.var),
    legend = list(title = list(text = '<b> Run </b>'))
  ) %>% return()
}
