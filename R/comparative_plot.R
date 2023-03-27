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
#' @param variable (REQ) (string) variable to compare
#' @param depth (OPT) (numeric) depth of variable. leave blank if variable has
#' no depth
#' @param verbose (OPT) (logical) print status?
#'
#' @importFrom grDevices colorRampPalette
#' @importFrom dplyr %>%
#' @importFrom stringr str_split
#' @importFrom glue glue
#' @importFrom ggpubr color_palette
#' @importFrom plotly plot_ly
#'
#' @export
#'
comparative_plot <-
  function(project_path,
           variable,
           depth =  NULL,
           verbose = F) {

  # a custom color pallette
  color_palette<-colorRampPalette(c("red","blue","green" ), )

  # grab data from model run
  run_name <- project_path %>% str_split("./") %>% unlist() %>% tail(1)


  # combine the past runs, the current run
  master_df <-
    melt_all_runs(
      project_path = project_path,
      variable = variable,
      depth = depth,
      verbose = verbose
    )

  # find out how many total runs there are, and then create a color palette
  # with that amount of colors.
  custom_colors = master_df$run %>% unique() %>% length() %>% color_palette(.)

  # create a base plot with the custom color palette
  plot <- plot_ly(colors = custom_colors)

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
      data = master_df %>% filter(tag == "present"),
      x = ~ DATE,
      y = ~ value,
      name = "Current Run",
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
      name = ~ run,
      opacity = 1,
      color = ~ run,
      mode = "lines",
      line = list(
        color = ~ run,
        width = 1,
        dash = 'dot'
      ),
      type = "scatter",
      visible = "legendonly"
    )

  plot %>% layout(
    autosize = T,
    title = paste("Comparative plot:", "<b>", run_name, "</b>", variable, depth, "cm"),
    plot_bgcolor = "white",
    hovermode = "x unified",
    xaxis = list(title = 'Date'),
    yaxis = list(title = variable),
    legend = list(title = list(text = '<b> Run </b>'))
  )
}
