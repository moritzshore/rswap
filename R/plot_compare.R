#' Compare model runs
#'
#' This graph highlights the differences between saved model runs for a given
#' variable at a certain depth.
#'
#' @seealso [rswap_plot_variable()] [rswap_plot_performance()] [rswap_plot_multi()]
#'
#' @param project_path path to project directory (string)
#' @param variable variable to show (string)
#' @param depth depth of variable, if it has a depth. Otherwise leave blank (string)
#' @param verbose print status? (flag)
#'
#' @importFrom dplyr %>%
#' @importFrom stringr str_split
#' @importFrom glue glue
#' @importFrom ggpubr color_palette
#' @importFrom plotly plot_ly
#'
#' @export
#'
#' @importFrom dplyr filter
#' @importFrom grDevices colorRampPalette
#' @importFrom plotly plot_ly add_trace layout
#' @importFrom stringr str_split
#' @importFrom utils tail
rswap_plot_compare <- function(project_path, variable, depth =  NULL, verbose = F) {
  # TODO: rename to compare_swap_runs
  # Custom color palette
  color_palette <- grDevices::colorRampPalette(c("red", "blue", "green"))

  # Grab data from last model run
  project_name <- project_path %>% stringr::str_split("./") %>% unlist() %>% utils::tail(1)

  # combine the past runs, the current run
  master_df <-
    melt_swap_runs(
      project_path = project_path,
      variable = variable,
      depth = depth,
      verbose = verbose
    )

  # find out how many total runs there are, and then create a color palette
  # with that amount of colors.
  colors_needed <- master_df$run %>% unique() %>% length()
  custom_colors <- color_palette(colors_needed)

  # create a base plot with the custom color palette
  plotly_plot <- plotly::plot_ly(colors = custom_colors)

  # add observed trace
  plotly_plot <-
    plotly_plot %>% plotly::add_trace(
      data = master_df %>% dplyr::filter(.data$tag == "observed"),
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
  plotly_plot <-
    plotly_plot %>% plotly::add_trace(
      data = master_df %>% dplyr::filter(.data$tag == "present"),
      x = ~ DATE,
      y = ~ value,
      name = "Current Run",
      opacity = 1,
      mode = "lines+marker",
      type = "scatter",
      line = list(color = "orange", width = 2)
    )

  # add all the previous runs
  plotly_plot <-
    plotly_plot %>% plotly::add_trace(
      data = master_df %>% dplyr::filter(.data$tag == "past"),
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

  # modify the layout
  plotly_plot %>% plotly::layout(
    autosize = T,
    title = paste("Comparative plot:", "<b>", project_name, "</b>", variable, depth, "cm"),
    plot_bgcolor = "white",
    hovermode = "x unified",
    xaxis = list(title = 'Date'),
    yaxis = list(title = variable),
    legend = list(title = list(text = '<b> Run </b>'))
  )
}
