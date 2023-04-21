#' Plot variable
#'
#' This function plots a single SWAP `variable` at any given `depth`. If no depths
#' are provided, all available depths are used. This plot can also be
#' interactive if `interactive = TRUE`
#'
#' The non-interactive version of this plot uses `ggbraid`, which the function
#' will attempt to install from github if it is not already installed.
#'
#' @param project_path path to project directory `string`
#' @param variable SWAP variable to plot `string`
#' @param depth depth(s) to plot `numeric` (optional, if left blank, all available depths will be used)
#' @param interactive use an interactive plotly plot? `flag`
#' @param verbose print status? `flag`
#'
#' @export
#'
#' @example man/examples/ex_rswap_plot_variable.R
#'
#'
#' @importFrom ggplot2 ggplot geom_line aes facet_wrap ylab ggtitle
#' @importFrom graphics plot
#' @importFrom plotly ggplotly
#' @importFrom remotes install_github
#' @importFrom stringr str_split
#' @importFrom tidyr pivot_wider
#' @importFrom utils installed.packages tail
rswap_plot_variable <- function(project_path, variable, depth = NULL, interactive = F, verbose = F) {

  if(!interactive){
    # I am really not sure if this is best practice...?
    if("ggbraid" %in% utils::installed.packages() == FALSE){
      cat("package ggbraid from github is needed for this plot, installing now\n")
      remotes::install_github("nsgrantham/ggbraid")
      if("ggbraid" %in% utils::installed.packages() == FALSE){
        stop("error installing ggbraid!")
      }else{
        cat("[rswap]: ggbraid succesfully installed!\n")
      }
    }
  }

  proj <- project_path %>% stringr::str_split("/") %>% unlist() %>% utils::tail(1)
  swap_data <- melt_swap_data(project_path, variable, depth, verbose)
  depth_number <- swap_data$depth %>% unique() %>% length()
  df_wide <- tidyr::pivot_wider(swap_data, names_from = type, values_from = value)

  if(!interactive){
    g_plot <- ggplot2::ggplot() + ggplot2::geom_line(ggplot2::aes(DATE, value, linetype = type), data = swap_data) +
      ggplot2::facet_wrap( ~ depth, nrow = depth_number)+
      ggbraid::geom_braid(ggplot2::aes(DATE, ymin = obs, ymax = mod, fill = obs < mod),
                          data = df_wide, alpha = 0.6, method = "line")+
      ggplot2::ylab(variable)+ggplot2::ggtitle(paste(proj, variable))

    g_plot %>% graphics::plot()
  }

  if(interactive){
    i_plot <- ggplot2::ggplot() + ggplot2::geom_line(ggplot2::aes(DATE, value, color = type), data = swap_data) +
      ggplot2::facet_wrap( ~ depth, nrow = depth_number)+
      ggplot2::ylab(variable)+ggplot2::ggtitle(paste(proj, variable))

    i_plot %>% plotly::ggplotly()
  }

}
