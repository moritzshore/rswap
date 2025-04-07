#' Plot variable
#'
#' This function plots a single SWAP `variable` at any given `depth`. If no depths
#' are provided, all available depths are used. This plot can also be
#' interactive if `interactive = TRUE`
#'
#' @param project_path path to project directory `string`
#' @param variable SWAP variable to plot `string`
#' @param depth depth(s) to plot `numeric` (optional, if left blank, all available depths will be used)
#' @param interactive use an interactive plotly plot? `flag`
#' @param verbose print status? `flag`
#'
#' @export
#'
#' @examples
#' # path to sample results (only for package internal example, you don't need
#' # this function)
#' example_path <- system.file(package = "rswap", "extdata/rswap_example_output")
#'
#' # in your own projects, you would use something like this:
#' # example_path <- "C:/path/to/swap_folder/example_project"
#'
#' rswap_plot_variable(example_path, variable = "WC")
#'
#' @importFrom ggplot2 ggplot geom_line aes facet_wrap ylab ggtitle
#' @importFrom graphics plot
#' @importFrom dplyr .data
#' @importFrom plotly ggplotly
#' @importFrom stringr str_split
#' @importFrom tidyr pivot_wider
#' @importFrom utils tail
#'
#' @seealso [rswap_plot_compare()] [rswap_plot_performance()] [rswap_plot_multi()]
rswap_plot_variable <- function(project_path, variable, depth = NULL, interactive = F, verbose = F) {

  if(!interactive){
    #install_missing_packs("ggbraid")
    # I am really not sure if this is best practice...?
    if("ggbraid" %in% utils::installed.packages() == FALSE){
      cat("package ggbraid from github is needed for this plot, installing now\n")
      devtools::install_github("nsgrantham/ggbraid")
      if("ggbraid" %in% utils::installed.packages() == FALSE){
        stop("error installing ggbraid!")
      }else{
        cat("[rswap]: ggbraid succesfully installed!\n")
      }
    }
  }



  proj <- project_path %>% stringr::str_split("/") %>% unlist() %>% utils::tail(1)
  swap_data <- melt_swap_data(project_path, variable, depth, verbose)
  swap_data$type[which(swap_data$type == "mod")] = "SWAP"
  swap_data$type[which(swap_data$type == "obs")] = "Measured"

  depth_number <- swap_data$depth %>% unique() %>% length()
  df_wide <- tidyr::pivot_wider(swap_data, names_from = .data$type, values_from = .data$value)

  if(!interactive){
    g_plot <- ggplot2::ggplot() + ggplot2::geom_line(ggplot2::aes(.data$DATE, .data$value, linetype = .data$type), data = swap_data) +
      ggplot2::facet_wrap( ~ depth, nrow = depth_number, labeller = "label_both")+
      ggbraid::geom_braid(ggplot2::aes(.data$DATE, ymin = .data$Measured, ymax = .data$SWAP, fill = .data$Measured < .data$SWAP),
                          data = df_wide, alpha = 0.6, method = "line", na.rm = FALSE, show.legend = F)+
      ggplot2::ylab(variable)+ggplot2::ggtitle(paste0("Project ",proj), paste0("variable = ", variable ))+
      theme_bw()+
      scale_x_date(date_labels="%b-%y",date_breaks  ="1 month")+
      theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))+
      labs(linetype = "")+
      theme(legend.position = c(.94, 1.19))
    g_plot %>% graphics::plot()
  }

  if(interactive){
    i_plot <- ggplot2::ggplot() + ggplot2::geom_line(ggplot2::aes(.data$DATE, .data$value, color = .data$type), data = swap_data) +
      ggplot2::facet_wrap( ~ depth, nrow = depth_number)+
      ggplot2::ylab(variable)+ggplot2::ggtitle(paste(proj, variable))

    i_plot %>% plotly::ggplotly()
  }

}
