#' Plot performance metrics
#'
#' This function calculates the statistical performance of all the saved model
#' runs in the /rswap_saved/ folder, as well as the last performed run. It does
#' this per `variable` and for any/all given `depths`, for the passed
#' statistical performance indicator `stat` from package
#' [hydroGOF](https://cran.r-project.org/web/packages/hydroGOF/hydroGOF.pdf)
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
#' @param variable variable to show (string)
#' @param depth depth(s) to show. Leave blank for all (numeric/vector)
#' @param stat a statistical indicator function from package `hydroGOF` (string)
#' @param verbose print status? (flag)
#'
#' @importFrom dplyr %>% pull select all_of filter
#' @importFrom stringr str_split
#' @importFrom glue glue
#' @importFrom plotly plot_ly add_trace layout hide_colorbar
#' @importFrom grDevices colorRampPalette
#' @importFrom utils tail
#'
#' @export
#'
rswap_plot_performance <-
  function(project_path, variable, stat, depth =  NULL, verbose = FALSE) {

  # extract the run name from the data package
  current_run <- project_path %>% stringr::str_split("./") %>% unlist() %>% utils::tail(1)
  save_path <- glue::glue("{project_path}/rswap_saved/")
  obs_dat <- load_swap_observed(project_path)

  if(depth %>% is.null()){
   depth <- get_swap_depths(data = obs_dat$data, variable = variable)
  }

  # get a path list of all the previous runs
  # get the file infos to sort by creation date
  files = list.files(path = save_path, full.names = T)
  details = files %>% file.info()
  files <- files[order(details$mtime, decreasing = T)]

  stat_df <- data.frame()

  for (file in files) {

    # extract the run name from the data package
    run_name <- file %>% stringr::str_split("./") %>% unlist() %>% utils::tail(1)

      performance = get_swap_performance(
        project_path = file,
        archived = TRUE,
        stat = stat,
        variable = variable,
        depth = depth,
        verbose = verbose
      )

      mean_perf <- performance[2] %>% dplyr::pull() %>% mean() %>% round(4)

      performance$run = run_name
      performance$mean = mean_perf

      stat_df <- rbind(stat_df, performance)
  }

  last_run <- get_swap_performance(
    project_path = project_path,
    stat = stat,
    variable = variable,
    depth = depth,
    verbose = verbose
  )

  last_run$run = "last_run"
  last_run$mean = last_run %>% dplyr::select("value") %>% dplyr::pull() %>% mean()

  stat_df <- rbind(stat_df, last_run)

  # color palette
  palette <- grDevices::colorRampPalette(c("dodgerblue4","dodgerblue2","deepskyblue"))

    fig1 <- plotly::plot_ly()

    # don't need columns if not depth wise
    if(depth %>% is.null() == FALSE){
      num_colors = stat_df$variable %>% unique() %>% length()
      # columns of stat value per depth
      fig1 <-
        fig1 %>% plotly::add_trace(
          data = stat_df,
          color = ~ variable,
          colors = palette(num_colors),
          x = ~ run,
          y = ~ stat_df[2] %>% dplyr::pull(),
          name = ~ variable,
          type = 'bar'
        )
    }

    # mean of columns as a line
    fig1 <-
      fig1 %>% plotly::add_trace(
        data = stat_df,
        x = ~ run,
        y = ~  stat_df$mean,
        name = "mean",
        mode = "lines+markers",
        type = "scatter"
      )
    # layout settings
    fig1 %>% plotly::layout(
      title = paste("Statistics for", variable),
      plot_bgcolor = '#e5ecf6',
      autosize = T,
      xaxis = list(categoryorder = "array",
                   categoryarray = stat_df$run %>% unique()),
      yaxis = list(
        title = stat,
        zerolinecolor = '#ffff',
        zerolinewidth = 2,
        gridcolor = 'ffff'
      )
    ) %>%
      # remove the colorbar legend
      plotly::hide_colorbar() %>%
      # print out the finsihed plot
      print()
  }
