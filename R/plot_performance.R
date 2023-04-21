#' Plot performance metrics
#'
#' This function calculates the statistical performance of all the saved model
#' runs in the /rswap_saved/ folder, as well as the last performed run. It does
#' this per `variable` and for any/all given `depths`, for the passed statistical
#' performance indicator `stat` (Currently supported are "NSE", "RMSE", "PBIAS", "RSR")
#'
#' @param project_path path to project directory (string)
#' @param var variable to show (string)
#' @param depth depth(s) to show. Leave blank for all (numeric/vector)
#' @param graph either "default" "sorted" or "ggplot" leave blank for default (string)
#' @param stat statistical indicator function ("NSE", "RMSE", "PBIAS", "RSR") (string)
#' @param verbose print status? (flag)
#'
#' @importFrom ggplot2 ggplot theme scale_color_manual geom_col aes geom_line geom_point labs ylab xlab ggtitle scale_fill_manual
#' @importFrom dplyr %>% pull select all_of filter
#' @importFrom stringr str_split
#' @importFrom glue glue
#' @importFrom plotly plot_ly add_trace layout hide_colorbar
#'
#' @export
#'
rswap_plot_performance <-
  function(project_path, var, depth =  NULL, graph = "default",stat = "NSE",
           verbose = F) {

  graph = graph %>% tolower()

  if (graph %in% c("ggplot", "default", "sorted") == FALSE) {
      graph = "default"
      warning("graph type not recognized! supported are:\n 'default', 'sorted', ggplot'")
    }

  # extract the run name from the data package
  current_run <- project_path %>% str_split("./") %>% unlist() %>% tail(1)
  save_path <- glue("{project_path}/rswap_saved/")
  obs_dat <- load_observed(project_path)

  if(depth %>% is.null()){
   depth <- get_depths(data = obs_dat$data, variable = var)
  }

  # get a path list of all the previous runs
  # get the file infos to sort by creation date
  files = list.files(path = save_path, full.names = T)
  details = files %>% file.info()
  files <- files[order(details$mtime, decreasing = T)]

  stat_df <- data.frame()

  for (file in files) {

    # extract the run name from the data package
    run_name <- file %>% str_split("./") %>% unlist() %>% tail(1)

    if(length(stat)>1){stop("rswap ERROR: please pass only one stat value")}

      performance = get_performance(
        project_path = file,
        archived = TRUE,
        stat = stat,
        variable = var,
        depth = depth,
        verbose = verbose
      )

      mean_perf <- performance[2] %>% pull() %>% mean() %>% round(4)

      performance$run = run_name
      performance$mean = mean_perf

      stat_df <- rbind(stat_df, performance)
  }

  last_run <- get_performance(
    project_path = project_path,
    stat = stat,
    variable = var,
    depth = depth,
    verbose = verbose
  )

  last_run$run = "last_run"
  last_run$mean = last_run %>% select(all_of(stat)) %>% pull() %>% mean()

  stat_df <- rbind(stat_df, last_run)

  # color palette
  palette <- colorRampPalette(c("dodgerblue4","dodgerblue2","deepskyblue"))

  # sort the runs by user stat
  if(stat == "NSE"){
    best_perf = stat_df$mean %>% max() %>% min()
    best_run = stat_df$run[which(stat_df$mean==best_perf) %>% min()]
  }else if(stat %in% c("RMSE", "PBIAS", "RSR")){
    best_perf = stat_df$mean %>% min() %>% min()
    best_run = stat_df$run[which(stat_df$mean==best_perf) %>% min()]
  }else{
    stop(stat, "not recognized")
  }

  # interactive default plot
  if (graph == "default") {
    fig1 <- plot_ly()

    # dont need columns if not depthwise
    if(depth %>% is.null() == FALSE){
      num_colors = stat_df$var %>% unique() %>% length()
      # columns of NSE value per depth
      fig1 <-
        fig1 %>% add_trace(
          data = stat_df,
          color = ~ var,
          colors = palette(num_colors),
          x = ~ run,
          y = ~ stat_df[2] %>% pull(),
          name = ~ var,
          type = 'bar'
        )
    }

    # mean of columns as a line
    fig1 <-
      fig1 %>% add_trace(
        data = stat_df,
        x = ~ run,
        y = ~  stat_df$mean,
        name = "mean",
        mode = "lines+markers",
        type = "scatter"
      )
    # layout settings
    fig1 %>% layout(
      title = paste("Statistics for", var),
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
      hide_colorbar() %>%
      # print out the finsihed plot
      print()
  }

  # plotted sorted to best user.stat
  if(graph=="sorted"){

    stat_df_ordered <- stat_df[order(stat_df$mean, decreasing = F),]

    # plot
    fig2 <- plot_ly()
    # individual columns for each depth
    if(depth %>% is.null() == FALSE){
      fig2 <-
        fig2 %>% add_trace(
          data = stat_df_ordered,
          x = ~ run,
          y = ~ stat_df_ordered[[2]],
          name = ~ var,
          color = ~ var,
          colors = palette(length(get_depths(obs_dat$data, var))),
          type = 'bar'
        )
    }
    # mean line
    fig2 <-
      fig2 %>% add_trace(
        data = stat_df_ordered,
        x = ~ run,
        y = ~ mean,
        name = "mean",
        mode = "lines+markers",
        type = "scatter"
      )


    # cutoms layout and print
    fig2 %>% layout(
      title = paste("Statistics for", var, "sorted by best mean", stat),
      autosize = T,
      xaxis = list(categoryorder = "array",
                   categoryarray = stat_df_ordered$run %>% unique()),
      yaxis = list(
        title = stat,
        zerolinecolor = '#ffff',
        zerolinewidth = 2,
        gridcolor = 'ffff'
      ),
      plot_bgcolor = '#e5ecf6'
    ) %>%
      hide_colorbar() %>%
      print()
  }

  # ggplot
  if(graph=="ggplot") {

    # find the best value
    if(stat == "NSE"){
      best_value <- stat_df$mean %>% max()
      best_run_name <- stat_df$run[which(stat_df$mean == best_value) %>% min()]

    }else if(stat=="PBIAS"){
      best_value_abs <- stat_df$mean %>% abs() %>% min()
      best_value <- stat_df$mean[which(stat_df$mean %>% abs() == best_value_abs) %>% min()]
      best_run_name <- stat_df$run[which(stat_df$mean %>% abs() == best_value_abs) %>% min()]
    }else{
      best_value <- stat_df$mean %>% min()
      best_run_name <- stat_df$run[which(stat_df$mean == best_value) %>% min()]

    }

    # find the value of the last path
    last_run_value <- stat_df %>% filter(run == "last_run") %>%  select(all_of(stat)) %>% pull() %>% mean()
    # color red if best path
    ifbest <- ifelse(unique(stat_df$run) == best_run_name, "red", "black")
    # format bold if last path
    iflast <- ifelse(unique(stat_df$run) == "last_run", "bold", "plain")

      # theme changes (x axis formatting and subtitle)
      # this is where the warning "Vectorized input to `element_text()` is not officially supported."
      # comes into play, but no fix can be found.
      plot<-ggplot(stat_df)+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, colour = ifbest, face = iflast),
            plot.subtitle = element_text(color = "red"))+

      # scale color manual
      scale_color_manual(values = c("red"))+
      # scale fill according to our palette
      scale_fill_manual(values=palette(length(get_depths(obs_dat$data, var))))+
      # y lab
      ylab(stat)+xlab("run")+
      ggtitle(paste0("Last run had a(n) ", stat, " of ",round(last_run_value,2)),
              paste0( "The best run was \"", best_run_name,"\", with a(n) " ,stat, " of ", round(best_value,2)))


    if(depth %>% is.null() == FALSE){
      # dodge columns for the depths
      plot <- plot+geom_col(aes(x=factor(run, level = run %>% unique()), y = stat_df[2] %>% pull(), fill = var), position = "dodge")
    }

    # mean line
    plot = plot+geom_line(aes(x=factor(run, level = run %>% unique()), y =mean, color = "mean"), group = 1, linewidth = 1)+
      # points on mean line
      geom_point(aes(x = run, y = mean, color = "mean"), size = 2)

    # Warning message:
    #   Vectorized input to `element_text()` is not officially supported.
    # ℹ Results may be unexpected or may change in future versions of ggplot2.
    plot %>% print()
  }
}

# TODO: sorting of the results NEEDS to react to the stat functions setting!