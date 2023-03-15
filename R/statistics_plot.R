#' Plot statistics
#'
#' This function generates a plot of all the saved runs, allowing the
#' user to see how well their runs are performing compared to previous
#' attempts.
#' @param project_path (REQ) (string) path to project directory
#' @param var (REQ) (string) variable to analyse
#' @param depth (numeric/vector) (OPT) optional, leave blank for all
#' @param graph (OPT) (string) either "default" "sorted" or "ggplot". leave
#' blank for default
#' @param stat (REQ) (string) Performance statistic. (NSE, PBIAS, RMSE, RSE)
#' @param observed_file_path (OPT) (string) pass if your observed file has a custom path. leave blank for default
#' @param custom_save_path  (OPT) (string) pass if your saved runs have custom paths. leave blank for default
#' @param verbose (OPT) (boolean) print status?
#'
#' @importFrom ggplot2 ggplot theme scale_color_manual geom_col aes geom_line geom_point labs ggtitle scale_fill_manual
#' @importFrom dplyr %>% pull select all_of filter
#' @importFrom stringr str_split
#' @importFrom glue glue
#' @importFrom plotly plot_ly add_trace layout hide_colorbar
#' @importFrom grDevices palette
#'
#' @export
plot_statistics <-
  function(project_path,
           var,
           depth =  NULL,
           graph = "default",
           stat = "NSE",
           observed_file_path = NULL,
           custom_save_path = NULL,
           verbose = F) {


  # extract the run name from the data package
  current_run <- project_path %>% str_split("./") %>% unlist() %>% tail(1)

  # TODO switch this so a function, or at least a global variable?
  if (custom_save_path %>% is.null()) {
    custom_save_path <- glue("{project_path}/rswap_saved/")
  }

  # TODO switch this so a function, or at least a global variable?
  if (observed_file_path %>% is.null()) {
    observed_file_path <- glue("{project_path}/rswap_observed_data.xlsx")
  }

  if(depth %>% is.null()){
   obs_dat <- load_observed(observed_file_path)
   depth <- get_depths(data = obs_dat$data, variable = var)
  }


  # get a path list of all the previous runs
  # get the file infos to sort by creation date
  files = list.files(path = custom_save_path, full.names = T)
  details = files %>% file.info()
  files <- files[order(details$mtime, decreasing = T)]

  stat_df <- data.frame()

  for (file in files) {

    # extract the run name from the data package
    run_name <- file %>% str_split("./") %>% unlist() %>% tail(1)

      mod = read_swap_output(file, custom_path = T)
      mod_filt <- filter_swap_data(mod$custom_depth, var = var, depth = depth)

      if(length(stat)>1){stop("rswap ERROR: please pass only one stat value")}

      performance = get_performance(
        project_path = file,
        stat = stat,
        variable = var,
        depth = depth,
        observed_file_path = observed_file_path,
        verbose = verbose,
        custom_path = T
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
    observed_file_path = observed_file_path,
    verbose = verbose
  )

  last_run$run = "last_run"
  last_run$mean = last_run$NSE %>% mean()

  stat_df <- rbind(stat_df, last_run)


  # color palette
  palette <- colorRampPalette(c("dodgerblue4","dodgerblue2","deepskyblue"))

  # sort the runs by user stat
  if(stat == "NSE"){
    best_perf = stat_df$mean %>% max() %>% min()
    best_run = stat_df$run[which(stat_df$mean==best_perf) %>% min()]
  }else{
    #TODO implement behavior for others (EZ just use max instead for some)
    stop("stats other than NSE not supported YET")
  }



  # plotting -----

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

    # plot
    fig2 <- plot_ly()
    # individual columns for each depth
    if(depth %>% is.null() == FALSE){
      fig2 <-
        fig2 %>% add_trace(
          data = stat_df,
          x = ~ run,
          y = ~ stat_df[2] %>% pull(),
          name = ~ var,
          color = ~ var,
          colors = palette(length(get_depths(obs_dat$data, var))),
          type = 'bar'
        )
    }
    # mean line
    fig2 <-
      fig2 %>% add_trace(
        data = stat_df,
        x = ~ run,
        y = ~ mean,
        name = "mean",
        mode = "lines+markers",
        type = "scatter"
      )

   stat_df_ordered <- stat_df[order(stat_df$mean, decreasing = T),]

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
    best_value <- stat_df$mean %>% max()
    # find the run with the best value
    best_run_name <- stat_df$run[which(stat_df$mean == best_value) %>% min()]

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
    options(warn=1)


    if(depth %>% is.null() == FALSE){
      # dodge columns for the depths
      plot <- plot+geom_col(aes(x=factor(run, level = stat_df$run %>% unique()), y = stat_df[2] %>% pull(), fill = var), position = "dodge")
    }

    # mean line
    plot = plot+geom_line(aes(x=run, y =mean, color = "mean"), group = 1, linewidth = 1)+
      # points on mean line
      geom_point(aes(x = run, y = mean, color = "mean"), size = 2)

    # Warning message:
    #   Vectorized input to `element_text()` is not officially supported.
    # ℹ Results may be unexpected or may change in future versions of ggplot2.
    plot %>% print()
  }
}
