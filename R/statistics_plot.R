# plot statistics

# SWAPUI statistics plotting
# Author: Moritz Shore | moritz.shore@nibio.no | 9.1.23
# Contents: This function generates a plot of all the saved runs, allowing the
#           user to see how well their runs are performing compared to previous
#           attempts.
#'
#' @importFrom grDevices colorRampPalette
plot_statistics <-
  function(project_path,
           var,
           depth =  NULL,
           graph = "default",
           stat = "NSE",
           observed_file_path = NULL,
           custom_save_path = NULL,
           verbose = NULL) {


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

  files = list.files(path = custom_save_path, full.names = T)

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


  # get a path list of all the previous runs
  # get the file infos to sort by creation date
  details = files %>% file.info()

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
    if(!is.na(merged$depth[1])){
      fig2 <-
        fig2 %>% add_trace(
          data = all_stat,
          x = ~ path,
          y = ~ get(user.stat),
          name = ~ depth,
          color = ~ depth,
          colors = palette(length(get_depths(data[[2]]))),
          type = 'bar'
        )
    }
    # mean line
    fig2 <-
      fig2 %>% add_trace(
        data = best_run,
        x = ~ path,
        y = ~ get(user.stat),
        name = "mean",
        mode = "lines+markers",
        type = "scatter"
      )

    # cutoms layout and print
    fig2 %>% layout(
      title = paste("Statistics for", user.var, "sorted by best mean", user.stat),
      autosize = T,
      xaxis = list(categoryorder = "array",
                   categoryarray = best_run$path),
      yaxis = list(
        title = user.stat,
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
    best_value = best_run[colnames(best_run)==user.stat] %>% max(na.rm=T)
    # find the run with the best value
    best_run_name <- best_run$path[which(best_run[colnames(best_run)==user.stat] == best_value)]

    # find the value of the last path
    last_run_value <- best_run %>% filter(path == last_run) %>%  select(all_of(user.stat)) %>% pull()

    # color red if best path
    ifbest <- ifelse(unique(all_stat$path) == best_run_name, "red", "black")
    # format bold if last path
    iflast <- ifelse(unique(all_stat$path) == last_run, "bold", "plain")

    options(warn=-1)
    # ggplot
    plot = ggplot(data = all_stat)+
      # theme changes (x axis formatting and subtitle)
      # this is where the warning "Vectorized input to `element_text()` is not officially supported."
      # comes into play, but no fix can be found.
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, colour = ifbest, face = iflast),
            plot.subtitle = element_text(color = "red"))+

      # scale color manual
      scale_color_manual(values = c("red"))+
      # scale fill according to our palette
      scale_fill_manual(values=palette(length(get_depths(data[[2]]))))+
      # y lab
      ylab(user.stat)+
      ggtitle(paste0("Last run: ", last_run, " with a(n)  ", user.stat, " of ",round(last_run_value,2) ),
              paste0( "The best run was \"", best_run_name,"\", with a(n) " ,user.stat, " of ", round(best_value,2)))
    options(warn=1)


    if(!is.na(merged$depth[1])){
      # dodge columns for the depths
      plot = plot +geom_col(mapping = aes(x = path, y = get(user.stat), fill = factor(depth)), position = "dodge")
    }

    # mean line
    plot = plot+geom_line(data = mean_stat, mapping = aes(path, y = get(user.stat), color = "mean"), group = 1, size = 1)+
      # points on mean line
      geom_point(data = mean_stat, aes(x = path, y = get(user.stat), color = "mean"), size = 2)

    # Warning message:
    #   Vectorized input to `element_text()` is not officially supported.
    # ℹ Results may be unexpected or may change in future versions of ggplot2.
    plot %>% return()
  }
}
