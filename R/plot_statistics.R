# SWAPUI statistics plotting
# Author: Moritz Shore | moritz.shore@nibio.no | 9.1.23
# Contents: This function generates a plot of all the saved runs, allowing the 
#           user to see how well their runs are performing compared to previous
#           attempts. 


plot_statistics <- function(field, data, user.var = "WC", user.stat = "NSE", graph = "default"){

# data processing -----
  
  # if the melt function doesn't work, return NA
  if(!is.data.frame(melt_all_runs(field, data, user.var = user.var))){return(NA)}
  
  
  # check if there is more than 1 observed data point. if not, return NA
  if(data[[3]][1] %>% pull() %>% length() <= 1){
    print("no observed data, cannot display statistics")
    return(NA)
  }
  
  # melt the previous runs together
  master <- melt_all_runs(field, data, user.var) %>% 
  # and remove the values which are NA
  filter(is.na(value)==FALSE)

  # extract the run name from the data package
  last_run = data[[1]]

  # extract the observed values from the master df
  observed <- master %>% filter(tag=="observed")
  colnames(observed)[5] <- "obsval"  # prone to failure!

  # extract the rest of the data
  modelled <-  master %>% filter(tag!="observed") %>%
    # but only that within the daterange
    filter(DATE %in% observed$DATE)

  if(is.na(master$depth[1])){
    # attach the respective observed value for each modeled one (double check this)
    merged <- left_join(x = modelled, y = observed %>% select(DATE, obsval), by = c("DATE"))
  }else{# depthwise if depthwise
    merged <- left_join(x = modelled, y = observed %>% select(DATE, obsval, depth), by = c("DATE", "depth"))
  }
  


  # get a path list of all the previous runs
  files = list.files(path = paste0(field, "/output"), full.names = T)
  # get the file infos to sort by creation date
  details = files %>% file.info() 

  # turn off dyplr summarise
  options(dplyr.summarise.inform = FALSE)


  # get the statistics for each model run, for each depth
  all_stat <-
    merged %>% group_by(path, depth) %>% summarise(
      NSE = NSE(mod = value, obs = obsval),
      PBIAS = PBIAS(mod = value, obs = obsval),
      RMSE = RMSE(mod = value, obs = obsval),
      RSR = RSR(mod = value, obs = obsval))

  # get the average statistics
  mean_stat <-
    all_stat %>% group_by(path) %>% summarise(
      NSE = mean(NSE),
      PBIAS = mean(PBIAS),
      RMSE = mean(RMSE),
      RSR = mean(RSR)
    )

  # turn on again: dyplr summarise
  options(dplyr.summarise.inform = TRUE)
  
  # color palette
  palette <- colorRampPalette(c("dodgerblue4","dodgerblue2","deepskyblue"))

  # sort the runs by user stat
  best_run = mean_stat[order(mean_stat[[which(colnames(mean_stat) == user.stat)]],
                             decreasing = F),]
  

  # plotting -----
  
  # interactive default plot
 
  if (graph == "default") {
    fig1 <- plot_ly()
    # dont need columns if not depthwise
    if(!is.na(merged$depth[1])){
    # columns of NSE value per depth
    fig1 <-
      fig1 %>% add_trace(
        data = all_stat,
        color = ~ depth,
        colors = palette(length(get_depths(data[[2]]))),
        x = ~ path,
        y = ~ get(user.stat),
        name = ~ depth,
        type = 'bar'
      )
    }
    # mean of columns as a line
    fig1 <-
      fig1 %>% add_trace(
        data = mean_stat,
        x = ~ path,
        y = ~ get(user.stat),
        name = "mean",
        mode = "lines+markers",
        type = "scatter"
      )
    # layout settings
    fig1 %>% layout(
      title = paste("Statistics for", user.var),
      plot_bgcolor = '#e5ecf6',
      autosize = T,
      yaxis = list(
        title = user.stat,
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