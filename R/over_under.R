# SWAPUI Over/underestimation graph
# Author: Moritz Shore | moritz.shore@nibio.no | 9.1.23
# Contents: Hacked together plot showing over/under estimates of the model
#           compared to measurements. used function ribbonize() from nsgrantham
#           Poor quality method, should be improved someday.


# source: https://www.nsgrantham.com/fill-between-two-lines-ggplot2
ribbonize <- function(.data, .x, .y, .type) {
  # Calculate the ribbons required for geom_ribbon().
  # For more info, visit nsgrantham.com/fill-between-two-lines-ggplot2
  #
  # Usage:
  # df <- tibble(
  #   x = c(1:8, 1:8),
  #   y = c(1, 5, 6, 4, 1, 1, 3, 2, 1, 4, 5, 4, 2, 2, 2, 2),
  #   type = c(rep("model", 8), rep("observed", 8))
  # )
  #
  # ribbons <- ribbonize(df, x, y, type)
  #
  # ggplot(df) +
  #   geom_line(aes(x, y, linetype = type)) +
  #   geom_ribbon(data = ribbons, aes(x, ymin = ymin, ymax = ymax, fill = fill))
  
  # Check there are only 2 level in .type
  levels <- .data %>%
    pull({{ .type }}) %>%
    unique()
  
  stopifnot(length(levels) == 2)
  
  # Check that there is exactly 1 observation per level in .type at every .x
  level_counts_by_x <- .data %>%
    filter(!is.na({{ .y }})) %>%
    group_by({{ .x }}) %>%
    count() %>%
    pull(n)
  
  stopifnot(all(level_counts_by_x == 2))
  
  bounds <- .data %>%
    mutate({{ .type }} := recode({{ .type }}, model = levels[1], observed = levels[2])) %>%
    pivot_wider(names_from = {{ .type }}, values_from = {{ .y }}) %>%
    mutate(
      ymax = pmax(model, observed),
      ymin = pmin(model, observed),
      fill = model >= observed
    )
  
  intervals <- bounds %>%
    filter(ymax > ymin) %>%
    select(-model, -observed)
  
  intersections <- bounds %>%
    mutate(lag_fill = lag(fill), lead_fill = lead(fill)) %>%
    filter(ymax == ymin) %>%
    select(-model, -observed, -fill) %>%
    pivot_longer(lag_fill:lead_fill, names_to = NULL, values_to = "fill") %>%
    filter(!is.na(fill)) %>%
    distinct()
  
  other_intersections <- bounds %>%
    transmute(
      x1 = {{ .x }},       y1 = model,
      x2 = lead({{ .x }}), y2 = lead(model),
      x3 = {{ .x }},       y3 = observed,
      x4 = lead({{ .x }}), y4 = lead(observed)
    ) %>%
    filter(((y1 > y3) & (y2 < y4)) | ((y1 < y3) & (y2 > y4))) %>%
    mutate(
      d = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4),
      u = x1 * y2 - y1 * x2,
      v = x3 * y4 - y3 * x4,
      x = (u * (x3 - x4) - v * (x1 - x2)) / d,
      y = (u * (y3 - y4) - v * (y1 - y2)) / d
    ) %>%
    select(x, ymax = y, ymin = y)
  
  bind_rows(
    intervals,
    intersections,
    mutate(other_intersections, fill = TRUE),
    mutate(other_intersections, fill = FALSE)
  ) %>%
    arrange({{ .x }})
}

# hacked together to get ribbonize() to work on it, but its far from pretty.

plot_over_under <- function(field, data, user.var = "WC", user.depth = NA) {

  # grabs the model data
  run_name = data[[1]]
  res = data[[2]]
  NODEPTH  = user.var %in% c("DRAINAGE")
  
  if(NODEPTH==FALSE & is.na(user.depth)){
    return("please enter depth for a depthwise variable!")
  }
  
  # supported non depthwise variables
  if(NODEPTH){
    # find which column contains the observed/modelled data
    obs_index <- colnames(res) %>% grepl(x=., paste0("obs",user.var)) %>% which()
    mod_index <- colnames(res) %>% grepl(x=., paste0("\\b",user.var,"\\b")) %>% which()
    if(length(obs_index)==0){return(paste("no obs data found for", user.var))}
    if(length(mod_index)==0){return(paste("no modelled data found for", user.var))}
  }else{ # otherwise, do it normally
    # find which column contains the observed/modelled data
    obs_index <- colnames(res) %>% grepl(x=., paste0("obs",user.var,"_",user.depth)) %>% which()
    mod_index <- colnames(res) %>% grepl(x=., paste0("\\b",user.var,"_",user.depth,"\\b")) %>% which()
    # if no column is found, return error.
    if(length(obs_index)==0){return(paste("no obs data found for", user.var, "depth:", user.depth))}
    if(length(mod_index)==0){return(paste("no modelled data found for", user.var, "depth:", user.depth))}
  }
  
 
  # filter the data so that only the timeframe with observed data is retained.
  res_filt <- res[which(!is.na(res[obs_index])),]
  
  # drop all the extra data other than the one variable we are interesed in
  # c(1) is the DATE column, always!
  res_crop <- res_filt[c(1, mod_index, obs_index)]
  
  # rename the columns
  colnames(res_crop) <- c("DATE", "mod", "obs")
  
  # reformat the data to be compatible with the ribbonize function
  mydf <- tibble(
    x = c(1:length(res_crop$DATE),1:length(res_crop$DATE)),
    y = c(res_crop$mod, res_crop$obs),
    type = c(rep("model", length(res_crop$DATE)), rep("observed", length(res_crop$DATE)))
  )
  
  # generate the ribbons
  ribbons <- ribbonize(mydf, x, y, type)
  
  # replace the x axis with the date (must be in character format, no support
  # for date...)
  mydf2 <-
    tibble(
      x = as.character(c(res_crop$DATE, res_crop$DATE)),
      y = c(res_crop$mod, res_crop$obs),
      type = mydf$type
    )
  
  # generate the plot. GGPLOT wont show linetype correctly so we need to pass
  # it to plotly which does.
  p <- ggplot(mydf2) +
    geom_line(aes(x, y, linetype = type, color = type, group = type)) +
    geom_ribbon(data = ribbons, aes(x, ymin = ymin, ymax = ymax, fill = fill), alpha = 0.4) +
    guides(linetype = "none", fill = "none") +
    scale_color_manual(values = c("black","black"))+
    scale_linetype_manual(values = c(3, 1)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    theme(legend.title = element_blank())+
    scale_x_discrete(name = "Date", breaks = mydf2$x[mydf2$x %>% substr(.,8,10) %>% grepl(x = ., "-01") %>% which()])+
    labs(y = user.var)
  
  if(NODEPTH){
    p=p+ggtitle(paste0(field,": ", run_name, "\nvar:", user.var))
  }else{p=p+ggtitle(paste0(field,": ", run_name,"\ndepth: ",user.depth,"cm"))}
  
  p %>% ggplotly(tooltip = "none")  %>% layout(hovermode = "text",
                                               legend = list(orientation = "h", y = 1.07, x = .6)) %>% return()
}
                