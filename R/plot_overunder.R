# Hacked together plot showing over/under estimates of the model
# compared to measurements. used function ribbonize() from nsgrantham
# Poor quality method, should be improved someday.

# TODO: fix x axis
# TODO: allow NA values..
# ...maybe try to write it myself...

# source: https://www.nsgrantham.com/fill-between-two-lines-ggplot2

#' ribbionize
#'
#' author: Neal Grantham
#' source: https://www.nsgrantham.com/fill-between-two-lines-ggplot2
#' @keywords internal
#' @importFrom dplyr %>% pull filter count group_by mutate select distinct
#' @importFrom dplyr transmute arrange recode lead bind_rows
#' @importFrom tidyr pivot_wider pivot_longer
#'
#' @author Neal Grantham
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


#' Plot Over/Underestimation graph
#'
#' Plots a graph showing where the model overestimates and underestimates for
#' the given variable and depth(s).
#'
#' Graph is based on [ribbonize()](https://www.nsgrantham.com/fill-between-two-lines-ggplot2) from Neal Grantham
#'
#' @param project_path path to project_directory (string)
#' @param variable desired variable (string)
#' @param depth optional depth specification. leave blank for no/all depths. (numeric/vector)
#' @param verbose print status? (flag)
#'
#' @importFrom ggplot2 ggplot geom_line geom_ribbon guides scale_color_manual
#' @importFrom ggplot2 scale_linetype_manual theme element_text element_blank scale_x_discrete labs aes
#' @importFrom plotly ggplotly layout
#' @importFrom dplyr %>% left_join pull count group_by
#'
#' @export
#'
#' @author Neal Grantham, Moritz Shore
rswap_plot_overunder <- function(project_path, variable, depth = NULL, verbose = F) {

  # grabs the model data

  run_name <- project_path %>% str_split("/") %>% unlist() %>% tail(1)

  observed_data <- load_observed(project_path, verbose = verbose)

  if(depth %>% is.null()){
    depth = get_depths(observed_data$data, variable = variable)
  }

  index = 1
  total = length(depth)

  # special routine if depth is null
  if(depth %>% is.null()){
    rlist <-
      match_mod_obs(
        project_path = project_path,
        variable = variable,
        verbose = verbose
      )
    modelled_data_filtered = rlist$mod
    observed_data_filtered = rlist$obs

    res_crop <- left_join(observed_data_filtered, modelled_data_filtered, by  = "DATE")

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
      labs(y =variable)

    p %>% ggplotly(tooltip = "y" )  %>%
      layout(title = list(text = paste(run_name, variable), y = 0.99), hovermode = "x unified") %>%
      print()
  }else{
    for (cdepth in depth) {

      rlist <-
        match_mod_obs(
          project_path = project_path,
          variable = variable,
          depth = cdepth,
          verbose = verbose
        )
      modelled_data_filtered = rlist$mod
      observed_data_filtered = rlist$obs

      res_crop <- left_join(observed_data_filtered, modelled_data_filtered, by  = "DATE")

      # rename the columns
      colnames(res_crop) <- c("DATE", "mod", "obs")

      # reformat the data to be compatible with the ribbonize function
      mydf <- tibble(
        x = c(1:length(res_crop$DATE),1:length(res_crop$DATE)),
        y = c(res_crop$mod, res_crop$obs),
        type = c(rep("model", length(res_crop$DATE)), rep("observed", length(res_crop$DATE)))
      )

      if(any(mydf$y %>% is.na())){
        stop("this graph currently doesnt support NA-values... I am working on it.")
      }

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
        labs(y = variable)

      p %>% ggplotly(tooltip = "y")  %>%
        layout(title = list(text = paste(run_name, variable, "depth:", cdepth, "cm"), y = 0.99), hovermode = "x unified") %>%
        print()

      # todo, clean up the graph
      # todo tidy up this readline thing
      if(index < total){
        index = index+1
        readline(prompt = paste0("hit enter for next depth (", depth[index], ")"))
      }
    }
  }
}
