# TODO add cum. drain (could be better in dedicated graph)
# TODO add total water (probably best in custom graph)
# TODO make the 3 chosen variables totally flexible!
  # -> requires more work in utils.R

#' Plot multiple variables at once
#'
#' Generates a 1-4 axis plot from desired variables. Designed to help
#' user understand how the model is working, how the variables are interacting.
#'
#' currently supports "H", "WC", "DRAINAGE", "TEMP". Working on expanding
#'
#' @param project_path path to project directory (string)
#' @param vars list of variables to include in the plot (string/vector)
#' @param show list of variables to show by default (string/vector)
#' @param verbose print status? (flag)
#'
#' @importFrom dplyr %>% left_join
#' @importFrom plotly plot_ly layout add_trace
#' @importFrom glue glue
#' @importFrom stringr str_split
#' @importFrom grDevices colorRampPalette
#'
#' @seealso [rswap_plot_variable()] [rswap_plot_compare()] [rswap_plot_performance()]
#'
#' @export
#'
rswap_plot_multi <- function(project_path, vars, show = NULL, verbose = F){

  # TODO rename to plot_swap_multivar()?

  vars <- vars %>% toupper()

  # RAIN present if show is left blank
  if(show %>% is.null){
    show = "RAIN"
  }else{
    show <- show %>% toupper()
  }

  observed_file_path <- glue("{project_path}/rswap_observed_data.csv")

  # cant do more than 4 variables. (3+RAIN)
  if(length(vars)>3){return("too many variables, max 3")}

  # check to see if any of the entered vars are not supported
  if(!(vars %in% c("H", "WC", "DRAINAGE", "TEMP")) %>% any()){
    print("at least one not supported variable. supported variables are:")
    return(c("H", "WC", "DRAINAGE", "TEMP"))
  }

  # adds the unit to the ylab labels
  # TODO convert this to the utils function!
  var_lab = vars
  for (i in c(1:length(vars))) {
    if( vars[i]  == "TEMP" ){
      var_lab[i] = "TEMP [C]"
    }
    if( vars[i]  == "H" ){
      var_lab[i] = "H [cm]"
    }
    if( vars[i]  == "WC" ){
      var_lab[i] = "WC [cm3/cm3]"
    }
    if( vars[i]  == "DRAINAGE" ){
      var_lab[i] = "DRAINAGE [cm]"
    }
  }

  # define the axes
  y2 <- list(
    overlaying = "y",
    side = "right",
    anchor = "free",
    position = 0.87,
    title = var_lab[1]
  )

  y3 <- list(
    overlaying = "y",
    side = "right",
    title = var_lab[2],
    anchor = "free",
    position = 0.95
  )

  y4 <- list(
    overlaying = "y",
    side = "left",
    anchor = "free",
    position = 0,
    title = var_lab[3]
  )

  observed_data <- load_swap_observed(project_path, verbose = verbose)
  obs_cols_names <- observed_data[-1] %>% colnames()
  obs_cols <- paste0("obs",obs_cols_names)
  colnames(observed_data)[2:length(colnames(observed_data))] <- obs_cols

  modelled_data <- load_swap_output(project_path)
  modelled_data <- modelled_data$custom_depth

  results = left_join(modelled_data, observed_data, by = "DATE")
  colnames_result <- colnames(results)
  # grabs the depths at which we have observed data for
  depths = get_swap_depths(data = results)

  # grab run name
  run_name <- project_path %>% str_split("./") %>% unlist() %>% tail(1)

  # base plot using SWAP model results
  fig <- plot_ly(data = results)

  # width of the graph
  graph_domain = c(0.079, 0.87)

  # Rain ----
  # rain gets added to the base plot first.
  # this is because ONE variable must always be constant, and since
  # some case studies might use WC, some might use H, some might use TEMP, or
  # not, the only constant I can be sure of is RAIN.
  #     -- on second thought, this might not be necessary..

  # find the RAIN column index
  obs_index <- grepl(x = colnames_result, paste0("\\bRAIN\\b")) %>% which()

  # add the first trace to the graph
  fig <-
    fig %>% add_trace(
      x = ~ DATE,
      y = results[[obs_index]],
      name = "Rain (obs)",
      type = "bar",
      visible = if("RAIN" %in% show){""}else{"legendonly"},
      marker = list(
        color = "steelblue4",
        opacity = 0.6,
        line = list(color = 'black',
                    width = 0.5)
      )
    )

  # the depth dependent variables are added in traces per depth
  for (depth in depths) {

  # if WC is among the desire variables, it shall be added
  if("WC" %in% vars){
    # color palettes for the depth dependent variables
    wc_palette <- colorRampPalette(c("dodgerblue4","dodgerblue2","deepskyblue" ))

    # the color of the trace is determined by which depth we are at.
    # the reason we cant just use ~depth is because we are going column-wise
    # instead of melted.

    # the color range is dynamically extended depending on how many depths there
    # are.
    wc_color = wc_palette(length(depths))[which(depth == depths)]

    # Water content -----

    # finds the column index of the modeled and observed WC. \\b is a flag for
    # exact match

    mod_index =  grepl(x = colnames_result, paste0("\\bWC_",depth,"\\b")) %>% which()
    obs_index =  grepl(x = colnames_result, paste0("\\bobsWC_",depth,"\\b")) %>% which()

    # If a column has been found for the modeled WC, then add the trace.
    # line = list() is required because we need use custom colors
    if(length(mod_index)>0){
      fig <-
        fig %>% add_trace(
          x = ~ DATE,
          y = results[[mod_index]], # the selected column
          yaxis = paste0("y",which("WC"==vars)+1), # adds this trace to the correct y axis
          name = paste("WC", depth, "[mod]"),
          mode = "lines",
          type = "scatter",
          visible = if("WC" %in% show){""}else{"legendonly"},
          line = list(color = wc_color,colors = wc_palette(length(depths)))

        )
    }

    # if observed data has been found, then a trace will be added as well.
    if(length(obs_index)>0){
      fig <-
        fig %>% add_trace(
          x = ~ DATE,
          y = results[[obs_index]],
          name = paste("WC", depth,"[obs]"),
          mode = "lines+markers",
          yaxis = paste0("y",which("WC"==vars)+1), # adds this trace to the correct y axis
          type = "scatter",
          visible = if("WC" %in% show){""}else{"legendonly"},
          line = list(color = "black"),
          marker = list(color = wc_color, line = list(
            color = 'black',
            width = 1))
        )
    }

  }

  # temp -----

    if ("TEMP" %in% vars) {
      temp_palette <- colorRampPalette(c("darkolivegreen", "forestgreen", "green1"))
      temp_color <- temp_palette(length(depths))[which(depth == depths)]
      mod_index <- grepl(x = colnames_result, paste0("\\bTEMP_", depth, "\\b")) %>% which()
      obs_index <- grepl(x = colnames_result, paste0("\\bobsTEMP_", depth, "\\b")) %>% which()

  if(length(mod_index)>0){
    fig <-
      fig %>% add_trace(
        x = ~ DATE,
        y = results[[mod_index]],
        name = paste("TEMP", depth, "[mod]"),
        yaxis = paste0("y",which("TEMP"==vars)+1), # adds this trace to the correct y axis
        mode = "lines",
        type = "scatter",
        visible = if("TEMP" %in% show){""}else{"legendonly"},
        line = list(color = temp_color, colors = temp_palette(length(depths)))
      )
  }

  if(length(obs_index)>0){
    fig <-
      fig %>% add_trace(
        x = ~ DATE,
        y = results[[obs_index]],
        name = paste("TEMP", depth,"[obs]"),
        yaxis = paste0("y",which("TEMP"==vars)+1), # adds this trace to the correct y axis
        mode = "lines+markers",
        type = "scatter",
        visible =if("TEMP" %in% show){""}else{"legendonly"},
        line = list(color = "black"),
        marker = list(color = temp_color, line = list(
          color = 'black',
          width = 1))
      )
  }
    }
  # pressure head -----
  if("H" %in% vars){
    h_palette <- colorRampPalette(c("deeppink4","deeppink2","magenta" ))
    h_color = h_palette(length(depths))[which(depth == depths)]
    mod_index =  grepl(x = colnames_result, paste0("\\bH_",depth,"\\b")) %>% which()
    obs_index =  grepl(x = colnames_result, paste0("\\bobsH_",depth,"\\b")) %>% which()

    if(length(mod_index)>0){
      fig <-
        fig %>% add_trace(
          x = ~ DATE,
          y = results[[mod_index]],
          name = paste("H", depth, "[mod]"),
          yaxis = paste0("y",which("H"==vars)+1), # adds this trace to the correct y axis
          mode = "lines",
          type = "scatter",
          visible = if("H" %in% show){""}else{"legendonly"},
          line = list(color = h_color, colors = h_palette(length(depths)))
        )
    }

    if(length(obs_index)>0){
      fig <-
        fig %>% add_trace(
          x = ~ DATE,
          y = results[[obs_index]],
          name = paste("H", depth,"[obs]"),
          yaxis = paste0("y",which("H"==vars)+1), # adds this trace to the correct y axis
          mode = "lines+markers",
          type = "scatter",
          visible = if("H" %in% show){""}else{"legendonly"},
          line = list(color = "black"),
          marker = list(color = h_color, line = list(
            color = 'black',
            width = 1))
        )
    }
  }
    }

 # the non-depth dependent variables get added after the loop.
  # Drainage ----
  if("DRAINAGE" %in% vars){
  mod_index =  grepl(x = colnames_result, paste0("\\bDRAINAGE\\b")) %>% which()
  obs_index =  grepl(x = colnames_result, paste0("\\bobsDRAINAGE\\b")) %>% which()

  if(length(mod_index)>0){
    fig <-
      fig %>% add_trace(
        x = ~ DATE,
        y = results[[mod_index]],
        name = "Drainage (mod)",
        yaxis = paste0("y",which("DRAINAGE"==vars)+1), # adds this trace to the correct y axis
        mode = "lines",
        type = "scatter",
        visible = if("DRAINAGE" %in% show){""}else{"legendonly"},
        line = list(color = "black")
      )
  }

  if(length(obs_index)>0){
    fig <-
      fig %>% add_trace(
        x = ~ DATE,
        y = results[[obs_index]],
        name = "Drainage (obs)",
        yaxis = paste0("y",which("DRAINAGE"==vars)+1), # adds this trace to the correct y axis
        mode = "lines+markers",
        type = "scatter",
        visible = if("DRAINAGE" %in% show){""}else{"legendonly"}
      )
    }
  }

  # layout settings for the multi-axis setup
  if(length(vars)==3){
  fig <- fig %>% layout(
    title = run_name, yaxis2 = y2, yaxis3 = y3, yaxis4 = y4, # the max, 4 axis
    xaxis = list(title = '', domain = graph_domain), # graph margins
    yaxis = list(title="RAIN" # name for the MAIN y axis
                 )
    )
  }

  if(length(vars)==2){
    fig <- fig %>% layout(
      title = run_name, yaxis2 = y2, yaxis3 = y3, # 3 axis
      xaxis = list(title = '', domain = graph_domain), # graph margins
      yaxis = list(title="RAIN" # name for the MAIN y axis
      )
    )
  }

  if(length(vars)==1){
    fig <- fig %>% layout(
      title = run_name, yaxis2 = y2, # dual axis
      xaxis = list(title = '', domain = graph_domain), # graph margins
      yaxis = list(title="RAIN" # name for the MAIN y axis
      )
    )
  }

  if(length(vars)==0){
    fig <- fig %>% layout(
      title = run_name, # no additional axis
      xaxis = list(title = '', domain = graph_domain), # graph margins
      yaxis = list(title="RAIN (cm)" # name for the MAIN y axis
      )
    )
  }

  #further layout settings
  fig <- fig %>% #
    layout(plot_bgcolor='white', # background color
           legend = list(orientation = 'h'), # horizontal legend
           hovermode = "x unified"
    )
  # print out the plot.
  fig %>% print()
}
