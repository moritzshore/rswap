# auto cal functions

# gets the statistics of the last run (WC, NSE for now)
autocal_get_stat <- function(){
  full_df <- left_join(read_daily_output(field), read_observed(field), by = "DATE")
  data = list(read_daily_output(field), full_df, read_observed(field))
  NSE = get_statistics(field, data, user.var = "WC") %>% select(NSE) %>% pull() %>% mean() %>% return()
}

# sets up the initial state
autocal_initial_spt <- function(field, spt_initial, timeout){
  
  # variables of the soil property table
  vars = colnames(spt_initial)
  
  # layers of the spt
  layers = spt_initial$ISOILLAY1
  
  # read SWAP file template
  swap_file  <- readLines(paste0(field,"/swap_autocal.swp"))
  
  # this nested loop replaces every parameter pattern  in the template
  # with the desired parameters from the initial_spt data frame 
  for (variable in vars) {
    for (layer in layers) {

      # the grab pattern
      auto_pattern = paste0("auto_",layer,"_", variable)
      
      # the replacement value from the initial-spt
      column = spt_initial %>% select(all_of(variable))
      replacement = column[layer,] %>% pull()

      # replacing the pattern with the placement
      swap_file  <- gsub(pattern = auto_pattern, replace = replacement, x = swap_file)
    }
  }
  
  # generate the file
  writeLines(swap_file, con=paste0(field,"/initial-cal.swp"))
  
  # run swap using the initial file
  code = run_swap_autocal(field = field, swapfile = "initial-cal.swp", timeout = timeout)
  
  # check to see if the model run suceeded
  if(code != 100){
    print(paste("error code", code))
    print("initial set-up failed. make sure SWAP project can run before using this function (or maybe make the timeout longer (code 124))")
    return(NA)
  }
  
  # then delete the initial file since it is no longer needed
  #unlink(paste0(field,"/initial-cal.swp"))
  
  # return the NSE of the initial run
  return(autocal_get_stat())
}

# adjusts a value of the SPT and returns the new table. 
adjust_spt <- function(spt_initial, variable, layer, value){
  
  # pull out the vector of layer-wise parameters for the current variable
  parameter_vector = spt_initial %>% select(all_of(variable)) %>% pull()
  
  # extract the current parameter for the current layer
  parameter = parameter_vector[layer]
  
  # modify the parameter (first converting back to numeric)
  # and then save it as a character
  swap_value = value %>% as.character()
  
  # if the value has no decimal place, add one back on
  if (swap_value %>% grepl(x = ., "\\.") == FALSE) {
    swap_value = paste0(swap_value, ".0")
  }
  
  modified_parameter = swap_value
  
  # if the modified parameter now has less digits than before, 
  # restore the 0 padding back to the original. (this should work)
  if(modified_parameter %>% nchar() < parameter %>% nchar()){
    modified_parameter = str_pad(
      string = modified_parameter,
      width = parameter %>% nchar(),
      side = "right",
      pad = "0"
    )
  }
  
  # predefine the adjusted parameter vector
  adjusted_parameter_vector <- parameter_vector
  
  # and add in the modified value
  adjusted_parameter_vector[layer] = modified_parameter
  
  # now take the initial SPT and replace the value of the
  # current variable and replace it with the new adjusted vector.
  spt_adjusted = spt_initial %>%
    mutate({{variable}}:= adjusted_parameter_vector) %>%
    tibble()
  
  # return the now modified SPT
  return(spt_adjusted)
}

# generates a swap file with an adjusted SPT
swap_autocal_iteration_file_gen <- function(field, spt_adjusted){
  
  # variables of the soil property table
  vars = colnames(spt_adjusted)
  
  # layers of the spt
  layers = spt_adjusted$ISOILLAY1
  
  # generate SWAP file given the SPT:
  
  # read in template
  swap_file  <- readLines(paste0(field,"/swap_autocal.swp"))
  
  # replace template values with desired values from spt_adjusted
  for (variable in vars) {
    for (layer in layers) {
      
      # the grab pattern
      auto_pattern = paste0("auto_",layer,"_", variable)
      
      # the replacement value from the initial spt
      column = spt_adjusted %>% select(all_of(variable))
      replacement = column[layer,] %>% pull()
      # replacing the pattern with the placement
      swap_file  <- gsub(pattern = auto_pattern, replace = replacement, x = swap_file)
    }
  }
  
  # then generate the file
  writeLines(swap_file, con=paste0(field,"/auto_cal_adjusted.swp"))
  
  # return path of created file
  return(paste0(field,"/auto_cal_adjusted.swp"))
}

# Adjusted the SPT, creates the SWAP file, runs swap, and returns NSE with info as tibble row
swap_autocal_run_iteration <- function(spt_initial, variable, layer, initial, value, timeout){
  
  # created the adjusted SPT
  spt_adjusted = adjust_spt(spt_initial, variable = variable, layer = layer, value = value)
  
  # Generate the SWP main file using the adjusted SPT
  swap_adj_path = swap_autocal_iteration_file_gen(field, spt_adjusted)
  
  # run this new SWAP file and store the NSE (and limit to given timer)
  code = run_swap_autocal(field, swapfile = "auto_cal_adjusted.swp", timeout = timeout)
  
  # delete the modified swap file after it has been used. 
  unlink(swap_adj_path)
  
# behavior if model run fails
  if(code!=100){
    iteration_info = tibble(layer = layer, variable = variable, value = value, NSE = NA)
    return(iteration_info)
  }

  # Behavior if model run succeeds
  if(code==100){
  NSE =  autocal_get_stat() %>% round(x=.,digits = 4)
  iteration_info = tibble(layer = layer, variable = variable, value = value, NSE = NSE)
  }
  
  return(iteration_info)
}

# runs swap using the given swapfile
run_swap_autocal <- function(field, swapfile, timeout){
  
  # refresh the work directory
  unlink(paste0(field, "/work"), recursive = T)
  dir.create(paste0(field, "/work"), showWarnings = F)
  
  # create the command to run the model with  swapfile
  command = paste0('dependent/swap.exe ', field, '/', swapfile)
  
  # execute command with timeout. hide output
  code = system(command, timeout = timeout, show.output.on.console = F)
  
  # delete the extra files
  unlink("swap.ok")
  unlink("reruns.log")
  swap_log_file_path = swapfile %>% str_replace(pattern = ".swp", "_swap.log")
  unlink(paste0(field, '/', swap_log_file_path))
  
  return(code)
}

spt_sensitivity_check <-
  function(field,
           SPT,
           variable,
           layer,
           timeout,
           values,
           initial_nse,
           run_initial = FALSE,
           VERBOSE = FALSE,
           runtime = NA) {
    
    return_df = tibble()

    if (run_initial) {
      print("running initial model setup...")
      # loading screen
      
      plot(
        x = values,
        y = rep(0, length(values)),
        main = "waiting for a model completion..",
        ylab = "NSE",
        xlab = variable,
        type = "b"
      )
      
      # run the model with the initial values.
      # this is to estimate how long a model run is
      # and to determine the initial NSE.
      return_df = auto_cal_initial_run(field = field, spt_initial = SPT, timeout)
      
      initial_nse = return_df$initial_nse
      runtime = return_df$runtime
    }
    
    # pre-definition of data lists needed in the loop
    auto_cal_info = tibble()
    icon_list = c()
    color_list = c()
    counter = 0
    
  # going through each value passed
  for (value in values) {
    counter = counter+1
    
    # run the SWAP iteration for the current value.
    iteration = swap_autocal_run_iteration(
      spt_initial = SPT,
      variable = variable,
      layer = layer,
      value = value,
      timeout = timeout
    )
    # append the info from the model run to the data frame
    auto_cal_info <- rbind(auto_cal_info, iteration)
    
    # save the current state of the data frame to the global environment.
    # this is so that if the function is aborted, the progress up until then 
    # is retained and saved. 
    assign("auto_cal_info", auto_cal_info, envir = .GlobalEnv)
    
    color_and_icon <- determine_NSE_class(color_list, icon_list, auto_cal_info, iteration, initial_nse)
    
    # extract the data from the package
    color_list = color_and_icon[[2]]
    icon_list = color_and_icon[[1]]

    if (iteration$NSE %>% is.na() == FALSE) {
      auto_cal_update_plot(auto_cal_info, iteration, initial_nse, color_list, icon_list)
    }
    # update run number
    # TODO: add est. time remaining
    
    eta = (length(values)-counter)*runtime
    
    # cat(/r) gets into problems when the width of the string changes. 
    # I havent figured out how to fix it yet though, so i need to pad the string 
    time_remaining = (eta/60) %>% round(x=.,digits = 2) %>%
      str_pad(string = ., width = 4, side = "right", pad = "0") %>%
      str_pad(string = ., width = 6, side = "left", pad = "0")
  
      if(VERBOSE){
    cat("\r", paste("run ", counter, "/", length(values), "[esc] to stop", "(Estimated time remaing: [", time_remaining, "mins])"))
    }
  }
  
   # to let the people know its done.
  if(VERBOSE){cat("\nfinished\n");beep()}
  #auto_cal_interactive_plot(field, auto_cal_info)
  
  # remove the files that are no longer needed.
  unlink("*.tmp")
  unlink("reruns.log")
  unlink(paste0(field,"/*.log"))
  unlink(paste0(field,"/initial-cal.swp"))
  unlink(paste0(field,"/auto_cal_adjusted.swp"))

    return(auto_cal_info)
}

# for determining color and icon of the data point
determine_NSE_class <- function(color_list, icon_list, auto_cal_info, iteration, initial_nse){
  
  # if at least one iteration has succeded, and the current iteration has succeed
  # then update the graph (otherwise errors come up with infinite ylims.
  # (this issue could be solved)
  if (length(auto_cal_info$NSE) > 0 &&
      iteration$NSE %>% is.na() == FALSE &&
      initial_nse %>% is.na() == FALSE) {
    
    # determining the quality class of the iteration
    # if this iteration is better than the initial state
    if(iteration$NSE > initial_nse){
      icon = "🤑"
      color = "purple"
        # if the iteration has equaled the NSE of the initial run
    }else if(iteration$NSE == initial_nse){
      icon = "🤠"
      color = "green"
        # if the  NSE is greater than .5 (acceptable state kinda)
    }else if(iteration$NSE >= .5){
      icon = "😏"
      color = "darkgreen"
        # if the NSE is better than zero
    }else if(iteration$NSE >= 0){
      icon = "😐"
      color = "orange"
        # otherwise the NSE is crap and you should cry
    }else{
      icon = "😭"
      color = "red"
    }
    
    # add the color and icon to the list
    icon_list = append(icon_list, icon)
    color_list = append(color_list, color)
    
    
  }else{ # if the model run failed, then add NA values to the icon and color
    icon = NA
    color = NA
    icon_list = append(icon_list, icon)
    color_list = append(color_list, color)
  }
  return(list(icon_list, color_list))
}

# tesating the speed and quality of the initial run
auto_cal_initial_run <- function(field, spt_initial, timeout) {
  # initial run of base.
  t1 = Sys.time()
  initial_nse = autocal_initial_spt(field, spt_initial, timeout = timeout) %>% round(x =., digits = 4)
  t2 = Sys.time()
  runtime = (t2 - t1) %>% round(x = ., digits = 2)
  return_df <- data.frame(initial_nse, runtime)
  return(return_df)
}

# updates the states plot
auto_cal_update_plot <- function(auto_cal_info, iteration, initial_nse, color_list, icon_list){

  auto_cal_info$value = auto_cal_info$value %>% as.numeric()
  # update the max values and NSE 
  minval <- min(auto_cal_info$value, na.rm = T)
  maxval <- max(auto_cal_info$value, na.rm = T)
  maxNSE <- max(auto_cal_info$NSE, na.rm = T)
  minNSE <- min(auto_cal_info$NSE, na.rm = T)
  
  # if there are more than 3 runs, shift them over to the left
  # and calculate the shift in y max. 
  if(auto_cal_info$value %>% length() > 3){
    y_range = abs(minNSE)-abs(maxNSE)
    custom_y_range <- c(minNSE, (maxNSE+abs(y_range*.15)))
    step_x <- auto_cal_info$value[2]-auto_cal_info$value[1]
    custom_x_range <- c(minval, (maxval+(step_x*2)))
  }else{
    custom_x_range <- c(minval, maxval)
    custom_y_range <- c(minNSE, maxNSE)
  }

  
    # Draw the plot
    plot(x = auto_cal_info$value, y = auto_cal_info$NSE, type = "c", lwd=2, lend = "round", ljoin = "round",
         ylab = "NSE", xlab =iteration$variable,
         main = paste(field, "layer",  iteration$layer, iteration$variable),
         xlim = custom_x_range,
         ylim = custom_y_range
    )
    
    # emojis
    points(
      x = auto_cal_info$value,
      y = auto_cal_info$NSE,
      col = color_list,
      pch = icon_list,
      cex = 2
    )
    
    # initial_nse horizontal line
    abline(h = initial_nse, col="red",lwd=2)
    text(min(auto_cal_info$value), initial_nse+.1, paste("initial NSE:", initial_nse))
    
    # annotations with status
    mtext(paste(iteration$variable, "w/", iteration$value, "=", iteration$NSE),
          side = 3, adj = 1)
    mtext(paste("Initial NSE  =", initial_nse),
          side = 3, adj = 0)
}

# plot the results of the autocal run in the interactive viewer
auto_cal_interactive_plot <- function(field, auto_cal_info) {
  plot_ly(auto_cal_info) %>%
    add_trace(
      x = ~ value,
      y =  ~ NSE,
      type = "bar",
      color = ~ NSE,
      name = "(value, NSE)"
    ) %>%
    layout(
      title = paste(
        field,
        "layer",
        auto_cal_info$layer[1],
        auto_cal_info$variable[1]
      ),
      yaxis = list(showgrid = FALSE),
      xaxis = list(showgrid = FALSE,
                   title = auto_cal_info$variable[1])
    ) %>% hide_colorbar() %>% print() %>% suppressWarnings()
}