# autocal_algorithm_functions


# This method keeps testing for values greater/less than the initial value. it stops
# when it detects that things are not improving anymore / getting worse. then
# it returns all the values it tested to the master function (nob_turn_optimize)
turn_nob <-
  function(field,
           variable,
           timeout,
           layer,
           start_value,
           direction,
           SPT,
           initial_nse,
           block_size,
           step,
           all_runs,
           middle_block_correlation,
           middle_block_max,
           middle_block_mean) {
    
    
    if (direction %in% c("left", "right") == FALSE) {
      stop("direction needs to be equal to either 'left' or 'right'")
    }
    
    # At the start of the method we want to assume we will keep going, are
    # however looking for reasons to stop. CONTINUE tracks this intent,
    # and as such we begin with it being TRUE. flatline_counter will come into play later on
    CONTINUE = TRUE
    flatline_counter = 0
    
    # While we want to continue, the contents of this loop need to be repeated.
    # once CONTINUE is no longer true, the method ends and returns its
    # values to the master method. 
    while (CONTINUE == TRUE) {
      
      # First we need to decide where to start. If this is the first iteration
      # of the while() loop, then this value will be the one passed to us from
      # the master function. after that, it will be updated by the while() loop
      # itself. 
      
      
      if (direction == "right") {
        # As we are going to the RIGHT in this method, we want our value range to
        # start from the "left". In this case then, the left starting point should
        # be the "start_value" plus one step, since we already did the start value
        # in the master method
        left =  start_value + step
        
        # We want to do exactly one block of iterations (or steps), both of which
        # were passed from the master function. The ending point (right terminus)
        # of the range of values we want to test is thus simply the start value
        # plus one block of steps, plus one step, since we already did the start
        # value in the master method
        right = start_value + (block_size * step) - step
      }
      
      if(direction == "left"){
        # As we are going to the LEFT in this method, we want our value range to
        # end from the "right". In this case then, the right starting point should
        # be the "start_value" minus one step, since we did that in the master
        # method.
        right =  start_value - step
        
        # We want to do exactly one block of iterations (or steps), both of which
        # were passed from the master function. The starting point (LEFT terminus)
        # of the range of values we want to test is thus simply the right terminus
        # (aka start value), minus one block of steps, minus one step
        left = start_value - (block_size * step) - step
      }
      
      
      
      
      # We need to make sure that the list of values we want to test are
      # actually valid. for this reason we pass our left and right bounds
      # through the the limits function. If the value exceeds the bound, then it
      # will be set equal to the bound. 
      
      # This needs to be checked for both limits on both sides, since it is
      # possible (esp. as coarse step levels) that both left and right are out
      # of bounds on the same side. In cases such as these, they will be both 
      # set to the bound and be equal. That is not a problem, since if the range
      # contains only one value, it will be skipped regardless and the algorithm
      # will know that we should no longer continue.
      if (left < get_parameter_limits(variable)[1]) {
        left = get_parameter_limits(variable)[1]
      }
      if (left > get_parameter_limits(variable)[2]) {
        left = get_parameter_limits(variable)[2]
      }
      if (right > get_parameter_limits(variable)[2]) {
        right = get_parameter_limits(variable)[2]
      }
      if (right < get_parameter_limits(variable)[1]) {
        right = get_parameter_limits(variable)[1]
      }
      
      # once we have the left and right bounds defined and checked by the limits
      # function, we can define the range that we will test in this iteration. 
      value_range = seq(from = left, to = right, by = step)
      
      # if the left and right bounds are equal, then we don't really have any
      # values left that we want to test. for this reason, the range is set to 
      # NA so that we know to stop turning the nob.
      if (left == right) {
        CONTINUE = FALSE
      }
      
      # now, if we would still like to continue, then we run the model 
      # for the created value range, at the correct field, variable, layer and 
      # SPT, all passed from the master method.
      # The results are saved in the auto_cal_info dataframe, which is later
      # used to determine if we should keep going to the right. 
      if(CONTINUE){
        auto_cal_info = spt_sensitivity_check(
          field = field,
          variable = variable,
          SPT = SPT,
          layer = layer,
          timeout = timeout,
          values = value_range,
          run_initial = F, 
          initial_nse = initial_nse
        )
      }
      
      # now before we add all those fresh new runs into the big dataframe, 
      # we want to check what our best attempt yet was. This is done
      # by finding the maximum NSE thus far, and will be used later on to
      # determine if this batch was any good, and if we should keep going. 
      best_nse = max(all_runs$NSE, na.rm = T)
      
      # With the best attempt saved, we are going to add this fresh batch of
      # runs to the big list, so that we can compare them later
      all_runs <- rbind(all_runs, auto_cal_info)
      
      # First we are going to check if more than 50% of the runs in that batch
      # failed. if that is the case, then this is probably not a direction we
      # want to keep going in, since its making the model so unstable.
      if (CONTINUE == TRUE &&
          auto_cal_info$NSE %>% is.na() %>% which() %>% length() > 0.5 *
          length((auto_cal_info$value))) {
        CONTINUE = FALSE
      }
      
      # If all things are still looking pretty good, then we are going to check
      # the mean, max, and correlation of this block, to decide if we want to keep going
      
      # since we are checking the correlation later, we need this to exist even
      # if it is undefined  
      block_correlation = NA 
      
      if(CONTINUE == TRUE){
        block_mean = auto_cal_info$NSE %>% mean(na.rm=T)
        block_max =  auto_cal_info$NSE[which(auto_cal_info$NSE == max(auto_cal_info$NSE, na.rm = T))] %>% min(na.rm=T)
        block_correlation = cor(auto_cal_info$value, auto_cal_info$NSE, use = "complete.obs") %>% suppressWarnings()
        
        # it is possible that the Means and Max will be either infinite or 
        # NotaNumber, in that case we do not want to keep going since it prob.
        # means things have gone desperately wrong, or there were no values to
        # test. (this block of code could probably be removed since we check
        # for model failure 50% at an earlier stage.)
        if (block_mean %>% is.infinite() ||
            block_mean %>% is.nan() ||
            block_max %>% is.infinite() ||
            block_max %>% is.nan()) {
          CONTINUE = FALSE
        }
      }
      
      # At this point, if we are still going it means that we have a valid
      # mean and max. that means we can test to see if they are better or worse
      # than the block we are coming from.
      if (CONTINUE == TRUE) {
        
        
        # first we check the mean of this block compared to the mean of the pre-
        # veious block. If this is the first run through of the while() loop
        # then the middle block is the block that was passed to us from the 
        # master method. Otherwise, it is the left block of the previous run-
        # through of the while() loop.
        
        # Here we are simply checking, if on average, the values of the new
        # block were better than the values of the previous block. We are doing
        # this because (especially at finer levels) SWAP returns erratic NSE
        # values that show little pattern. For this reason we cant just use
        # the trend line or the best value to decide if we want to keep going,
        # since we could have little trend, and no maximum values, but still
        # average improvement, giving hope to a better NSE in the next block
        
        # Conversely, it is possible that the values in the new batch were, on 
        # average, pretty bad, however one run was excellent, and better than
        # anything in the previous block. In that case we should keep going
        # since theres a chance that we'll get a stellar NSE again in the next
        # block.
        
        # If both the average and the max are worse than the previous block then
        # there really is no sense in still going, as it seeems to be getting 
        # worse. In that case, we set the continue flag to FALSE
        # (however we are still going to check the correlationw which might
        # override this choice!)
        if (block_mean >= middle_block_mean ||
            block_max >= middle_block_max) {
          CONTINUE = TRUE
        } else{
          CONTINUE = FALSE
        }
      }
      
      # Now we will check to see if the values correlate at all to better NSE:
      
      # We first want to make sure our values are valid or not. if they are not
      # valid for some reason, we set the correlation to 0
      
      # 1. Is the correlation vector simply NA? then set to 0
      if (block_correlation %>% is.na()) {
        block_correlation = 0
      }
      # 2. are there less than 2 values in the range? in that case no
      #    correlation should be made Set it to 0
      if (value_range %>% length() < 2) {
        block_correlation = 0
      }
      # 3. did more than 25% of the model runs result in an error? in that case
      #    we don't want to check the correlation of such a faulty batch. 
      if (value_range %>% is.na() %>% which() %>% length() > .25 * block_size) {
        block_correlation = 0
      }
      # 4. If the value range is less than half of the size of the original
      #    block, then we don't really want to trust the correlation all too
      #    much, and set it to 0
      if (value_range %>% length() < .5 * block_size) {
        block_correlation = 0
      }
      
      # The correlation check is placed after the other checks, since while
      # the mean/max might recommend a stop, correlation could recommend a go 
      # (or vice versa). Since we are trusting correlation over mean/max, it can
      # override the previous checks by being placed after.
      
      if (direction == "right") {
        # if the correlation of the block is very low in the negatives, it
        # indicates that the NSE is progressively getting worse. The current
        # threshold for it to stop is set at -0.80, but this can be altered
        # further towards -1 to be more strict, or closer to 0 to be more lenient
        # conversely, if the correlation approaches +1.00, that indicates that
        # the values are steadily getting better. This is a sign that we should
        # keep going.
        if (block_correlation < -.80) {
          CONTINUE = FALSE
        }
        if (block_correlation > .80) {
          CONTINUE = TRUE
        }
      }
      
      # this logic is reversed for the left direction.
      if (direction == "left"){
        if (block_correlation > .80) {
          CONTINUE = FALSE
        }
        if (block_correlation < -.80) {
          CONTINUE = TRUE
        }
      }
      
      # often at finer scales, the new values will not change the NSE
      # for a long long time, as the parameter is not sensitive to the current
      # level of detail. This can cause the model to slowly inch its way along
      # parameter range without making any progress. to stop this from happening
      # this counter checks to see if it has passed 3 blocks worth of values
      # all with identical NSE returns, if it has done so, the continuation will
      # stop. The thinking is, at 3 blocks, an improvement would have already
      # been detected in the previous stage. (only do this if the NSE is not NA)
      if(auto_cal_info$NSE %>% is.na() %>% any() == FALSE) {
        if ((auto_cal_info$NSE[1] == auto_cal_info$NSE) %>% all()) {
          flatline_counter = flatline_counter + 1
          if (flatline_counter > 3) {
            CONTINUE = FALSE
            flatline_counter = 0
          }
        } else{
          flatline_counter = 0
        }
      }
      
      # If, after all those checks, we still think we should keep going,
      # we need to update the middle block mean/max to be the current block,
      # and we need to change the start value of the next iteration to be the
      # end value of this iteration, which is the "right/left" bound depending
      # on the current direction
      if (CONTINUE == TRUE) {
        middle_block_mean = block_mean
        middle_block_max = block_max
        
        if (direction == "right") {
          start_value = right
          
        }
        if (direction == "left") {
          start_value = left
        }
      }
      
    } # end while loop
    
    # once the method decides it no longer want to turn the nob to the left, it
    # returns the all_runs dataframe which it was passed at the start, with
    # all its tested runs included, such that the master dataframe can pick the
    # best one (if there was any improvement.)
    return(all_runs)
  }

# This function is given a SWAP setup field, its current SPT, and a variable
# and layer to change a value in. It tests the waters around the initial
# variable and then starts to increase the parameter values. if the NSE gets 
# better (tested by means of correlation / mean / max) then it keeps going
# Once, for a multitude of possible reasons, it decides to stop going, it
# returns all the values it tested, and does the same thing while decreasing the
# value. once both those things are finished, it returns its attempt at
# improvment to the control sequence which decides it's fate.
nob_turn_optimize <-
  function(field,
           SPT,
           variable,
           layer,
           timeout,
           initial_nse,
           block_size,
           step) {
    
    # The starting value of the current variable is grabbed from the passed SPT
    # at the current layer.
    initial_value = SPT %>% select(all_of(variable)) %>% pull() %>% nth(layer) %>% as.numeric()
    
    # the all_runs tibble stores each and every run of the SWAP model, for
    # for tracking the best performance. 
    all_runs = tibble()
    
    # Step 1: We want to see what the NSE is like around the initial value.
    # To define this area we grab a range the size of the given blocksize around
    # the initial value. (+1)
    left = initial_value  -  (block_size  /  2  *  step)
    right = initial_value  +  (block_size  /  2  *  step)
    
    # We need to make sure that this range is within parameter limits, as often
    # when testing at coarse scales, you can quickly exceed parameter limits.
    # if the left or right bounds are outside of the given limits, then we set
    # them equal to that exceeded limit.
    if (left < get_parameter_limits(variable)[1]) {
      left = get_parameter_limits(variable)[1]
    }
    if (left > get_parameter_limits(variable)[2]) {
      left = get_parameter_limits(variable)[2]
    }
    if (right > get_parameter_limits(variable)[2]) {
      right = get_parameter_limits(variable)[2]
    }
    if (right < get_parameter_limits(variable)[1]) {
      right = get_parameter_limits(variable)[1]
    }
    
    # Now we have our proofed value range and can give it tom SWAP
    value_range = seq(from = left , to = right, by = step)
    
    # run the model for the mid block
    auto_cal_info = spt_sensitivity_check(
      field = field,
      SPT = SPT,
      variable = variable,
      layer = layer,
      timeout = timeout,
      values = value_range,
      run_initial = F,
      initial_nse = initial_nse
    )
    
    # These runs are added to the all_runs DF so that we can keep track of our
    # best ever attempts.
    all_runs <- rbind(all_runs, auto_cal_info)
    
    # As we are going to compare the performance of the blocks to the left and 
    # right of this one, we need to calculate some performance indexes for this
    # middle block. Currently this is mean, max, and correlation. Further
    # explanation about why these 3 metrics can be found within the sub-methods
    middle_block_mean = auto_cal_info$NSE %>% mean(na.rm = T)
    middle_block_max =  auto_cal_info$NSE[which(auto_cal_info$NSE == max(auto_cal_info$NSE, na.rm = T))] %>% min(na.rm =                                                                                                               T)
    middle_block_correlation = cor(auto_cal_info$value, auto_cal_info$NSE, use = "complete.obs") %>% suppressWarnings()

    # first we will start gradually increasing the parameter value. This method
    # will keep increasing until it decides the values are no longer getting any
    # better. At the point it will stop and return the all_runs dataframe with
    # any and all additional runs it may have performed.
    all_runs <- turn_nob(
      field,
      variable,
      timeout,
      layer,
      SPT = SPT,
      start_value = right,
      direction = "right",
      initial_nse,
      block_size,
      step,
      all_runs,
      middle_block_correlation,
      middle_block_max,
      middle_block_mean
    )
    
    # once the turn_nob_right method has decided to stop, and has returned its
    # entries to the "all_runs" DF, we start gradually decreasing the parameter
    # values, starting from the leftmost bound of the middle block. Once the
    # method decides it no longer wants to decrease the parameter value, it
    # stops and returns the all_runs data-frame with all its added entries.
    all_runs <- turn_nob(
      field,
      variable,
      timeout,
      layer,
      SPT = SPT,
      start_value = left,
      direction = "left",
      initial_nse,
      block_size,
      step,
      all_runs,
      middle_block_correlation,
      middle_block_max,
      middle_block_mean
    )
    
    # Having tested all the values we wanted to test, we can now see which one
    # had the best NSE and what parameter value caused that.
    best_nse = all_runs$NSE[which(all_runs$NSE == max(all_runs$NSE, na.rm = T)) %>% min(na.rm = T)]
    best_value = all_runs$value[which(all_runs$NSE == max(all_runs$NSE, na.rm = T)) %>% min(na.rm = T)]
    
    # then we return our best attempt, along with its NSE to the control
    # sequence, where it's fate will be decided.
    # along with the best attempt, the step, variable, layer, initial value +
    # NSE are passed, for diagnostics.
    return(
      data.frame(
        step = step,
        variable = variable,
        layer = layer,
        initial_value = initial_value,
        initial_nse = initial_nse,
        best_value = best_value,
        NSE = best_nse
      )
    )
  }

# this is the control function for the nob_turn_optimize method. This is the 
# function called by the user
autocalibrate_SPT_SWAP <- function(field, spt_initial, timeout = 60, steps, block_size, variables){
  
  initial_state = auto_cal_initial_run(field = field, spt_initial = spt_initial, timeout = timeout)
  initial_nse = initial_state$initial_nse
  
  if(initial_nse %>% is.na()){
    stop("Initial model set-up failed. Timeout potentially too short")
  }
  
  layers = spt_initial[1] %>% pull() %>% as.numeric()
  # initially, the SPT is set to what it was initially. it will be modified from
  # here on out
  SPT = spt_initial
  
  tracking_df <- tibble()
  new_NSE = 2 # initially perfect so that it runs once
  old_NSE = initial_nse
  while(new_NSE > old_NSE){
    initial_state_running = auto_cal_initial_run(field = field, spt_initial = SPT, timeout = timeout)
    old_NSE = initial_state_running$initial_nse
    # the actual complete algorithm
    # for each coarseness level
    for (step in steps) {
      # check each variable
      for (variable in variables) {
        # at every layer
        for (layer in layers) {
          
          paste("step:" ,step,"variable:",variable,"layer:", layer) %>% print()
          
          initial_state_running = auto_cal_initial_run(field = field, spt_initial = SPT, timeout = timeout)
          initial_nse = initial_state_running$initial_nse
          
          if(initial_nse %>% is.na()){
            print(initial_state_running)
            stop("Initial model set-up failed. Timeout potentially too short")
          }
          
          # twist the nob around a bit and find the best value
          results = nob_turn_optimize(
            field = field,
            SPT = SPT,
            variable = variable,
            layer = layer,
            timeout = timeout,
            initial_nse = initial_nse,
            block_size = block_size,
            step = step
          )
          
          print(results %>% tibble())
          
          tracking_df <- rbind(tracking_df, results)
          
          # if the best value is better than what we had before, then replace it.
          if(results$NSE > results$initial_nse){
            original_vector = SPT %>% select(all_of(variable)) %>% pull()
            
            # convert new value to string with dec.
            new_value = results$best_value %>% as.character()
            new_NSE = results$NSE
            # add the .0 in case it was removed
            if(new_value %>% grepl(x=.,"\\.") == FALSE){
              new_value  = paste0(new_value, ".0")
            }
            SPT[variable][layer,] = new_value
            
            # in- case of crash, load this
            saveRDS(object = SPT, file = paste0(field,"/SPT_backup.rds"))
          }else{new_NSE = results$initial_nse}
        }
      }
    }
    print("Calibration run complete, if the new NSE is better than the old one, we will continue")
    paste("oldNSE=",old_NSE)%>%print();paste("newNSE=",new_NSE)%>%print()
  }
  print("Autocalibration can improve the NSE no more, retuning modified SPT")
  tracking_df$ID = c(1:length(tracking_df$initial_value))
  ggplot(tracking_df)+geom_col(aes(x = ID, y = NSE, fill = step))
  return(SPT)
}

