# Reading and writing parameters

#' Clean swap file
#'
#' Returns a cleaned up SWAP file fit for manipulation
#' Make sure each table in the swap main file ends with a '* End of table' marker!!!
#'
#' Not necessarily meant for manual use.
#'
#' @param project_path (REQ) (string) path where the swap file is located
#' @param swap_file (OPT) (string) name of the swap file (will default to "swap.swp" if left blank)
#'
#' @importFrom dplyr %>%
#' @importFrom stringr str_trim
#'
#' @returns cleaned up SWAP file in list form.
#' @export
clean_swp_file <- function(project_path, swap_file = "swap.swp") {
  path = paste0(project_path, "/", swap_file)
  swp <- readLines(path)

  # checking to see if all "end of tables" are case sensitively correct
  # this was not always the case
  eot <- (swp %>% grepl(x = ., "* End of table")) %>% which()
  eot_exact <-
    (swp %>% grepl(x = ., "\\bEnd of table\\b")) %>% which()
  if ((eot == eot_exact) %>% all() == FALSE) {
    stop("please make sure that all tables end with the line '* End of table'")
  }

  # remove all the comment lines starting with *, except for the * End of lines
  comment_lines = (swp %>% substr(x = ., 1, 1) == "*") %>% which()
  if(length(eot)>1){
    comment_lines <- comment_lines[-which(comment_lines %in% eot)]

  }
  if (comment_lines %>% length() > 0) {
    swp <- swp[-comment_lines]
  }

  # remove all the comment lines which start with "!"
  comment_lines2 = (swp %>% str_trim() %>% substr(x = ., 1, 1) == "!") %>% which()
  if (comment_lines2 %>% length() > 0) {
    swp <- swp[-comment_lines2]
  }

  # remove any empty lines
  empty_lines = (swp %>% str_trim() %>% substr(x = ., 1, 1) == "") %>% which()
  if (empty_lines %>% length() > 0) {
    swp <- swp[-empty_lines]
  }

  return(swp)
}


# todo: test if this works with ANY swap parameter file (drain, crop)
# todo, figure out how to return a list of dataframes by appending.


#' Parse swap file
#'
#' Reads the passed swap file and parses it. returns parameters and tables in a
#' convienient format. can also write the file directly into the rswap directory
#'
#' Not necessarily meant for manual use.
#'
#' Make sure each table in the swap main file ends with a '* End of table' marker!!!
#'
#' Also this is designed to work with the same format as the example files. if
#' you do something different with yours,  you might run into issues
#'
#' @param project_path (REQ) (string), path to project directory
#' @param swap_file (OPT) (string), name of swap file to be used. defaults to
#' swap.swp if left blank
#' @param swap_file (OPT) (string) name of the swap file to be used. defaults to swap.swp
#' @param verbose (OPT) (boolean) print status?
#' @param quiet prints important warning by default, use quiet=TRUE to silence.
#'
#' @importFrom dplyr %>% nth
#' @importFrom stringr str_trim str_split
#' @importFrom purrr map
#' @importFrom tibble tibble
#'
#' @returns SWAP parameters in a dataframe format and path to where the tables
#' were saved in a csv format (until i figure out how to return an array of
#' dataframes)
#'
#' @export
parse_swp_file <-
  function(project_path,
           swap_file = "swap.swp",
           verbose = F,
           quiet = F) {

    if(quiet == FALSE){
      cat("Warning:\nall TABLES in the swap file must end with exactly this line:\n\n    -->   '* End of table'   <--\n\notherwise this function will not work!\n(turn this warning off with quiet = T)\n ..I am working on improving this\n")
    }
    path = paste0(project_path, "/", swap_file)

    swp <- clean_swp_file(project_path, swap_file = swap_file)

    # predefine for-loop vars
    par_df <- data.frame()
    tab_df <- list()
    new_i = 0
    special_cases<-c("OUTDATINT", "OUTDAT")


    table_path = paste0(project_path, "/rswap/tables")
    dir.create(table_path, showWarnings = F)

    # loop through the main swap file line by line
    # why for loop and not vectorized?
    # because required behavior depends on what lines come before and after!
    for (i in c(1:length(swp))) {
      # the line to be handled
      line = swp[i]

      # this is important for not double-reading tables later on
      if (i < new_i) {
        next()
      }

      # special case for outdatint =
      # why special case? OUTDATINT is the only Table that has an equal sign,
      # so by my parsing rules, it is both a parameter and a table (annoying!)
      # with this special case it is parsed as a table, but saved as a parameter


      # this line we dont want to deal with if it ever should come up
      # it shoudnt... but template swap file is not very consistent
      if (line == "* End of table") {
        next()
      }

      # parameters  always have an equal sign in their line, so we can use this
      # to identify them
      is_param <- line %>% grepl(x = ., "=")

      # the routine for if its a parameter:
      if (is_param == TRUE) {
        # extract the comment denoted by a !
        comment = line %>% str_split("!") %>% unlist() %>% nth(2)
        # split the data from the comment using "!" as an indicator
        line_split = line %>% str_split("!") %>% unlist() %>% nth(1)
        # split that line again, based off the equals sign. left of the
        # equals sign is the parameter name, to the right is the value
        param = line_split %>% str_split("=") %>% unlist() %>% str_trim() %>% nth(1)

        if(param %in% special_cases){
          swap_table_end = swp[i:length(swp)] %>% grepl(x = ., " End of table") %>% which() %>% min()
          value <- swp[(i+1):(i+swap_table_end-2)] %>% paste(collapse = " ") %>% str_trim()
          new_i <- i + swap_table_end
        }else{
          value = line_split %>% str_split("=") %>% unlist() %>% str_trim() %>% nth(2)

        }
        # create a row in the final dataframe containing these 3 things,
        # seperately
        add_row = data.frame(param = param,
                             value = value,
                             comment = comment)

        par_df <- rbind(par_df, add_row)
        # routine for if its a table
      } else{
        header = line  %>%  str_split("!") %>% map(1)  %>% str_trim() %>% str_split("\\s+") %>%  unlist()
        width = header %>% length()

        # finds the next NON NUMERIC VALUE
        swap_table_end = swp[i:length(swp)] %>% grepl(x = ., " End of table") %>% which() %>% min()

        # super messy!
        swap_table <- swp[i:(i + swap_table_end - 2)]
        swap_table[1] <-
          header %>% paste(collapse = " ") %>% str_trim()
        swap_table2 <-
          swap_table %>% str_trim() %>% str_split("\\s+")
        base <- swap_table2 %>% map(1) %>% unlist()
        for (j in c(2:width)) {
          col <-  swap_table2 %>% map(j) %>% unlist()
          base <- cbind(base, col)
        }
        swap_table3 <- base %>% as.data.frame()

        colnames(swap_table3) <- header
        swap_table3 <- swap_table3[-1, ]
        swap_table3 <- swap_table3 %>% tibble()

        table_name <- header %>% paste(collapse = "_")
        write.table(
          x = swap_table3,
          file = paste0(table_path, "/", table_name, ".csv"),
          sep = ",",
          row.names = F,
          col.names = T,
          quote = F
        )

        # skip the next rows because you already read them in
        new_i = i + swap_table_end
      }
    }

    if (verbose) {
      cat("...tables have been saved in .csv format here:\n",
          table_path,
          "\n")
    }

    return(list(
      parameters = (par_df %>% tibble()),
      table_path = table_path
    ))
  }

# todo, should be internal

#' write swap file
#'
#' writes the swap file from the passed parameter dataframe, and the passed path
#' to the tables stored as csv from the parse_swap_file() function
#'
#' @param parameters (REQ) (dataframe) dataframe of project parameters as created
#' by the parse_swap_file() function
#' @param table_path (REQ) (string) path to swap tables stored as csv as written
#' by the parse_swap_fil function
#' @param outpath (REQ) (string) where to write the swap file (and what to call it)
#' @param verbose (OPT) (boolean) print status?
#'
#' @importFrom dplyr %>%
#' @export
#'
write_swap_file <-
  function(parameters,
           table_path,
           outpath,
           verbose = F) {
    version = "v1.0" # TODO version should be some kind of global variable)

    removed <- file.remove(outpath) %>% suppressWarnings()

    if(removed & verbose){
      cat("...overwriting file:\n", outpath, "\n")
    }

    write.table(
      x = paste("* SWAP main file created by rswap", version, "at", Sys.time()),
      file = outpath,
      quote = F,
      col.names = F,
      row.names = F,
      append = F
    )

    # only do this if they exist...
    if( parameters$value[which(parameters$param=="OUTDATINT")] %>% length() > 0){
    # need to remove outdat and outdatint, because for parsings sake they need
    # to be written on a new line
    outdatint <- parameters$value[which(parameters$param=="OUTDATINT")]
    outdatint <- outdatint %>% str_split(" ") %>% unlist()
    outdat <- parameters$value[which(parameters$param=="OUTDAT")]
    outdat <- outdat %>% str_split(" ") %>% unlist()
    odlist = c("OUTDATINT =")
    for (variable in outdatint) {
      odlist <- append(odlist, variable)
    }
    odlist <- append(odlist, "* End of table")
    odlist <- append(odlist, "OUTDAT = ")
    for (variable in outdat) {
      odlist <- append(odlist, variable)
    }
    odlist <- append(odlist, "* End of table")
    special_case <- odlist %>% as.data.frame()
    # remove outdat and outdat int from dataframe
    param_clean = parameters[which(parameters$param %in% c("OUTDATINT", "OUTDAT") == FALSE),]
    # write the special cases:
    write.table(
      special_case,
      file = outpath,
      quote = F,
      row.names = F,
      col.names = F,
      append = T
    )
    }else{
      param_clean <- parameters
    }

    # now back to normal buisness
    par_write = paste(param_clean$param, "=", param_clean$value)
    write.table(
      par_write,
      file = outpath,
      quote = F,
      row.names = F,
      col.names = F,
      append = T
    )

    tables <- list.files(table_path, full.names = T)
    for (table in tables) {
      read <- read.csv(table, colClasses = "character") %>% tibble()

      read %>% tibble()
      write.table(
        read,
        file = outpath,
        quote = F,
        row.names = F,
        col.names = T,
        append = T,
        sep = " "
      ) %>% suppressWarnings()

      eol_table <- data.frame("* End of table")

      write.table(
        eol_table,
        file = outpath,
        quote = F,
        row.names = F,
        col.names = F,
        append = T,
        sep = " "
      ) %>% suppressWarnings()

    }

    if (verbose) {
      cat("...swap file written to:\n", outpath, "\n")
    }
    return(outpath)
  }


#' Set SWAP output
#'
#' by changing the inlist_csv function. if you depths for variables differ,
#' you need to change the "INLIST_CSV" parameter manually with change_swap_par()
#'
#' @param parameters parsed parameter DF
#' @param variables  variables desired for output
#' @param depths depths depth at which to output them
#' @param verbose print status?
#'
#' @importFrom glue glue
#' @importFrom dplyr %>% last
#'
#' @returns parameter dataframe with modified INLIST_CSV parameter.
#'
#' @export
#'
set_swap_output <- function(parameters, variables, depths, verbose = F){

  # template:
 # INLIST_CSV = 'rain,snow,drainage,DSTOR,TEMP[-15,-40,-70],WC[-15,-40,-70],H[-10,-20,-30]'

  variables <- variables %>% toupper()
  # todo need to expand these... or something
  depthwise <- c("TEMP", "WC", "H")
  nodepth <- c("RAIN", "SNOW", "DRAINAGE", "DSTOR")

  string <- "["
  for (d in depths) {
    if(d == last(depths)){
      string <- glue("{string}-{d}]")

    }else{
      string <- glue("{string}-{d},")
    }
  }

  # rain must always be present! (for the soft calibration plot)
  outstring <- "'RAIN,"
  for (var in variables) {
    if (var %in% depthwise) {
      if (var == last(variables)) {
        add_var <- glue("{var}{string}'")

      } else{
        add_var <- glue("{var}{string},")
      }
    }else if(var %in% nodepth){
      if(var == last(variables)){
        add_var <- glue("{var}'")
      }else{
        add_var <- glue("{var},")
      }
    }else{
      stop(glue("variable {var} not supported yet"))
    }

    outstring <- glue("{outstring}{add_var}")
  }

  parameters <- change_swap_par(parameters, "INLIST_CSV", outstring)

  if(verbose){
    cat(glue("...updating parameter:\n INLIST_CSV = {outstring} \n"))
  }
  return(parameters)

}
