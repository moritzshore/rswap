# Reading and writing parameters

#' Clean SWAP File
#'
#' Returns a cleaned up SWAP file fit for manipulation (removes all non-vital
#' information).
#'
#' This function is designed to be used internally, but may be useful to the end
#' users. This is currently only tested for the SWAP main file.
#'
#' @param project_path path to the project directory (string)
#' @param swap_file name of the swap file (string)
#'
#' @importFrom dplyr %>%
#' @importFrom stringr str_trim
#'
#' @returns Returns a cleaned up SWAP file in string vector form.
#'
#' @export
clean_swap_file <- function(project_path, swap_file = "swap.swp") {
  # TODO: find out for which SWAP input files this works, other than main
  # path and read
  path <- paste0(project_path, "/", swap_file)
  swp <- readLines(path)
  # remove all the comment lines starting with *,
  comment_lines = (substr(x = swp, 1, 1) == "*") %>% which()
  if (comment_lines %>% length() > 0) {
    swp <- swp[-comment_lines]
  }
  # remove all the comment lines which start with "!"
  c_lines <- swp %>% stringr::str_trim()
  comment_lines2 = (substr(x = c_lines, 1, 1) == "!") %>% which()
  if (comment_lines2 %>% length() > 0) {
    swp <- swp[-comment_lines2]
  }
  # remove any empty lines
  c_lines <- swp %>% stringr::str_trim()
  empty_lines = (substr(x = c_lines, 1, 1) == "") %>% which()
  if (empty_lines %>% length() > 0) {
    swp <- swp[-empty_lines]
  }
  return(swp)
}

#' Parse SWAP File
#'
#' This function reads in a SWAP main file writes the parameters, vectors, and
#' tables into csv format into the rswap directory. these can then be loaded
#' using `load_swap_parameters()`, `load_swap_tables()` and `load_swap_vectors()`
#'
#' Also, this parsing technique was designed to work with the SWAP example cases.
#' If your SWAP main file structure does something different, you might run into trouble.
#'
#' This function will be expanded to work with more SWAP input files like .dra
#'
#' @param project_path path to project directory (string)
#' @param swap_file optional swap main file name (string) defaults to "swap.swp"
#' @param verbose print status? (flag)
#'
#' @importFrom dplyr %>% nth
#' @importFrom stringr str_trim str_split
#' @importFrom purrr map
#' @importFrom tibble tibble
#' @importFrom crayon green underline blue
#'
#' @returns Returns paths to where the parameters, vectors, and tables are written
#'
#' @export
#'
parse_swap_file <- function(project_path, swap_file = "swap.swp", verbose = F) {

  # TODO rename to parse_swap_file()
  # finds the end of the table when passed a snipped of the swap file
  find_eot <- function(short_swp) {
    first_element <- short_swp %>%  stringr::str_trim() %>% stringr::str_split("\\s+") %>% purrr::map(1) %>% unlist()
    next_par <- grepl(x = short_swp, "=") %>% which() %>% min() %>% suppressWarnings()
    if(next_par %>% length() == 0){
      next_par = Inf
    }
    next_non_value_nor_date <- (first_element %>% as.numeric() %>% is.na() == TRUE &
                                  grepl(x=first_element, "-") == FALSE) %>% which() %>% min() %>% suppressWarnings()
    if(next_non_value_nor_date %>% length() == 0){
      next_non_value_nor_date = Inf
    }
    eot <- min(c(next_non_value_nor_date, next_par))
    # special case if at end of file:
    if(eot %>% is.infinite()){
      # return the index for the end of the file
      return(length(short_swp)+1)
    }
    return(eot)
  }

  rswap_dir <- build_rswap_directory(project_path)
  swp <- clean_swap_file(rswap_dir, swap_file = swap_file)

  swp_file <- paste0(rswap_dir, "/", swap_file)
  if(verbose){
    cat(blue("\u2139"),
        blue("Parsing swap file:"),"\n")
    cat(green((underline(glue("{swp_file}")))), "\n")
  }

  # predefine for-loop vars
  par_df <- data.frame()
  tab_df <- list()
  new_i = 0

  table_path = paste0(project_path, "/rswap/tables")
  vector_path =  paste0(project_path, "/rswap/vectors")
  parameter_path = paste0(project_path, "/rswap/parameters")

  dirs <- c(table_path, vector_path, parameter_path)

  unlink(dirs, recursive = T)
  dir_stat<-lapply(dirs, function(x) dir.create(x, showWarnings = F))

  # loop through the main swap file line by line
  # why for loop and not vectorized?
  # because required behavior depends on what lines come before and after!
  for (i in c(1:length(swp))) {
    # the line to be handled
    line = swp[i]
    # this is important for not double-reading tables later on, so we might
    # have to skip ahead sometimes
    if (i < new_i) {
      next()
    }

    # parameters  always have an equal sign in their line, so we can use this
    # to identify them
    is_param <- grepl(x = line, "=")
    is_table <- (grepl(x = line, "=") == FALSE)
    special_case <- FALSE

    # the routine for if its a parameter:
    if (is_param == TRUE) {

      # extract the comment denoted by a !
      comment = line %>% stringr::str_split("!") %>% unlist() %>% dplyr::nth(2)

      # split the data from the comment using "!" as an indicator
      line_split = line %>% stringr::str_split("!") %>% unlist() %>% dplyr::nth(1)

      # split that line again, based off the equals sign. left of the
      # equals sign is the parameter name, to the right is the value
      param = line_split %>% stringr::str_split("=") %>% unlist() %>% stringr::str_trim() %>% dplyr::nth(1)
      value = line_split %>% stringr::str_split("=") %>% unlist() %>% stringr::str_trim() %>% dplyr::nth(2)

      # special case is when the parameter has an equals sign, but the format
      # of how it is written in the SWAP file resembles a table.
      if (value == "") {
        is_table = TRUE
        special_case = TRUE
      } else{
        # create a row in the final dataframe containing these 3 things,
        # separately
        add_row = data.frame(param = param, value = value, comment = comment)
        par_df <- rbind(par_df, add_row)
      }
    }

    # routine for if its a table
    if (is_table) {
      # routine for if a special case was detected
      if (special_case) {

        header = line %>% stringr::str_trim()
        width = 1
        swap_table_end <- find_eot(short_swp = swp[(i + 1):length(swp)])
        swap_table <- swp[i:(i + swap_table_end-1)]

        table_name = header %>% stringr::str_remove("=") %>% stringr::str_trim()
        utils::write.table(
          x = swap_table,
          file = paste0(vector_path, "/", table_name, ".csv"),
          sep = ",",
          row.names = F,
          col.names = F,
          quote = T
        )

        # skip the next rows because you already read them in
        new_i = i + swap_table_end

      }else{
        # Routine for a Table, not special case
        header = line  %>%  stringr::str_split("!") %>% purrr::map(1)  %>% stringr::str_trim() %>% stringr::str_split("\\s+") %>%  unlist()
        width = header %>% length()

        swap_table_end <- find_eot(short_swp = swp[(i + 1):length(swp)])
        swap_table <- swp[i:(i + swap_table_end-1)]
        swap_table[1] <- header %>% paste(collapse = " ") %>% stringr::str_trim()
        swap_table2 <- swap_table %>% stringr::str_trim() %>% stringr::str_split("\\s+")

        base <- swap_table2 %>% purrr::map(1) %>% unlist()
        for (j in c(2:width)) {
          col <-  swap_table2 %>% purrr::map(j) %>% unlist()
          base <- cbind(base, col)
        }

        swap_table3 <- base %>% as.data.frame()
        colnames(swap_table3) <- header
        swap_table3 <- swap_table3[-1, ]
        swap_table3 <- swap_table3 %>% dplyr::tibble()
        table_name <- header %>% paste(collapse = "-")

        utils::write.table(
          x = swap_table3,
          file = paste0(table_path, "/", table_name, ".csv"),
          sep = ",",
          row.names = F,
          col.names = T,
          quote = T
        )
        # skip the next rows because you already read them in
        new_i = i + swap_table_end
      } # END of table not special
    } # END of table
  } # END of for loop

  # write parameters:
  write_swap_parameters(project_path, par_df, verbose)

  if(verbose){
    cat(blue("\U0001f4dd SWAP tables have been generated in .csv format here: \n"))
    cat(green(underline(vector_path)), "\n")
  }

  if(verbose){
    cat(blue("\U0001f4dd SWAP vectors have been generated in .csv format here: \n"))
    cat(green(underline(table_path)), "\n")
  }

  return(list(
    parameter_path = parameter_path,
    table_path = table_path,
    vector_path = vector_path
  ))
}

#' Write SWAP File
#'
#' Writes a SWAP main file from the parameters, vectors, and tables stored in
#' the rswap directory. **Before you use this function, you need to have parsed a SWAP file**
#'
#' This function currently is only intended for the SWAP main file, but will be
#' expanded to handle the other SWAP input files over time.
#'
#' @param project_path path to project directory
#' @param outfile name of the SWAP file to write. will be stored in project directory (string)
#' @param verbose print status? (flag)
#'
#'
#' @returns Returns path of written file
#'
#' @importFrom dplyr %>%
#' @importFrom crayon underline green
#' @importFrom readr read_csv cols
#'
#' @export
#'
write_swap_file <- function(project_path, outfile, verbose = F) {

  version <- utils::packageVersion("rswap") %>% as.character() %>% enc2utf8()

  if(file.exists(paste0(project_path,"/rswap/parameters/parameters.csv"))==FALSE){
    stop("[rswap] no swap file has been parsed by rswap yet. You must do so before writing a swap file!")
  }

  outpath <- paste0(project_path,"/",outfile)

  # Write header
  utils::write.table(
    x = paste("* SWAP main file created by rswap", version, "at", Sys.time()),
    file = outpath,
    quote = F,
    col.names = F,
    row.names = F,
    append = F
  )

  if (verbose) {
    cat("\U0001f4dd",
        bold(blue("created SWAP main file.")), "\n")
  }

  # Append parameters
  parameters = load_swap_parameters(project_path = project_path, verbose = verbose)
  par_write = paste(parameters$param, "=", parameters$value)

  utils::write.table(
    par_write,
    file = outpath,
    quote = F,
    row.names = F,
    col.names = F,
    append = T
  )

  if (verbose) {
    cat("\U0001f4dd",
        blue("SWAP parameters appended to main file."), "\n")
  }

  # Append tables
  tables<-load_swap_tables(project_path = project_path, verbose = verbose)
  for (table in tables) {
    utils::write.table(
      table,
      file = outpath,
      quote = F,
      row.names = F,
      col.names = T,
      append = T,
      sep = " "
    ) %>% suppressWarnings()

    # Theoretically don't need this, but its nice to have
    eol_table <- data.frame("* End of table")
    utils::write.table(
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
    cat("\U0001f4dd",
        blue("SWAP tables appended to main file."), "\n")
  }

  # Write vectors
  vectors <- load_swap_vectors(project_path = project_path, verbose = verbose)
  for (vector in vectors) {

    utils::write.table(
      vector,
      file = outpath,
      quote = F,
      row.names = F,
      col.names = T,
      append = T,
      sep = " "
    ) %>% suppressWarnings()

    # Theoretically don't need this, but its nice to have
    eol_table <- data.frame("* End of table")
    utils::write.table(
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
    cat("\U0001f4d",
        blue("SWAP vectors appended to main file."), "\n")
  }

  if (verbose) {
    cat(glue(blue("\u2705",
                  "SWAP main file written to: \n")))
    cat(green(underline(outpath)), "\n")
  }
  return(outpath)
}

#' Set SWAP output
#'
#' This function alters the output parameters of your SWAP input files.
#'
#' Currently it only modifies INLIST_CSV, but more functionality will be placed
#' here over time.
#'
#' @param project_path path to project directory
#' @param parameters parameter dataframe
#' @param autoset_output flag for matching output to observed file.
#' @param verbose print status? (flag)
#'
#' @importFrom glue glue
#' @importFrom dplyr %>% last
#' @importFrom crayon blue green underline bold
#'
#' @returns Returns parameter dataframe with modified INLIST_CSV parameter.
#'
#' @export
#'
set_swap_output <-
  function(project_path,
           parameters,
           autoset_output = F,
           verbose = F) {

    # change console output based on verbose flag
    if (verbose) {
      parameters <- change_swap_parameter(parameters, "SWSCRE", 2, verbose)
    } else{
      parameters <- change_swap_parameter(parameters, "SWSCRE", 0, verbose)

    }


    # add the critical output params if they are not present.
    if("INLIST_CSV" %in% parameters$param == FALSE){
      if(verbose){
        cat("\u2795",
            blue("adding", bold("INLIST_CSV = ''"), "to parameter list"))
      }
      add <- data.frame(param = "INLIST_CSV", value = "", comment = glue("added by rswap on {Sys.time()}"))
      parameters <- rbind(parameters, add)
    }

    # add the critical output params if they are not present.
    if("INLIST_CSV_TZ" %in% parameters$param == FALSE){
      if(verbose){
        cat("\u2795",
            blue("adding", bold("INLIST_CSV_TZ = ''"), "to parameter list"))
      }
      add <- data.frame(param = "INLIST_CSV_TZ ", value = "''", comment = glue("added by rswap on {Sys.time()}"))
      parameters <- rbind(parameters, add)
    }

    # SWCSV needs to be present in the SWAP main file, such that the needed
    # output can be printed. If this parameter already exists within the
    # parameter dataframe, we can simply adjust the value to 1. If it does
    # not exist yet, we need to add it, with the value of 1.
    if ("SWCSV" %in% parameters$param) {
      parameters = change_swap_parameter(parameters, "SWCSV", "1", verbose)
    } else{
      if(verbose){
        cat("\u2795",
            blue("adding", bold("SWCSV = 1"), "to parameter list"))
      }
      rbind(parameters,
            data.frame(
              param = "SWCSV",
              value = "1",
              comment = glue("added by rswap v{version} @ {Sys.time()}")
            ))
    }
    # The exact same thing goes for SWCSV...
    if ("SWCSV_TZ" %in% parameters$param) {
      parameters = change_swap_parameter(parameters, "SWCSV_TZ", "1", verbose)
    } else{
      if(verbose){
        cat("\u2795",
            blue("adding", bold("SWCSV_TZ = 1"), "to parameter list"))
      }
      rbind(parameters,
            data.frame(
              param = "SWCSV_TZ",
              value = "1",
              comment = glue("added by rswap v{version} {Sys.time()}")
            ))
    }
    # when more output is needed, I will need to add more and more, so this
    # should / will be updated to do as a loop, as it is done below:


    if(autoset_output){

      if(verbose){cat(blue("\u23fa Autosetting output to match observed file!"),"\n")}

      # load variables and depths
      obs <- load_swap_observed(project_path, archived = F, verbose)
      variables <- obs$observed_variables %>% toupper()
      depths <- get_swap_depths(data = obs$data) %>% sort()
      cat("\u2705",
          blue("Follwing depths detected"),
          green(bold(underline(depths))), "\n")

      # CREATING INLIST_CSV
      # TODO need to expand these... or figure out how to do this.
      # but I think I just need a database of every parameter with their properties
      # (Depth-wise? and Units)
      depthwise <- c("TEMP", "WC", "H")
      nodepth <- c("RAIN", "SNOW", "DRAINAGE", "DSTOR")

      string <- "["
      for (d in depths) {
        if (d == dplyr::last(depths)) {
          string <- glue("{string}-{d}]")
        } else{
          string <- glue("{string}-{d},")
        }
      }
      # rain must always be present! (for the soft calibration plot)
      outstring <- "'RAIN,"
      for (var in variables) {
        if (var %in% depthwise) {
          if (var == dplyr::last(variables)) {
            add_var <- glue("{var}{string}'")

          } else{
            add_var <- glue("{var}{string},")
          }
        } else if (var %in% nodepth) {
          if (var == dplyr::last(variables)) {
            add_var <- glue("{var}'")
          } else{
            add_var <- glue("{var},")
          }
        } else{
          stop(glue("variable {var} not supported yet"))
        }
        outstring <- glue("{outstring}{add_var}")
      }
      parameters <- change_swap_parameter(parameters, "INLIST_CSV", outstring, verbose)
    }
    return(parameters)
  }

#' Change SWAP Parameter
#'
#' This function changes a SWAP parameter. It is passed a parameter dataframe
#' as parsed by `parse_swap_file()` as well as the name of the parameter which
#' should be modified, along with its value.
#'
#' Important: this function only works for single parameters. Any values stored
#' as "tables" in the SWAP input file will need to be modified in another way
#' (WIP)
#'
#' Note: the value of the parameter should be passed as a string in the correct
#' format (ie. correct decimal place), as SWAP/FORTRAN is very particular about
#' this
#'
#' @param param Parameter set (dataframe)
#' @param name name of the parameter to change (string)
#' @param value value the parameter should take on (string)
#' @param verbose print status? (flag)
#'
#' @returns This function returns the same dataframe it was passed, with the
#' parameter value altered.
#'
#' @importFrom glue glue
#' @importFrom crayon bold blue
#'
#' @export
change_swap_parameter <- function(param, name, value, verbose = F){
  version <- utils::packageVersion("rswap") %>% as.character() %>% enc2utf8()
  value2 <- glue(value, " ! changed by rswap v{version} @ {Sys.time()}")
  param$value[which(param$param == name)] = value2

  if(verbose){cat(blue("\U0001f4ac setting"), bold(glue("{name} = {value}")), "\n")}

  return(param)
}

#' Load SWAP tables
#'
#' This function loads all the SWAP tables as parsed by `parse_swap_file()`If
#' the SWAP vectors have not been parsed, then this will be done first using
#' `swap_file`
#'
#' @param project_path Path to project directory (string)
#' @param swap_file name of the swap file to parse (string)
#' @param verbose print status? (flag)
#'
#' @importFrom magrittr %>%
#' @importFrom tibble tibble
#' @importFrom purrr map
#' @importFrom readr read_csv
#' @importFrom stringr str_remove
#' @returns Returns list of tibbles named by their column names.
#'
#' @export
#' @importFrom readr read_csv
load_swap_tables <- function(project_path, swap_file = "swap.swp", verbose = F){

  if (dir.exists(paste0(project_path, "/rswap/")) == FALSE) {
    warning("[rswap] project has not been parsed yet. Doing so now with '", swap_file, "'")
    p <- parse_swap_file(project_path, swap_file, verbose = T)
  }

  table_path <- paste0(project_path, "/rswap/tables/")
  files <- list.files(table_path, full.names = T)
  file_names <- list.files(table_path, full.names = F) %>% stringr::str_remove(".csv")

  # this forces the files to read in all the values as strings, so that no type
  # conversion is done. Changing the type will break the fragile FORTRAN format
  # important line of code here is the "col_types = cols(.default = "c")" which
  # forces every column to be read as char.
  custom_read <- function(x){
    readr::read_csv(file = x, col_names = T, col_types = readr::cols(.default = "c"), quote = '"')
  }

  table_list <- purrr::map(files, custom_read)

  names(table_list) <- file_names

  if (verbose) {
    cat("\U0001f4d6", blue("SWAP table set loaded."), "\n")
  }

  table_list %>% return()

}

#' Load SWAP Vectors
#'
#' This function loads all the SWAP vectors as parsed by `parse_swap_file()`. If
#' the SWAP vectors have not been parsed, then this will be done first using
#' `swap_file`
#'
#'
#' @param project_path path to project_directory (string)
#' @param swap_file name of the swap file to parse (string)
#' @param verbose print status? (flag)
#'
#' @returns Returns a list of vectors
#'
#' @importFrom magrittr %>%
#' @importFrom tibble tibble
#' @importFrom purrr map
#' @importFrom readr read_csv
#' @importFrom stringr str_remove
#'
#' @export
#'
#' @importFrom readr read_csv
load_swap_vectors <- function(project_path, swap_file = "swap.swp", verbose = F) {

  # TODO: load only specific table with extra param

  if(dir.exists(paste0(project_path, "/rswap/"))==FALSE){
    warning("[rswap] project has not been parsed yet. Doing so now with '",
            swap_file, "'")
    p <- parse_swap_file(project_path, swap_file, verbose = verbose)
  }

  vector_path <- paste0(project_path, "/rswap/vectors/")

  files <- list.files(vector_path, full.names = T)
  file_names <- list.files(vector_path, full.names = F) %>% stringr::str_remove(".csv")
  # this forces the files to read in all the values as strings, so that no type
  # conversion is done. Changing the type will break the fragile FORTRAN format
  # important line of code here is the "col_types = cols(.default = "c")" which
  # forces every column to be read as char.
  custom_read <- function(x){
    readr::read_csv(file = x, col_names = T, col_types = readr::cols(.default = "c"), quote = '"')
  }

  vector_list <- purrr::map(files, custom_read)

  names(vector_list) <- file_names


  if (verbose) {
    cat("\U0001f4d6",
        blue("SWAP vector set loaded."),
        "\n")
  }

  vector_list %>% return()
}

#' Load SWAP parameters
#'
#' Loads the SWAP parameters as parsed by `parse_swap_file()` and returns them as
#' a tibble
#'
#' @param project_path path to project directory (string
#' @param swap_file name of the swap file to parse (string)
#' @param verbose print status? (flag)
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#'
#' @returns Tibble of parameter set
#'
#' @export
load_swap_parameters <- function(project_path, swap_file = "swap.swp", verbose = F){

  if(dir.exists(paste0(project_path, "/rswap/"))==FALSE){
    warning("[rswap] project has not been parsed yet. Doing so now with '",
            swap_file, "'")
    p <- parse_swap_file(project_path, swap_file, verbose = verbose)
  }

  if (verbose) {
    cat("\U0001f4d6",
        blue("SWAP parameter set loaded."),
        "\n")
  }

  param_path <- paste0(project_path, "/rswap/parameters/parameters.csv")
  table <- utils::read.table(param_path, header = T, sep = ",", quote = '"') %>% dplyr::tibble()
  table %>% return()


}

#' Write SWAP parameters
#'
#' Writes the SWAP parameter set into the rswap directory in .csv format
#'
#' @param project_path path to project directory
#' @param parameters parameter set as loaded by `load_swap_parameters()` (dataframe)
#' @param verbose print status? (flag)
#'
#' @importFrom crayon blue green underline
#'
#' @export
write_swap_parameters <- function(project_path, parameters, verbose = F){

  file_dir = paste0(project_path, "/rswap/parameters/" )
  file_path = paste0(project_path, "/rswap/parameters/parameters.csv" )

  if(dir.exists(file_dir) == FALSE){
    dir.create(file_dir)
  }

  utils::write.table(
    x = parameters,
    file = file_path,
    sep = ",",
    row.names = F,
    col.names = T,
    quote = T
  )

  if(verbose){
    cat("\U0001f4dd",
        blue("SWAP parameter set written to: \n"))
    cat(green(underline(file_path)),"\n")
  }
}

#' Write SWAP Tables
#'
#' Writes the SWAP table set into the rswap directory in .csv format
#'
#' @param project_path path to project directory
#' @param tables tables as loaded by `load_swap_tables()` (list of dataframes)
#' @param verbose print status? (flag)
#'
#' @importFrom crayon blue green underline
#' @importFrom utils write.table
#' @export
#' @importFrom utils write.table
write_swap_tables <- function(project_path, tables, verbose = F) {

  file_path <-  paste0(project_path, "/rswap/tables/")

  if(dir.exists(file_path) == FALSE){
    dir.create(file_path)
  }

  for (table in tables){

    table_name <- names(table) %>% paste0(collapse = "-")
    table_path <- paste0(file_path, table_name, ".csv")
    utils::write.table(
      x = table,
      file = table_path,
      sep = ",",
      row.names = F,
      col.names = T,
      quote = T
    )
  }

  if(verbose){
    cat("\U0001f4dd",
        blue("SWAP table set written to: \n"),
        green(underline(file_path)),"\n")
  }
}

#' Write SWAP Vectors
#'
#' Writes the SWAP vector set into the rswap directory in .csv format
#'
#' @param project_path path to project directory
#' @param vectors vectors as loaded by `load_swap_vectors()` (list of dataframes)
#' @param verbose print status? (flag)
#' @importFrom crayon blue green underline
#' @importFrom stringr str_remove str_trim
#'
#' @export
write_swap_vectors <- function(project_path, vectors, verbose = F) {


  file_path <-  paste0(project_path, "/rswap/vectors/")

  if(dir.exists(file_path) == FALSE){
    dir.create(file_path)
  }

  for (vector in vectors){

    vector_name <- vector %>% colnames() %>% stringr::str_remove("=") %>% stringr::str_trim()
    vector_path <- paste0(file_path, vector_name, ".csv")
    utils::write.table(
      x = vector,
      file = vector_path,
      sep = ",",
      row.names = F,
      col.names = T,
      quote = T
    )
  }

  if(verbose){
    cat("\U0001f4dd",
        blue("SWAP table set written to: \n"),
        green(underline(file_path)),"\n")
  }
}

#' Change SWAP Table
#'
#' changes the value of a variable in the given row of a SWAP table.
#'
#' @param table can be one SWAP table, or a list of SWAP tables as loaded by
#' `load_swap_tables()`.
#' @param variable the name of the variable you would like to alter (string)
#' @param row the row you would like to alter (integer)
#' @param value the value you would like to enter (must be SWAP-FORTRAN compatible)
#' @param verbose print status? (flag)
#'
#' @importFrom magrittr  %>%
#' @importFrom glue glue
#' @importFrom crayon blue bold
#'
#' @returns Returns the same table / list of tables passed to the function, with
#' the desired variable modified at the given row.
#'
#' @export
change_swap_table <- function(table, variable, row, value, verbose = F){

  # \\b for exact match of var name.
  table_names <- names(table) %>% stringr::str_split("-")
  table_match <- grepl(x = table_names, paste0("\\b", variable, "\\b")) %>% which()

  if((table_match %>% length()) > 1){
    stop("[rswap] more than one table has this variable '",variable,"' this should never happen, report to maintainer!")
  }

  if((table_match %>% length()) < 1){
    stop("[rswap] no columns with '",variable,"' detected in passed table(s) cannot change table value")
  }

  # set the value of the row/var
  table[[table_match]][[variable]][row] = value

  if(verbose){cat(blue("\U0001f4ac setting"), bold(glue("{variable} = {value}")) ,blue(glue("@ row {row}")), "\n")}


  # return the modified table set.
  return(table)
}

#' Change SWAP vector
#'
#' Alters a SWAP vector with the given value at the given index. Can optionally
#' pass a list of vectors as returning by `load_swap_vectors()`, and the variable
#' name, and rswap will find the right vector for you
#'
#' @param vector dataframe of vector to alter. (Optionally, one can pass a list
#'  of vectors as returned by `load_swap_vectors()` and then also pass the variable name)
#' @param index index of the vector to alter (integer)
#' @param value value to set the vector at the given index (SWAP-FORTRAN compatible)
#' @param variable optional, only required if passing a list of multiple vectors. (string)
#' @param verbose print status? (flag)
#'
#' @returns the same vector or vector list as passed, but with the modified value
#'
#' @importFrom tibble  %>%
#' @importFrom glue glue
#' @importFrom crayon blue bold
#'
#' @export
#'
change_swap_vector <- function(vector, index, value, variable = NULL, verbose = F){

  vector_names <- names(vector)
  vector_match <-grepl(x = vector_names, paste0("\\b", variable, "\\b")) %>% which()

  if((vector_match %>% length()) > 1){
    stop("[rswap] more than one vector has this variable '",variable,"' this should never happen, report to maintainer!")
  }

  if((vector_match %>% length()) < 1){
    stop("[rswap] no vectors with '",variable,"' detected in passed vector(s) cannot change vector value!")
  }

  # always column one because its actually a vector! .. this really should be fixed sometime
  vector[[vector_match]][[1]][index] = value

  if (verbose) {
    cat(blue("\U0001f4ac setting"), bold(glue("{variable} = {value}")) , blue(glue("@ index {index}")), "\n")
  }

  return(vector)
}

#' Modify SWAP file
#'
#' This function generates a modified SWAP file, changing the `value` of the
#' passed `variable`. Standard behavior is to pass an `input_file` and an
#' `output_file`. The input file will be parsed by `parse_swap_file()`, the value
#' will be changed, and then a `output_file` will be written to `project_path`.
#' However, the function has a variety of different modes, see **Details**
#'
#' This function has a variety of modes. If you would like to run the function
#' quickly, you can enable `fast` which does **NOT** parse the `input_file`.
#' This only works if you have already parsed the desired data, and it is
#' present in your `rswap` directory.
#'
#' Another way to speed up this function is to disable `write`. this simply does
#' not write your `output_file`
#'
#' **Caution:** Not passing an `output_file` with `write=TRUE` will cause your
#' `input_file` to be overwritten!
#'
#' Note: If you are changing a variable located in a SWAP vector or table, you
#' need to pass the `row` argument (which is an index for vectors, and a normal
#' row for tables)
#'
#' @param project_path path to project directory `string`
#' @param input_file SWAP file name to modify (required if fast=FALSE) `string`
#' @param output_file SWAP file name write (required if write=TRUE) `string`
#' @param variable SWAP variable to change `string`
#' @param value value to assign `string`
#' @param row (optional) only pass if vector (as index) or table. `integer`
#' @param fast (optional) If rswap has already parsed the swap file `flag`
#' @param write (optional) Flag to enable or disable writing of output file `flag`
#' @param verbose (optional) print status? `flag`
#'
#' @returns Returns the path to written output file
#' @export
#'
#' @importFrom readr read_csv cols write_csv
#' @importFrom stringi stri_extract_all_regex
#' @importFrom stringr str_remove str_split
#' @importFrom dplyr last %>%
#' @importFrom crayon blue bold
#'
modify_swap_file <- function(project_path,
                             input_file = NULL,
                             output_file = NULL,
                             variable,
                             value,
                             row = NULL,
                             fast = F,
                             write = T,
                             verbose = F) {

  # check the file type, if we are reading or writing here
  if (write & (input_file %>% is.null())) {
    if(output_file %>% is.null()){
      warning("You need to pass either an input or output file if you have write=TRUE!")
    }

    file_ext <- output_file %>% stringr::str_split("\\.") %>% unlist() %>% dplyr::last()
    if (file_ext != "swp") {
      warning("writing has only been implemented for the main swap file '*.swp', so this probably wont work!.. but it might, who knows!")
    }
  }

  if(write & ((output_file %>% is.null()))){
    warning("Overwriting input file!")
  }

  if(!fast){
    if(is.null(input_file)){
      stop("If you want to use fast=FALSE you need to pass an input file!")
    }

    file_ext <- input_file %>% stringr::str_split("\\.") %>% unlist() %>% dplyr::last()
    if (file_ext != "swp") {
      warning("reading has only been implemented for the main swap file '*.swp', so this probably wont work!.. but it might, who knows!")
    }
  }

  # parse the swap file is it has not been already (as determined by the fast flag)
  if(!fast){
    if(is.null(input_file)){
      stop("If you want to use fast=FALSE you need to pass an input file!")
    }
    parse_swap_file(project_path = project_path, swap_file = input_file, verbose = verbose)
  }

  # determine location of variable (is it a vector, param, or table?)
  vec_path <- paste0(project_path, "/rswap/vectors")
  vec_vars <- list.files(vec_path) %>% stringr::str_remove(".csv")

  tab_path <- paste0(project_path, "/rswap/tables")
  tab_vars_pre <- list.files(tab_path) %>% stringr::str_remove(".csv")
  tab_vars <- tab_vars_pre %>% stringr::str_split("-") %>% unlist()

  if(variable %in% vec_vars){
    if(row %>% is.null()){
      stop("This variable is stored in a vector, therefore you must pass the  'row' arugment as an index!")
    }
    # don't pass swap_file because it should have been parsed already!
    vec_file <- paste0(vec_path, "/", variable, ".csv")
    vec_df <- readr::read_csv(vec_file, col_types = readr::cols(.default = "c"))
    vec_df[[1]][[row]] = value
    if(verbose){cat(blue("\U0001f4ac setting"), bold(glue("{variable} = {value}")), blue(glue("@ index {row}")), "\n")}

    readr::write_csv(x = vec_df, file = vec_file, quote = "all")
  }else if(variable %in% tab_vars){

    if(row %>% is.null()){
      stop("This variable is stored in a table, therefore you must pass the  'row' arugment!")
    }
    # don't pass swap_file because it should have been parsed already!

    find_table <- (stringi::stri_extract_all_regex(str = tab_vars_pre, pattern = variable, simplify = T ) %>% is.na() == FALSE) %>% which()
    single_table_path <- paste0(project_path, "/rswap/tables/",tab_vars_pre[find_table], ".csv")

    tab_df <- readr::read_csv(single_table_path, col_types = readr::cols(.default = "c"))

    col_index <- (colnames(tab_df) == variable) %>% which()
    tab_df[[col_index]][[row]] = value
    if(verbose){cat(blue("\U0001f4ac setting"), bold(glue("{variable} = {value}")), blue(glue("@ row {row}")), "\n")}
    readr::write_csv(x = tab_df, file = single_table_path, quote = "all")
  }else{
    pars <- load_swap_parameters(project_path, verbose = verbose)
    par_vars <- pars$param
    if(variable %in% par_vars){
      change_swap_parameter(param = pars,name = variable, value = value, verbose = verbose)
    }else{
      stop("variable ", variable, " not found")
    }
  }
  if(write){
    if(output_file %>% is.null()){
      path <- write_swap_file(project_path = project_path, outfile = input_file, verbose = verbose)
    }else{
      path <- write_swap_file(project_path = project_path, outfile = output_file, verbose = verbose)
    }
  }else{
    path <- NULL
  }
  return(path)
}


