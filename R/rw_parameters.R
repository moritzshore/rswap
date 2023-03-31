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
clean_swp_file <- function(project_path, swap_file = "swap.swp") {
  # TODO: find out for which SWAP input files this works, other than main
  # path and read
  path <- paste0(project_path, "/", swap_file)
  swp <- readLines(path)
  # remove all the comment lines starting with *,
  comment_lines = (swp %>% substr(x = ., 1, 1) == "*") %>% which()
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

#' Parse SWAP File
#'
#' This function reads in a SWAP main file and returns the parameter set in a
#' dataset form. It also converts the tables in the main file into .csv format
#' and saves them in the rswap directory. In this format it is easy to make
#' alterations to the SWAP parameters and tables.
#'
#' I am working on getting tables to be returned as a "list of dataframes"
#' instead of saving them as a file.
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
#'
#' @returns Returns SWAP parameters in a dataframe format and path to where the tables
#' were saved in a csv format (until i figure out how to return an array of
#' dataframes)
#'
#' @export
#'
parse_swp_file <- function(project_path, swap_file = "swap.swp", verbose = F) {

    # TODO rename to parse_swap_file()
    # finds the end of the table when passed a snipped of the swap file
    find_eot <- function(short_swp) {
      first_element <- short_swp %>%  str_trim() %>% str_split("\\s+") %>% map(1) %>% unlist()
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
    swp <- clean_swp_file(rswap_dir, swap_file = swap_file)

    # predefine for-loop vars
    par_df <- data.frame()
    tab_df <- list()
    new_i = 0

    table_path = paste0(project_path, "/rswap/tables")
    unlink(table_path, recursive = T)
    dir.create(table_path, showWarnings = F)

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
      is_param <- line %>% grepl(x = ., "=")
      is_table <- (line %>% grepl(x = ., "=") == FALSE)
      special_case <- FALSE

      # the routine for if its a parameter:
      if (is_param == TRUE) {

        # extract the comment denoted by a !
        comment = line %>% str_split("!") %>% unlist() %>% nth(2)

        # split the data from the comment using "!" as an indicator
        line_split = line %>% str_split("!") %>% unlist() %>% nth(1)

        # split that line again, based off the equals sign. left of the
        # equals sign is the parameter name, to the right is the value
        param = line_split %>% str_split("=") %>% unlist() %>% str_trim() %>% nth(1)
        value = line_split %>% str_split("=") %>% unlist() %>% str_trim() %>% nth(2)

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

          header = line %>% str_trim()
          width = 1
          swap_table_end <- find_eot(short_swp = swp[(i + 1):length(swp)])
          swap_table <- swp[i:(i + swap_table_end-1)]

          table_name = header %>% str_remove("=") %>% str_trim()
          write.table(
            x = swap_table,
            file = paste0(table_path, "/", table_name, ".csv"),
            sep = ",",
            row.names = F,
            col.names = F,
            quote = F
          )

          # skip the next rows because you already read them in
          new_i = i + swap_table_end

        }else{
          # Routine for a Table, not special case
          header = line  %>%  str_split("!") %>% map(1)  %>% str_trim() %>% str_split("\\s+") %>%  unlist()
          width = header %>% length()

          swap_table_end <- find_eot(short_swp = swp[(i + 1):length(swp)])
          swap_table <- swp[i:(i + swap_table_end-1)]
          swap_table[1] <- header %>% paste(collapse = " ") %>% str_trim()
          swap_table2 <- swap_table %>% str_trim() %>% str_split("\\s+")

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
        } # END of table not special
      } # END of table
    } # END of for loop

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

#' Write SWAP File
#'
#' Writes a SWAP main file from the passed parameter dataframe, and the passed path
#' to the tables stored as .csv from the `parse_swap_file()` function. The SWAP
#' main file will be written to the location specified by `outpath`.
#'
#' This function currently is only intended for the SWAP main file, but will be
#' expanded to handle the other SWAP input files over time.
#'
#' @param parameters dataframe of project parameters as created by `parse_swap_file()` (dataframe)
#' @param table_path path of project tables as created by `parse_swap_file()` (string)
#' @param outpath path where to save the swap file (string)
#' @param verbose print status? (flag)
#'
#' @returns Returns path of written file
#'
#' @importFrom dplyr %>%
#'
#' @export
#'
write_swap_file <- function(parameters, table_path, outpath, verbose = F) {

    version <- packageVersion("rswap") %>% as.character() %>% enc2utf8()

    # Give a message if the file was overwritten
    removed <- file.remove(outpath) %>% suppressWarnings()
    if (removed & verbose) {
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

    par_write = paste(parameters$param, "=", parameters$value)

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

      # band-aid fix for the special cases
      # TODO improve this
      colnames(read) <- colnames(read) %>% str_replace("\\..", " = ")

      write.table(
        read,
        file = outpath,
        quote = F,
        row.names = F,
        col.names = T,
        append = T,
        sep = " "
      ) %>% suppressWarnings()

      # Theoretically don't need this, but its nice to have
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
#' This function alters the output parameters of your SWAP input files.
#'
#' Currently it only modifies INLIST_CSV, but more functionality will be placed
#' here over time.
#'
#' @param parameters parameter dataframe as returned by `parse_swap_file()`
#' @param variables  variables desired for output (string)
#' @param depths depths depth at which to output them (string)
#' @param verbose print status? (flag)
#'
#' @importFrom glue glue
#' @importFrom dplyr %>% last
#'
#' @returns Returns parameter dataframe with modified INLIST_CSV parameter.
#'
#' @export
#'
set_swap_output <- function(parameters, variables, depths, verbose = F) {

    variables <- variables %>% toupper()

    # CREATING INLIST_CSV
    # TODO need to expand these... or figure out how to do this.
    # but I think I just need a database of every parameter with their properties
    # (Depth-wise? and Units)
    depthwise <- c("TEMP", "WC", "H")
    nodepth <- c("RAIN", "SNOW", "DRAINAGE", "DSTOR")

    string <- "["
    for (d in depths) {
      if (d == last(depths)) {
        string <- glue("{string}-{d}]")
      } else{
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
      } else if (var %in% nodepth) {
        if (var == last(variables)) {
          add_var <- glue("{var}'")
        } else{
          add_var <- glue("{var},")
        }
      } else{
        stop(glue("variable {var} not supported yet"))
      }
      outstring <- glue("{outstring}{add_var}")
    }

    parameters <- change_swap_par(parameters, "INLIST_CSV", outstring)

    if (verbose) {
      cat(glue("...updating parameter:\n INLIST_CSV = {outstring} \n"))
    }
    return(parameters)
  }
