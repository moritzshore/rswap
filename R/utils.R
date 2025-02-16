#' Install SWAPtools
#'
#' Installs SWAPtools from `waterwijzerlandbouw.wur.nl/repo`
#'
#' @returns returns `TRUE` if package is already installed, or if installation
#' was successful. returns `FALSE` if installation failed.
#'
#' @keywords internal
#'
install_SWAPtools <- function() {
  if ("SWAPtools" %in% utils::installed.packages()) {
    return(TRUE)
  } else{
    cat("SWAPtools is required for this functionality. Installing...\n")

    utils::install.packages(pkg = "SWAPtools",
                            dependencies = TRUE,
                            repos = "https://waterwijzerlandbouw.wur.nl/repo",)
    if ("SWAPtools" %in% utils::installed.packages()) {
      cat("SWAPtools installed succesfully\n")
      return(TRUE)
    } else{
      stop("SWAPtools install unsuccessful! \nYou can try installing the package manually from the SWAP website.")
      return(FALSE)
    }
  }
}

#' Loads the SWAPtools variables database
#'
#' which gives package wide access to the SWAPtools databases, and means
#' individual function calls do not need to re-load the database for their
#' individual environments.
#'
#' @importFrom crayon yellow bold
#'
#' @returns returns `TRUE` if database has been loaded (for now just SWAPtools_variables.rds)
#' and `FALSE` if it has not been loaded.
#'
#' @keywords internal
#' @importFrom utils installed.packages
load_variables_db <- function() {
  # check if SWAPtools was installed correctly.
  if ("SWAPtools" %in% utils::installed.packages() == FALSE) {
    status <- install_SWAPtools()
    if(status==FALSE){stop("SWAPtools not installed")}
  }

  if (exists("SWAPtools_env")) {
    if (is.list(SWAPtools_env$swap_variables)) {
      return(TRUE)
    } else{
      cat(yellow(bold("loading SWAPtools variables database..\n")))
      SWAPtools_variables_path <-
        system.file("rds/swap_variables.rds", package = "SWAPtools")
      SWAPtools_variables <- SWAPtools_variables_path %>% readRDS()
      SWAPtools_env$swap_variables <- SWAPtools_variables
      return(is.list(SWAPtools_env$swap_variables))
    }
  } else{
    stop("SWAPtools_env does not exist. Internal error, should never happen")
  }
}

load_swap_par_db <- function(){

  # load the database from SWAPtools
  status = load_variables_db()
  if (status == FALSE) {
    stop("SWAPtools variable database could not be loaded!")
  }

  # extract the parameter names
  par_names <- SWAPtools_env$swap_variables %>% names()

  # clean up the hierarchy
  first_level_idx  = (!(par_names %>% grepl(pattern = "::"))) %>% which()
  second_level_idx  = par_names %>% grepl(pattern = "::") %>% which()
  remove_before_dot_dot <- function(element){
    if(grepl(pattern = "::", x = element)){
      (element %>% str_split("::", simplify = T))[,2] %>% return()
    }else{
      element %>% return()
    }
  }
  flattened_db <- lapply(par_names, remove_before_dot_dot) %>% unlist()


  # extract flat formats
  extract_format <- function(idx, db){
    db[[idx]]['format'] %>% return()
  }
  flatformats <- lapply(1:length(flattened_db), db = SWAPtools_env$swap_variables, extract_format) %>% unlist()

  # create a flat dictonary with duplicates
  my_dict <- data.frame(parname = flattened_db, parformat = flatformats)
  # remove duplicates
  unique_pars <- flattened_db %>% unique()
  # create  UNIQUE dataframe and join the formats by parname (first match)
  # TODO: double check that this is valid? (check if the duplicate params all have same format (they should?))
  uniq_dic <- data.frame(parname = unique_pars)
  uniq_db <- left_join(uniq_dic, my_dict, by = "parname",multiple = "first")
  # create and return a named vector
  param_format_db <- uniq_db$parformat
  names(param_format_db) <- uniq_db$parname

  # remove all table parameters since I dont think they're actually real parameters
  param_format_db_no_table <- param_format_db[-which(param_format_db == "table")]
  SWAPtools_env$param_format_db <- param_format_db_no_table
  return(is.vector(SWAPtools_env$param_format_db))
}


#' Get SWAP Format
#'
#' Gets the format of a SWAP parameter
#'
#' This code relies on package "SWAPtools"!
#'
#' @param parameter SWAP parameter to get the format of.
#
#' @returns Returns unit of passed SWAP variable in string form.
#'
#' @importFrom dplyr %>%
#' @export
#' @examples
#' get_swap_format("OSAT")
#' get_swap_format("DRFIL")
#' get_swap_format("ISOILLAY")
#' get_swap_format("SWSOPHY")
#' get_swap_format("TSTART")
get_swap_format <- function(parameter) {
  status = load_swap_par_db()

  if(!status){
    stop("error loading parameter format database! contact maintainer?")
  }

  par_db <- SWAPtools_env$param_format_db
  format <- par_db[parameter] %>% unname()
  if(is.na(format)){
    stop("parameter '", parameter, "' not found in database, contact maintainer!")
  }
  return(format)
}

#' Set SWAP Format
#'
#' This function converts a parameter value into the SWAP (FORTRAN) required
#' format. This functionality relies on SWAPtools.
#'
#' @param parameter the SWAP parameter name, in string form
#' @param value the value to be converted
#'
#' @author Moritz Shore, Martin Mulder
#'
#' @importFrom dplyr %>%
#' @importFrom stringr str_detect
#'
#' @export
#'
set_swap_format <- function(value, parameter) {

    # check if the database was loaded successfully
    ST <- install_SWAPtools()
    if (ST == FALSE) {
      stop("SWAPtools is required for this functionality!")
    }
    status <- load_variables_db()
    if (status == FALSE) {
      stop("SWAPtools variable database could not be loaded!")
    }

    #  "string"  "date"    "switch"  "integer" "vector"  "float"   "table"   "array"
    format <- get_swap_format(parameter)

    # if FORTRAN wants a string,
    # as a string
    if (format == "string") {
      value<- value %>% as.character()
      ## add a ' to start if not existing
      need_prefix <- which(substr(value, 0, 1) != "'")
      value[need_prefix] <- paste0("'", value[need_prefix])
      need_postfix <- which(substr(value, nchar(value), nchar(value)) != "'")
      value[need_postfix] <- paste0(value[need_prefix],"'")
      return(value)
    }

    # if FORTRAN wants an integer or switch (switch is not bool)
    if (format %in% c("integer", "switch")) {
      return_val <- value %>% as.numeric() %>% round(0) %>% as.character()
      return(return_val)
    }

    # if FORTRAN wants an float
    if (format == "float") {
      charform <- as.character(value)
      # double \\ negates the regex function of `.`
      no_dec_idx <- grep(pattern = "\\.", x = charform, invert = T)
      charform[no_dec_idx] <- paste0(charform[no_dec_idx], ".0")
      return(charform)
    }

    # if FORTRAN wants a date
    if (format == "date") {
      return_val <- value %>% as.Date() %>% as.character()
      return(return_val)
    }
}
#' Format SWAP Table
#'
#' This handy function converts your dataframe into a SWAP compatible format
#' (mainly, instead of R writing 4 to disk, it writes 4.0 when its a double. Or
#' when its a string, it will write 'hupsel' instead of hupsel. )
#'
#' @param table dataframe of your SWAP table
#'
#' @importFrom purrr map2
#' @importFrom tibble as_tibble
#' @returns A dataframe with formatted values ready to be written by `write_swap_tables()`
#' @export
#' @seealso [write_swap_tables()]
#'
#' @examples
#'
#' vdsp <- data.frame(
#'   ISUBLAY = c(1:5),
#'   ISOILLAY =  c(1:5),
#'   HSUBLAY =  c(15, 20, 5, 50, 50),
#'   HCOMP =  c(1, 5, 1, 5, 10),
#'   NCOMP =  HSUBLAY / HCOMP
#' )
#'
#' format_swap_table(vdsp)
#'
format_swap_table <- function(table, verbose = FALSE) {
  # Vectorized behavior for a list of tables
  if((is.data.frame(table)) == FALSE && is.list(table)){
    lapply(X = table, FUN = format_swap_table)
  }

  if(verbose){
    cat(bold("Formatting:", paste(blue(underline(names(table))), collapse = " ")), "\n")
  }

  format_swap_column <- function(one_column, swapcolname) {
    one_column2 <- one_column[[1]]
    formatted = set_swap_format(parameter = swapcolname, value = one_column2)
    return(formatted)
  }

  # STOP HERE. cant figure out why only first row is pared. try using lapply?
  stop("stopped working here")
  purrr::map2(.x = as.list(table),
              .y =  names(table),
              .f = format_swap_column) %>% as_tibble()
  formatted_table <- purrr::map2(.x = as.list(table),
                                 .y = names(table),
                                 .f = format_swap_column) %>% as_tibble()

  return(formatted_table)
}

#' Format SWAP Vector
#'
#' This handy function converts your vector into a SWAP compatible format
#' (mainly, instead of R writing 4 to disk, it writes 4.0 when its a double. Or
#' when its a string, it will write 'hupsel' instead of hupsel.)
#'
#' @param vector a vector to be formatted for SWAP
#'
#' @returns A vector with formatted values ready to be written by `write_swap_vectors()`
#' @export
#' @seealso [write_swap_vectors()]
#'
#' @examples
#'
#' format_swap_vector(c(10, 10, 10, 20, 30, 50), "DZNEW")
#' format_swap_vector(c("2003-12-31", "2004-12-31"), "OUTDAT")
#' format_swap_vector(c("2003-12-31", "2004-12-31"), "OUTDATINT")
#'
format_swap_vector <- function(vector, name, verbose = FALSE) {

  if(verbose){
    cat(bold("Formatting:", paste(blue(underline(name)), collapse = " ")), "\n")
  }
    flat <- vector %>% unlist() %>% unname()
    formatted = set_swap_format(parameter = name, value = flat)
    return(formatted)
}

