# creates a package wide enviroment to store SWAPtools data in. (hopefully)
SWAPtools_env <- new.env(parent = emptyenv())


#' Loads the SWAPtools variables database
#'
#' which gives package wide access to the SWAPtools databases, and means
#' individual function calls do not need to re-load the database for their
#' individual environments.
#'
#' @returns returns `TRUE` if database has been loaded (for now just SWAPtools_variables.rds)
#' and `FALSE` if it has not been loaded.
#' @export
load_variables_db <- function() {
  # check if SWAPtools was installed correctly.
  ST <- install_SWAPtools()
  if (ST == FALSE) {
    stop("SWAPtools is required for this functionality!")
  }
  if (exists("SWAPtools_env")) {
    if (is.list(SWAPtools_env$swap_variables)) {
      return(TRUE)
    } else{
      cat("loading SWAPtools variables database..\n")
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


#' Install SWAPtools
#'
#' Installs SWAPtools from `waterwijzerlandbouw.wur.nl/repo`
#'
#' @returns returns `TRUE` if package is already installed, or if installation
#' was successful. returns `FALSE` if installation failed.
#'
#' @export
#'
install_SWAPtools <- function() {
  if ("SWAPtools" %in% installed.packages()) {
    return(TRUE)
  } else{
    cat("SWAPtools is required for this functionality. Installing...\n")

    install.packages(pkg = "SWAPtools",
                     dependencies = TRUE,
                     repos = "https://waterwijzerlandbouw.wur.nl/repo",)
    if ("SWAPtools" %in% installed.packages()) {
      cat("SWAPtools installed succesfully\n")
      return(TRUE)
    } else{
      stop("SWAPtools install unsuccessful! \nYou can try installing the package manually from the SWAP website.")
      return(FALSE)
    }
  }
}

#' Get SWAP Format
#'
#' Gets the format of a SWAP parameter
#' @param parameter SWAP parameter to get the format of.
#
#' @returns Returns unit of passed SWAP variable in string form.
#'
#' @importFrom tibble %>%
#' @importFrom purrr map
#' @export
get_swap_format <- function(parameters) {

  # current issue: omitting the ones it cant find instead of returning ERROR

  status = load_variables_db()
  if (status == FALSE) {
    stop("SWAPtools variable database could not be loaded!")
  }

  par_names <- SWAPtools_env$swap_variables %>% names()

  # hopefully now vectorized:
  matched <- match(parameters, par_names)

  extract <- SWAPtools_env$swap_variables[matched] %>% map(2) %>% unname()

  # convert the nulls to NA
  if(which(extract == "NULL") %>% length() > 0){
    warning("rswap warning: some formats were not detected, and returned as NA!")
    extract <- extract %>% as.character()
    extract[which(extract == "NULL")] = NA
  }

  return(extract %>% unlist())
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
set_swap_format <- function(parameter, value){

  # check if the database was loaded successfully
  status <- load_variables_db()
  if (status == FALSE) {
    stop("SWAPtools variable database could not be loaded!")
  }

  #  "string"  "date"    "switch"  "integer" "vector"  "float"   "table"   "array"
  format <- get_swap_format(parameter)

  supported <- c("switch","string", "integer", "float")

  if((format %in% supported) == FALSE){
    warning("[rswap] Sorry, this format is not supported yet (", format, "). returning NA")
    return(NA)
  }
  # if FORTRAN wants a switch,
  if (format == "switch") {
    if (format %in% c(0, 1)) {
      value %>% as.character() %>% return()
    } else{
      stop("format of ", parameter, " is a SWITCH and must therefore be either '0' or '1', value passed is: ", value)
    }
  }

  # if FORTRAN wants a string,
  # as a string
  if (format == "string") {
    # and it already is a string, then just return it
    value %>% as.character() %>% return()
  }

  # if FORTRAN wants an integer
  if(format == "integer"){

    # and the value is numeric, then round it to the nearest integer and return
    # as a string
    if(value %>% is.numeric()){
      value %>% round(0) %>% as.character() %>% return()
    }

    # if the value is already in string form, turn it into a number,
    # and then round it to the nearest integer, and then return it back as a
    # character again.
    else if(value %>% is.character()){

      # check to see if the conversion fails
      if(value %>% as.numeric() %>% is.na()){
        stop("rswap error: Value conversion failed! from ", value, " to ", value %>% as.numeric())
      }

      # otherwise return.
      value %>% as.numeric() %>% round(0) %>% as.character() %>% return()
    }else{
      stop("rswap error: Do not know how to convert this to integer: " , value)
    }
  }

  # if FORTRAN wants an float
  if (format == "float") {
    # first convert value to string
    value_string <- value %>% as.character()

    # check if it failed
    if(value_string %>% is.na()){
      stop("rswap error: Value conversion failed! from ", value, " to ", value %>% as.numeric())
    }

    # then detect whether it has a decimal
    # (double period to escape the single period special character)
    has_dec <- str_detect(value_string, "..")

    # if the string already has a decimal place, then return as is.
    if (has_dec) {
      value_string %>% return()
    } else{
      # otherwise, add a .0 onto it
      paste0(value_string, ".0") %>% return()
    }
  }

  # if FORTRAN wants a date
  ## TODO: How to implement this? what date format does FORTRAN need?
  ## FORTRAN wants this format: 01-jan-2017..

  # if FORTRAN wants a vector
  ## no idea what to do here. need to check what swap variables are even vectors

  # same goes for ARRAY (what is the difference between vector and array)

  # I currently dont need to convert TABLE but if i do it will be done here.
}


