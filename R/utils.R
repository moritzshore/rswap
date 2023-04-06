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
#' @export
get_swap_format <- function(parameter) {
  status = load_variables_db()
  if (status == FALSE) {
    stop("SWAPtools variable database could not be loaded!")
  }
  extract <- SWAPtools_env$swap_variables[[parameter]]
  return(extract$format)
}

#' Set SWAP Format
#'
#' This function converts a parameter value into the SWAP (FORTRAN) required
#' format. This functionality relies on SWAPtools.
#'
#' @param parameter the SWAP parameter name, in string form
#' @param value the value to be converted
#'
#' @importFrom dplyr %>%
set_swap_format <- function(parameter, value){

  # check if the database was loaded successfully
  status <- load_variables_db()
  if (status == FALSE) {
    stop("SWAPtools variable database could not be loaded!")
  }

  format <- get_swap_format(parameter)


  if(format == "string"){
    value %>% as.character() %>% return()
  }

  if(format == "integer"){
    value %>% round(0) %>% as.character() return()
  }

  if(format == "float"){

    if(value %>% is.character()){
      # need to implement this

      # detect decimals
    }

    if(value %>% is.numeric()){

    }

    value %>% sprintf("%.2f", .)
  }

}


