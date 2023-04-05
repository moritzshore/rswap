#' Create the SWAPtools database environment
#'
#' which gives package wide access to the SWAPtools databases, and means
#' individual function calls do not need to re-load the database for their
#' individual environments.
#'
#' @returns returns `TRUE` if database has been loaded (for now just SWAPtools_variables.rds)
#' and `FALSE` if it has not been loaded.
#' @export
create_stdb <- function() {
  # check if SWAPtools was installed correctly.
  ST <- install_SWAPtools()
  if (ST == FALSE) {
    stop("SWAPtools is required for this functionality!")
  }

  if(exists("SWAPtools_variables") == FALSE) {
    cat("loading SWAPtools database...\n")
    SWAPtools_variables_path <- system.file("rds/swap_variables.rds", package = "SWAPtools")
    # probably shouldn't do it like this because it shows up in the environment pane,
    # but for now its going to have to do. It must be possible to somehow assign to
    # package environment but i havent figured it out yet
    SWAPtools_variables <<- SWAPtools_variables_path %>% readRDS()
  }

  status <- is.list(SWAPtools_variables)


  if (status) {
    #cat("SWAPtools database loaded.\n")
  } else{
    stop("could not load SWAPtools database")
  }

  return(status)
}

#' Install SWAPtools
#'
#' Installs SWAPtools from waterwijzerlandbouw.wur.nl/repo
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

#' check SWAP format
#'
#' Gets the format of a SWAP parameter
#' @param parameter
#'
#' @returns Returns unit of passed SWAP variable in string form.
#' @export
check_swap_format <- function(variable){
  status = create_stdb()
  if(status==FALSE){
    stop("SWAPtools variable database could not be loaded!")
  }

  extract<-SWAPtools_variables[[variable]]

  return(extract$format)

  SWAPtools_variables$SHAPE$format

  SWAPtools_variables$SWBLC$format

}
