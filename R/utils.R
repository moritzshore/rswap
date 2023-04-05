#' Create the SWAPtools database environment
#'
#' which gives package wide access to the SWAPtools databases, and means
#' individual function calls do not need to re-load the database for their
#' individual environments.
#'
#' @returns returns `TRUE` if database has been loaded (for now just SWAP_variables.rds)
#' and `FALSE` if it has not been loaded.
#' @export
create_stdb <- function() {
  # check if SWAPtools was installed correctly.
  ST <- install_SWAPtools()
  if (ST == FALSE) {
    stop("SWAPtools is required for this functionality!")
  }

  if (exists("swap_variables") == FALSE) {
    cat("\n loading SWAPtools database...\n")
    swap_variables_path <- system.file("rds/swap_variables.rds", package = "SWAPtools")
    swap_variables <<- swap_variables_path %>% readRDS()
  }

  status <- is.list(swap_variables)


  if (status) {
    cat("\nSWAPtools database loaded.\n")
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
    cat("\n SWAPtools is required for this functionality. Installing...\n")

    install.packages(pkg = "SWAPtools",
                     dependencies = TRUE,
                     repos = "https://waterwijzerlandbouw.wur.nl/repo",)
    if ("SWAPtools" %in% installed.packages()) {
      cat("\nSWAPtools installed succesfully\n")
      return(TRUE)
    } else{
      stop("SWAPtools install unsuccessful! \nYou can try installing the package manually from the SWAP website.")
      return(FALSE)
    }
  }
}

#' Get SWAP unit
#'
#' Gets the unit of the a SWAP variable.
#' @param variable
#'
#' @returns Returns unit of passed SWAP variable in string form.
#' @export
get_swap_unit <- function(variable){

  status = create_stdb()
  if(status==FALSE){
    stop("SWAPtools variable database could not be loaded!")
  }

  print(st_db$swap_variables$PROJECT$label)
}
