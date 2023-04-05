#' Get SWAP Units
#'
#' returns default swap units for the given variable (Work in progress). Will
#' possibly also include a database on what variables are depth-wise.
#'
#' @param variable SWAP variable (string)
#' @returns Returns unit in string from
#' @importFrom tibble %>%
#' @export
get_swap_units <- function(variable){
  unit = NA
  # todo: improve format and maybe use expression()

  if(variable == "WC"){
    unit = "cm3/cm3"
  # check if SWAPtools was installed correctly.
  ST <- install_SWAPtools()
  if(ST==FALSE){
    stop("SWAPtools is required for this functionality!")
  }
  return(NA)
}

  if(variable == "H"){
    unit = "cm"
  }

  if(variable == "TEMP"){
    unit = "oC"
#' Create the SWAPtools database environment
#'
#' which gives package wide access to the SWAPtools databases, and means
#' individual function calls do not need to re-load the database for their
#' individual environments.
#'
#' @returns returns TRUE if database has been loaded (for now just SWAP_variables.rds)
#' and FALSE if it has not been loaded.
create_stdb <- function() {
  # check if SWAPtools was installed correctly.
  ST <- install_SWAPtools()
  if (ST == FALSE) {
    stop("SWAPtools is required for this functionality!")
  }

  if(variable == "DRAINAGE"){
    unit = "cm/d"
  # What does this do?
  # it checks to see if the database already exists, and if it does, if
  # swap_variables has already been loaded. if it hasn't, it loads it. if the
  # environment doesn't exist, it creates it. if the object exists, but is not
  # an environment, then it overwrites it. (although this last case should never
  # actually happen). This could be rewritten with recursion, but its already
  # 4 PM..
  if (exists("st_db")) {
    if (is.environment(st_db)) {
      if (exists("st_db$swap_variables")) {
        return(TRUE)
      }
      else{
        cat("\n loading SWAPtools database...\n")
        swap_variables <-
          system.file("rds/swap_variables.rds", package = "SWAPtools")
        st_db$swap_variables <- swap_variables %>% readRDS()
      }
    } else{
      st_db  <- new.env(parent = emptyenv())
      cat("\n loading SWAPtools database...\n")
      swap_variables <-
        system.file("rds/swap_variables.rds", package = "SWAPtools")
      st_db$swap_variables <- swap_variables %>% readRDS()
    }
  } else{
    st_db  <- new.env(parent = emptyenv())
    cat("\n loading SWAPtools database...\n")
    swap_variables <-
      system.file("rds/swap_variables.rds", package = "SWAPtools")
    st_db$swap_variables <- swap_variables %>% readRDS()
  }
  return(is.list(st_db$swap_variables))
}



  if(variable == "RAIN"){
    unit = "cm/d"
  }

  if(unit %>% is.na()){warning("variable not listed")}
#' Install SWAPtools
#'
#' Installs SWAPtools from waterwijzerlandbouw.wur.nl/repo
#'
#' @returns returns TRUE if package is already installed, or if installation
#' was successful. returns FALSE if installation failed.
#'
install_SWAPtools <- function() {
  if ("SWAPtools" %in% installed.packages()) {
    return(TRUE)
  } else{
    cat("\n SWAPtools is required for this functionality. Installing...\n")

  return(unit)
    install.packages(pkg = "SWAPtools",
                     dependencies = TRUE,
                     repos = "https://waterwijzerlandbouw.wur.nl/repo",)
    if ("SWAPtools" %in% installed.packages()) {
      cat("\nSWAPtools installed succesfully\n")
      return(TRUE)
    } else{
      stop("SWAPtools install unsuccessful!")
      return(FALSE)
    }
  }
}
