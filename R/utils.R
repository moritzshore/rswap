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
        cat("\nSWAPtools database loaded.\n")
        return(TRUE)
      }
      else{
        # if the enthronement already exists, but the database doesn't
        cat("\n loading SWAPtools database...\n")
        swap_variables <-
          system.file("rds/swap_variables.rds", package = "SWAPtools")
        st_db$swap_variables <- swap_variables %>% readRDS()
      }
    } else{
      # if the environment does not exist, but the var name does (should never happen)
      st_db  <- new.env(parent = emptyenv())
      cat("\n loading SWAPtools database...\n")
      swap_variables <-
        system.file("rds/swap_variables.rds", package = "SWAPtools")
      st_db$swap_variables <- swap_variables %>% readRDS()
    }
  } else{
    # if the object, environment, and database don't exist yet
    st_db  <- new.env(parent = emptyenv())
    cat("\n loading SWAPtools database...\n")
    swap_variables <-
      system.file("rds/swap_variables.rds", package = "SWAPtools")
    st_db$swap_variables <- swap_variables %>% readRDS()
  }
  return(is.list(st_db$swap_variables))
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
