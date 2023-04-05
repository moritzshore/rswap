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
  }

  if(variable == "H"){
    unit = "cm"
  }

  if(variable == "TEMP"){
    unit = "oC"
  }

  if(variable == "DRAINAGE"){
    unit = "cm/d"
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
      cat("\n SWAPtools install unsuccessfull! ")
      return(FALSE)
    }
  }
}
