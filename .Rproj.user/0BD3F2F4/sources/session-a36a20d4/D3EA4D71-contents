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

  return(unit)
}
