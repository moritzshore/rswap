### This file contains the code related to meteorological operations.

## TODO:
# - Add functionality to convert the regular date to a DD MM YYYY column set.
#
# - Add a plot-meteo function to analyze the input data.
#
# - in autoset functionality, the dates of the model run could be auto set to
#   the met data dataframe.
#
# - The plotting functions could be changed to implement the "load_meteo" func
#
# - Add the force functionality to load meteo


#' Load SWAP meteo data
#'
#' This function loads the meteo file `rswap_meteo.csv` into the R-environment,
#' allowing to modify the contents at will. Descriptions of the variables and
#' their units are provided below.
#'
#' **NOTE:** This is my best "guess" at what the units of the meteo input file
#' are. I could not find any official definitions in the manual, so use with
#' caution and let me know if I got anything wrong.
#'
#' * `Station`  --  a string, name of station, ie. "'Robert'"
#' * `DD`       -- day of month, ie. "01"
#' * `MM`       -- month number, ie. "01"
#' * `YYYY`     -- year, ie "2001"
#' * `Rad`      -- solar radiation, unit is  kJ/m^2 ie "3810.1"
#' * `Tmin`     -- Minimum temperature (daily) in degrees Celsius, ie "-3.2"
#' * `Tmax`     -- Maximum temperature (daily) in degrees Celsius, ie. "5.3"
#' * `Hum`      -- Humidity (kPa?) (See note below) ie. "0.524"
#' * `Wind`     -- Wind speed (m/s) ie. "4.5"
#' * `Rain`     -- precipitation, (mm) ie. "0.2"
#' * `ETref`    -- (optional) Reference Evapotranspiration (mm) ie. "0.4"
#' * `Wet`      -- (optional) Rain intensity, fraction of day between 0 and 1, ie. "0.029"
#' @param project_path Path to project directory (string)
#' @param verbose print status? (flag)
#' @return A dataframe of the meteo data
#' @export
#' @importFrom readr read_csv
#'
#' @examples
#' #tbc
#'
#' @seealso [write_swap_meteo()] [est_avp()]
#'
load_swap_meteo <- function(project_path, verbose = FALSE){

  ## TODO, add forcing

  met_path <- paste0(project_path, "/rswap_meteo_data.csv")

  if(file.exists(met_path)){
    if(verbose){cat("met file exists in project path, loading\n")}
  }
  else{
    if(verbose){cat("met file does not exist in project path, creating template\n")}
    pkg_path <- system.file(package = "rswap")
    extdata <- paste0(pkg_path, '/extdata/rswap_example_input')
    inst_path <- paste0(extdata, "/rswap_meteo_data.csv")
    status = file.copy(from = inst_path, to = met_path)
    if(status == FALSE){stop("error creating meteo template!")}
  }
    met_df <- readr::read_csv(met_path, show_col_types = verbose)
    return(met_df)
}


#' Saves SWAP meteo data to project
#'
#' This function writes the provided dataframe back into the rswap template
#' csv file, and optionally also exports the data into a SWAP compatible format
#' (.met). Additionally, one can automatically update the parameters `TSTART`,
#' `TEND` and `METFIL` by setting `autoset = TRUE`.
#'
#' Writes a / the SWAP meteo dataframe as loaded by `load_swap_meteo()`
#' @param project_path Path to project directory (string)
#' @param data dataframe with SWAP meteo data as loaded by `load_swap_meteo()`
#' @param export (optional) flag, should the data be written to SWAP format?
#' @param name (optional) string, desired name of the meteo file.
#' @param autoset (optional) flag, automatically adjusts SWAP project with the
#'   start and end times of the provided data, as well as the metfile name.
#' @param swap_file (optional) name of the swap file to modify (swap.swp by
#'   default)
#' @param verbose (flag) print actions to console?
#' @return path to where the met file was written.
#' @export
#' @importFrom readr write_csv
#' @importFrom dplyr %>%
#' @importFrom stringr str_remove_all
#' @importFrom crayon blue cyan bold italic underline
#'
#' @seealso [load_swap_meteo()] [est_avp()]
#'
#' @examples
#' #tbc
write_swap_meteo <- function(project_path,
                            data,
                            export = TRUE,
                            name = NULL,
                            autoset = FALSE,
                            swap_file = "swap.swp",
                            verbose = TRUE) {

  if(export == FALSE){
    if(autoset){stop("Cannot autoset if export is false!")}

    write_path <- paste0(project_path,"/", "rswap_meteo_data.csv")
    readr::write_csv(x = data, file = write_path)
    if(verbose){cat("meteo data saved in: \n",
                    underline(italic(blue(write_path))), "\n",
                    italic("note: data not written to SWAP format because `export == FALSE`"))}
    return(write_path)

  }

  if(is.null(name)){
    name <- data$Station[1] %>% str_remove_all("'")
  }

  write_path <- paste0(project_path,"/", name, ".met" )
  if(verbose){cat(blue("> writing SWAP met file to "), underline(write_path))}
  readr::write_csv(x = data, file = write_path)

  write_path2 <- paste0(project_path,"/", "rswap_meteo_data.csv")
  readr::write_csv(x = data, file = write_path2)
  if(verbose){cat("meteo data saved in: \n",
                  underline(italic(blue(write_path2))), "\n")}

  if(autoset){
    swap_pars <- load_swap_parameters(project_path = project_path, file_type = ".swp", verbose = verbose)
    old_tstart <- swap_pars$value[which(swap_pars$param == "TSTART")]
    old_tend <- swap_pars$value[which(swap_pars$param == "TEND")]
    old_statname <- swap_pars$value[which(swap_pars$param == "METFIL")]

    n <- length(data$DD)
    new_tstart <- paste0(data$YYYY[1], "-", data$MM[1] ,"-", data$DD[1])
    new_tend <- paste0(data$YYYY[n], "-", data$MM[n] ,"-", data$DD[n])
    new_statname <- paste0("'", name, ".met", "'")

    swap_pars$value[which(swap_pars$param == "TSTART")] <- new_tstart
    swap_pars$value[which(swap_pars$param == "TEND")] <- new_tend
    swap_pars$value[which(swap_pars$param == "METFIL")] <- new_statname

    if(verbose){
      cat("changing ",bold("TSTART")," and ", bold("TEND"), " from ", underline(blue(paste0("[", old_tstart,",", old_tend,
          "]"))), " to ", bold(cyan(underline(paste0("[",new_tstart, ",", new_tend, "]")))), " and changing ", bold("METFIL"),  " from ",
          blue(underline(old_statname)), " to ", cyan(underline(bold(new_statname))), "\n", sep = "")
    }

    write_swap_parameters(project_path = project_path, parameters = swap_pars, type = "main", verbose = verbose)

  }

  return(write_path)
}

#' Estimate Actual Vapour Pressure
#'
#' Converts relative humidity (RH) to actual vapour pressure (AVP) using an
#' estimation with maximum and minimum temperature. This function exists because
#' SWAP uses AVP whereas many datasets contain RH. Read more on [fao.org](https://www.fao.org/4/x0490e/x0490e07.htm)
#'
#' @seealso [load_swap_meteo()] [write_swap_meteo()]
#' @param tmin vector, in kelvin (K)
#' @param tmax vector, in kelvin (K)
#' @param rh vector, in % (ie. 53.2)
#'
#' @return vector of AVP, rounded to 2 decimal placews
#' @export
#'
#' @examples
#'
#' est_avp(tmin = c(1.8, 1.9, 2.0), tmax =	c(6, 7, 8),
#'                    rh = c(60.2, 40.5, 90.2))
#'
est_avp <- function(tmin, tmax, rh) {
  # https://www.fao.org/4/x0490e/x0490e07.htm

  e_0 <- function(temp) {
    0.6108 * exp(17.27 * (temp) / (temp + 237.3))
  }
  e_s <- (e_0(tmax) + e_0(tmin)) / 2
  e_a <- (rh / 100) * e_s

  return(round(e_a, 2))
}
