
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
#'
#' @return
#' @export
#'
#' @examples
load_swap_meteo <- function(){

}
