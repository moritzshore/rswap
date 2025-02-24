install_missing_packs <- function(required_packages) {
  missing_packs <- which((required_packages %in% utils::installed.packages()) == FALSE)
  if(length(missing_packs) > 0){
    cat(bgBlue(white(("[rswap] Missing CRAN packages are required to run this function, installing now:"))),
             paste0(required_packages[missing_packs], sep = " "))
    utils::install.packages(required_packages[missing_packs])
  }
  if(length(which((required_packages %in% utils::installed.packages()) == FALSE)) > 0){
    stop("following packages failed to install:\n",
         required_packages[which((required_packages %in% utils::installed.packages()) == FALSE)])
  }
}
