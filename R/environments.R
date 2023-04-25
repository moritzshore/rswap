# creates a package wide enviroment to store SWAPtools data
SWAPtools_env <- new.env(parent = emptyenv())

# environment to store rswap data in.
rswap_env <-  new.env(parent = emptyenv())
#
#
# if (exists("SWAPtools_env")) {
#   if (is.list(SWAPtools_env$swap_variables)) {
#     return(TRUE)
