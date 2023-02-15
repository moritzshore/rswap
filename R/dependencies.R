# SWAPUI dependency file
# Author: Moritz Shore | moritz.shore@nibio.no | 9.1.23
# Contents: loads all the libraries needed for the project, as well as all
#           the functions. Includes to-do list at the bottom.

# multiple functions that make the script tick
source("dependent/functions.r")
# plot allowing you to compare variables
source("dependent/comparative_plot.r")
# Statistics plot
source("dependent/plot_statistics.r")
# The soft calibration plot
source("dependent/soft_calibration_plot.R")
# The over/under estimate graph
source("dependent/over_under.R")
# Auto calibration / sensitivity functions
source("dependent/auto_cal_functions.R")
# Functions for running the auto cal algorithm
source("dependent/autocal_algorithm_functions.R")

# loads and/or installs required packages
load_libraries(
  required_packages = c(
    "tidyverse",
    "plotly",
    "vroom",
    "reshape2",
    "RColorBrewer",
    "beepr",
    "readxl"
  )
)


print("code loaded")

# TODO  -----

# Switch to generalized process per variables: for depth for var
# Implement Observed H, TEMP, etc.
# total amount of water stored in the soil profile?
# implement a REVERT CHANGE function?
# implement GUI?
# re implement archive_plot?


