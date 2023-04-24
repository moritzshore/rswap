# rswap <img src="man/figures/logo.png" align="right" height="138" />


rswap is an R-package designed to help interface and work with [SWAP4](https://www.swap.alterra.nl/) [[1]](#1). It consists of a variety of functions that assist the user in otherwise tedious and repetitive tasks during the calibration process. The scope of the package will hopefully be expanded overtime to include sensitivity analysis, multi-core parallelization, autocalibration / PEST integration, scenario runs, and much more. **DISCLAIMER: rswap is very much in development, and therefore not robustly tested, nor extremely stable. use at your own risk, and be critical of the results, for now..**

![GitHub R package version](https://img.shields.io/github/r-package/v/moritzshore/rswap) ![GitHub issues](https://img.shields.io/github/issues/moritzshore/rswap) ![GitHub](https://img.shields.io/github/license/moritzshore/rswap) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7795153.svg)](https://doi.org/10.5281/zenodo.7795153)

# Table of contents

1.  [Installing rswap](#install)
2.  [Running SWAP](#run)
3.  [Accessing data](#data)
4.  [Visuals](#visuals)
5.  [Model Performance](#performance)
6.  [Saving model runs](#saving)
7.  [Comparing model runs](#compare)
8.  [Modification of Parameters](#mod)
9.  [SWAPtools Integration](#swaptools)
10. [Miscellaneous functions](#misc)
11. [Roadmap](#roadmap)
12. [Support and Contributing](#support)
13. [Acknowledgements](#ack) [References](#ref)

## Installing `rswap` <a name="install"></a>

You can install `rswap` from GitHub:

``` r
# install remotes, if not already present
install.packages("remotes")

remotes::install_github(repo = "moritzshore/rswap", ref = remotes::github_release())

library(rswap)
```

A useful place to start would be the `rswap_init()` function. This function creates the "Hupselbrook" example case in the same directory as your `swap.exe`. It goes on to run the setup, copy in the observed data template file, and plot the results. If this function finished successfully, you know `rswap` is working properly.

``` r
project_path <- rswap_init(swapexe = "C:/path/to/swap.exe")
```

> You can use this `project_path` to run all the following example code on this page!.

**⚠️IMPORTANT⚠️** Its important to know that `rswap` never modifies files in your project directory (`project_path`) unless stated otherwise, instead all files are *copied* from `project_path` to `project_path/rswap`, modified there, and executed. All results are stored there as well and will be overwritten over time. Remember to save your results if you would like to keep them (`save_run(), write_swap_file()`), and remember that anything in the `project_path/rswap` directory is temporary!

## Running SWAP <a name="run"></a>

The SWAP model can be run using the `run_swap()` function. It needs to know where your model setup is located (`project_path`). The `swap.exe` must be located in the parent directory of `project_path`!

``` r
run_swap(project_path, autoset_output = TRUE)
```

`run_swap()` can be further customized with the following parameters:

-   `swap_file` can be set to a custom name for your SWAP main file (\*.swp)
-   `autoset_output` can be enabled, such that the output of the SWAP model matches your provided observed data
-   `timeout` sets the max allowed runtime of SWAP
-   `verbose` Prints everything the function does to the console with pretty colors

## Accessing Data <a name="data"></a>

### Modelled Data

To read the output of your executed SWAP run, you can use the following command:

``` r
modelled_data <- read_swap_output(project_path)
```

`read_swap_output()` returns two dataframes, `daily_output` which contains depth wise values of various variables. The other is `custom_depth` which contains custom variables at custom depths either explicitly altered by the user, or automatically parsed by the `autoset_output` flag of `run_swap()`. This dataframe is used widely throughout the package. (more/all results will be added over time)

### Observed Data

As `rswap` heavily revolves around calibration, observed data is of high importance. When running `build_rswap_directory()` a template observed file will be copied into the `project_directory` (if not already existing). It is up to the user to fill this file with the appropriate data and column names. Documentation for how to do this is in a companion text file.

To load your observed file, you can use the following command:

``` r
observed_data <- load_observed(project_path)
```

Which will return a dataframe of the user-entered observed data, as well as a vector for the detected variables.

To find out what depths your observed variables have, you can use the following command:

``` r
get_depths(observed_data$data)
```

..this can also be filtered by a specific variable by passing `variable`

## Visuals <a name="visuals"></a>

There are a variety of functions used to visualize your SWAP data, such as `plot_over_under()`

``` r
plot_over_under(project_path, variable = "WC", depth = c(15, 40, 70))
```

`plot_over_und()` can be passed a `variable`, as well as a vector `depth`.

<p align="center">

<img src="man/figures/overunder.png" width="60%" height="60%"/>

</p>

> (this plot heavily relies on code from [Neal Grantham](https://www.nsgrantham.com/fill-between-two-lines-ggplot2/))

For a more detailed look at multiple variables at once, you can use the `soft_calibration_plot()`

``` r
soft_calibration_plot(project_path, vars = c("H", "WC", "DRAINAGE"))
```

This function can be passed up to 3 variables, and will display them interactively on the same plot. If observed data is available, they will be displayed as well.

<p align="center">

<img src="man/figures/softcalplot.png" width="60%" height="60%"/>

</p>

## Model Performance <a name="performance"></a>

A few functions focus on assessing model performance by comparing modelling values to user provided observed values. This functionality is based on the `get_performance()` function:

``` r
get_performance(project_path, stat = "NSE", variable = "WC", depth = 15)
```

This function is very flexible and can be passed any number of `variables`, `depths`, and performance indicators `stat` (currently supported are `NSE`, `PBIAS`, `RSR`, and `RMSE`.

## Saving Runs <a name="saving"></a>

While calibrating a model it can be useful to keep track of different model runs with different parameterization. `rswap` aids this process with a variety of functions, such as

``` r
save_run(project_path, run_name = "COFRED = 0.35")
```

This function saves your entire model set up in a directory (`project_directory/rswap_saved`). Once a model run has been saved, it can be compared to other model runs, with the following functions.

## Comparing Runs <a name="compare"></a>

``` r
comparative_plot(project_path, variable = "WC", depth = 15)
```

<p align="center">

<img src="man/figures/compareplot.png" width="60%" height="60%"/>

</p>

Once again, this function is quite flexible, and can be passed any available `variable` or `depth`

You can compare the performance of your various model runs by using the `plot_statistics()` function.

``` r
plot_statistics(project_path, var = "WC", depth = c(15,40,70))
```

<p align="center">

<img src="man/figures/stat_plot1.png" width="60%" height="60%"/>

</p>

This plot is equally flexible and can be passed any `variable` and any amount of `depths` for any supported `stat`. the graph type can be switched between `default`, `sorted` and `ggplot`

## Modification of Parameters <a name="mod"></a>

Changing of parameters, tables, and vectors of the SWAP main file can be done with `rswap`. The simple way of doing this is by using the `modify_swap_file()` function:

``` r
modify_swap_file(project_path = project_path,
  input_file = "swap.swp", output_file = "swap_mod.swp",
  variable = "ORES", value = "0.43", row = 2)
```

This function has many different behaviors depending on which flags are enabled, and which arguments are passed. For more information, check the **Details** in the help page of the function.

**⚠️ If used incorrectly, this function can overwrite your swap file!** *Check the Details page!*

`rswap` uses a whole set of functions for the reading, altering, and writing of SWAP parameters. While `modify_swap_file()` covers most use-cases, the underlying functions can be of use as well, for more advanced work flows. You can read more about them in their documentation.

#### General parameter functions:

``` r
# removes any non-essential data from the input file:
clean_swp_file(project_path) 
# parses the data to be R-readable:
parse_swp_file(project_path) 
# writes the SWAP main file sourced from ".csv" files stored in the rswap directory
write_swap_file(project_path, outfile = "swap_modified.swp")
```

#### Parameter specific functions:

``` r
param <- load_swap_parameters(project_path)
param <- change_swap_par(param, name = "SHAPE", value = "0.75")
write_swap_parameters(project_path, param)
```

#### Table specific functions:

``` r
tables <- load_swap_tables(project_path)
tables <- change_swap_table(tables, variable = "OSAT", row = 1, value = "0.34")
write_swap_tables(project_path, tables)
```

#### Vector specific functions:

``` r
vectors <- load_swap_vectors(project_path)
vectors <- change_swap_vector(vectors, variable = "OUTDAT", index = 1, value = "10-jun-2013")
write_swap_vectors(project_path, vectors)
```

⚠️ You have the choice of passing the value in `character` format as shown above, to assure `FORTRAN` compatible format, or you can use the `set_swap_format()` function, to convert your value to the `FORTRAN` compatible format.

To run SWAP with the modifications you've made to your parameters, you need to make sure you `write_swap_file()` before running `run_swap()` -- **All changes in `/rswap/` are temporary until you write your SWAP file!**

> This functionality is currently only tested for the SWAP main file. Support for the other SWAP input files is coming soon©

## SWAPtools integration <a name="swaptools"></a>

The following features are possible when using `rswap` with another SWAP-related R-package: `SWAPtools`

`get_swap_format()` returns the format of the given parameter, whereas `set_swap_format()` forces the value of the given parameter into the FORTRAN-required format. These functions rely on data from package `SWAPtools`. (Over time, `change_swap_par()` will use these automatically to protect you from incorrect formats)

``` r
get_swap_format(parameters = "ALTW")
# [1] "float"

set_swap_format(parameter = "ALTW", value = 5)
# [1] "5.0"
```

More functionality will be implemented over time.

## Miscellaneous functions <a name="misc"></a>

The aforementioned functions rely on more basic general functions which, while are designed for internal use, can possibly also be of assistance to the end user. These are listed below.

``` r
# Load data
ob_dat <- load_observed(project_path)
mod_dat <- load_swap_output(project_path)

# Filters SWAP data (observed or modelled) by variable and depth
mod_filt <- filter_swap_data(data = mod_dat$custom_depth, var = "WC", depth = 15)
ob_filt <-  filter_swap_data(data = mod_dat$custom_depth, var = "WC", depth = 15)

# Filters and Matches dataframe structure of observed and modelled
data <- match_mod_obs(project_path, variable = "WC", depth = 15)

# Model performance metrics
NSE(obs = data$obs$WC_15, mod = data$mod$WC_15)
PBIAS(obs = data$obs$WC_15, mod = data$mod$WC_15)
RMSE(obs = data$obs$WC_15, mod = data$mod$WC_15)
RSR(obs = data$obs$WC_15, mod = data$mod$WC_15)

# Melts together all saved runs + current into tidy format
melt_all_runs(project_path, variable = "WC", depth = 15)
```

## Roadmap <a name="roadmap"></a>

### Major

-   Linux Support (0.4.0)
-   Sensitivity analysis (0.5.0)
-   Multi-core running (0.6.0)
-   Autocalibration / PEST integration (0.7.0)
-   Scenario runs (0.8.0)
-   SWAPtools plotting integration (0.9.0)
-   ...(1.0)

### Minor

-   Parsing support for all SWAP files, not just the main file.
-   Add support for multiple variables at differing depths for `autoset_output`
-   Update `plot_over_under()` to use [ggbraid](https://nsgrantham.github.io/ggbraid/)
-   Give all exported `rswap` functions a consistent naming scheme (`verb_swap_noun()`)
-   `plot_statistics()` sorting to follow stat property
-   Add "exact variable matching" and stop removing "RAIN" in `io.R` -\> `melt_all_runs()`
-   Renovate `soft_calibration_plot()` to accept any variable using the new system.
-   Add D-Statistic from Moriasi et al 2015.
-   Load observed data into package environment, to prevent the need for constant re-loading
-   Add `verbose` to `load_swap_output()`
-   Add support for reading saved runs with differing output.
-   `load_swap_observed()` to return just the dataframe, add a dedicated function for var names

## Support and Contributing <a name="support"></a>

If you run into any bugs or problems, please open an [issue](https://github.com/moritzshore/rswap/issues). The same goes for if you have any suggestions for improvement. If would you like to contribute to the project, let me know! Very open towards collaborative improvement. Fork/Branch off as you please :)

Any OPTAIN case-studies which use `rswap` are required to bake Moritz Shore a `cake` using a local recipe from the case-study country.

## Acknowledgements <a name="ack"></a>

This package was developed for the [OPTAIN](https://optain.eu) project and has received funding from the European Union's Horizon 2020 research and innovation program under grant agreement No. 862756.

## References <a name="ref"></a>

[1] Van Dam, J. Field-Scale Water Flow and Solute Transport: SWAP Model Concepts, Parameter Estimation, and Case Studies. Ph.D. Thesis, Wageningen University, Wageningen, The Netherlands, 2000. <a name="1"></a>

<p align="center">

<img src="man/figures/support_banner.png" width="70%" height="70%"/>

</p>
