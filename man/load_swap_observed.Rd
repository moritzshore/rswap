% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/io.R
\name{load_swap_observed}
\alias{load_swap_observed}
\title{Load Observed Data}
\usage{
load_swap_observed(project_path, force = F, archived = F, verbose = F)
}
\arguments{
\item{project_path}{Path to project directory (string)}

\item{force}{Force reload of observed data? (flag)}

\item{archived}{set to true if project path in saved in 'rswap_saved' (flag)}

\item{verbose}{print status? (flag)}
}
\value{
returns a dataframe of your observed data
}
\description{
This function loads your observed data from the template file that was placed
in your project directory either by \code{build_rswap_directory()} or \code{rswap_init()}
}
\details{
It is critical that the template observed file is filled out correctly.
Please see the file itself for more information. It should be located in
your project directory, and must bear the name \code{swap_observed_data.csv}.
}
\examples{
# path to sample results (only for package internal example, you don't need
# this function)
example_path <- system.file(package = "rswap", "extdata/rswap_example_output")

# in your own projects, you would use something like this:
# example_path <- "C:/path/to/swap_folder/example_project"

load_swap_observed(example_path, verbose = TRUE)
}
