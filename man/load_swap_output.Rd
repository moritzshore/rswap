% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/io.R
\name{load_swap_output}
\alias{load_swap_output}
\title{Reads SWAP Model Output}
\usage{
load_swap_output(project_path, archived = F, verbose = F)
}
\arguments{
\item{project_path}{path to project directory (string)}

\item{archived}{needs to be set to true when reading from the rswap_saved archive (flag)}

\item{verbose}{print status? (flag)}
}
\value{
Returns a list of dataframes. \code{.$daily_output} and \code{.$custom_depth}
}
\description{
This function reads the output of the SWAP model for the last run of the
given \code{project_path}. It can also be used to read the results of saved runs
in the "/rswap_saved" folder if \code{archived} is set to \code{TRUE}.
}
\details{
Currently, the function only returns the data needed by the rest of the
package, but this will be expanded over time to return all SWAP output
}
\examples{
# path to sample results (only for package internal example, you don't need
# this function)
example_path <- system.file(package = "rswap", "extdata/rswap_example_output")

# in your own projects, you would use something like this:
# example_path <- "C:/path/to/swap_folder/example_project"

load_swap_output(example_path)
}
