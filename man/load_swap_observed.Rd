% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/io.R
\name{load_swap_observed}
\alias{load_swap_observed}
\title{Load Observed Data}
\usage{
load_swap_observed(project_path, archived = F, verbose = F)
}
\arguments{
\item{project_path}{Path to project directory (string)}

\item{archived}{set to true if project path in saved in 'rswap_saved' (flag)}

\item{verbose}{print status? (flag)}
}
\value{
Returns a list consiting of \code{.$data}, a dataframe of the observed values,
as well as \code{.$observed_variables}, a vector of the detected observed variables.
}
\description{
This function loads your observed data from the template file that was placed
in your project directory either by \code{build_rswap_directory()} or \code{rswap_init()}
}
\details{
It is critical that the template observed file is filled out correctly.
Please see the file itself for more information. It should be located in
your project directory, and must bear the namer \code{swap_observed_data.csv}.

Please note, the file type will eventually be switched to .csv.
}