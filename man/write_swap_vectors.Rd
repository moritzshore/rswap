% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rw_parameters.R
\name{write_swap_vectors}
\alias{write_swap_vectors}
\title{Write SWAP Vectors}
\usage{
write_swap_vectors(project_path, vectors, type = "main", verbose = F)
}
\arguments{
\item{project_path}{path to project directory}

\item{vectors}{vectors as loaded by \code{load_swap_vectors()} (list of dataframes)}

\item{type}{supported swap parameter sets, current includes "main" (default) and "dra" (drainage)}

\item{verbose}{print status? (flag)}
}
\description{
Writes the SWAP vector set into the rswap directory in .csv format
}
\seealso{
\code{\link[=load_swap_vectors]{load_swap_vectors()}} \code{\link[=write_swap_tables]{write_swap_tables()}} \code{\link[=write_swap_parameters]{write_swap_parameters()}}
}
