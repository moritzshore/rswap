% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rw_parameters.R
\name{write_swap_tables}
\alias{write_swap_tables}
\title{Write SWAP Tables}
\usage{
write_swap_tables(project_path, tables, type = "main", verbose = F)
}
\arguments{
\item{project_path}{path to project directory}

\item{tables}{tables as loaded by \code{load_swap_tables()} (list of dataframes)}

\item{type}{supported swap parameter sets, current includes "main" (default) and "dra" (drainage)}

\item{verbose}{print status? (flag)}
}
\description{
Writes the SWAP table set into the rswap directory in .csv format
}
\seealso{
\code{\link[=load_swap_tables]{load_swap_tables()}} \code{\link[=write_swap_parameters]{write_swap_parameters()}} \code{\link[=write_swap_vectors]{write_swap_vectors()}}
}
