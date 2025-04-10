% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rw_parameters.R
\name{write_swap_file}
\alias{write_swap_file}
\title{Write SWAP File}
\usage{
write_swap_file(project_path, outfile, format = F, verbose = F)
}
\arguments{
\item{project_path}{path to project directory}

\item{outfile}{name of the SWAP file to write. will be stored in project directory (string)}

\item{format}{(logical) should the parameters stored in the rswap directory be converted to Fortran format before being written? (SLOWER)}

\item{verbose}{print status? (flag)}
}
\value{
Returns path of written file
}
\description{
Writes a SWAP main file from the parameters, vectors, and tables stored in
the rswap directory. \strong{Before you use this function, you need to have parsed a SWAP file}
}
\details{
This function currently is only intended for the SWAP main file and drainage file. More may be added over time.
}
\seealso{
\code{\link[=modify_swap_file]{modify_swap_file()}}
}
