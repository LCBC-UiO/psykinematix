% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read-funcs.R
\name{read_lpkmx}
\alias{read_lpkmx}
\title{Read psykinematix log file}
\usage{
read_lpkmx(path, ...)
}
\arguments{
\item{path}{string. path to file.}

\item{...}{other arguments to \code{\link[base]{readLines}}}
}
\value{
string vector of the log file contents
}
\description{
Reads in the psykinematix log
file (lpkmx) as a string vector.
}
