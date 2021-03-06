% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write2disk.R
\name{write2disk}
\alias{write2disk}
\title{Writing data to disk}
\usage{
write2disk(x, folder, filename = NULL, up = 2)
}
\arguments{
\item{x}{object to save to disk.}

\item{folder}{name of the folder we want to write the files to.}

\item{filename}{name of the CSV and RDS files. Default value is NULL and, in
this case, the name of the file is the name of the object.}

\item{up}{integer, number of levels up the current working directy that
`folder` is. Default value is 2.}
}
\description{
This function writes data in CSV and RDS in a folder that will be `up` levels
up the current working directory.
}
\examples{
## Writting mtcars to disk in a subdirectory test of the current working
## directory:
write2disk(mtcars, "test", up = 0)
dir()

}
\author{
Marc Choisy
}
