\name{put.neurocdf}
\alias{put.neurocdf}
\title{Add volume to neuro netCDF file}
\usage{
  \method{put}{neurocdf} (object, value, description, id,
    var = "ResultImage", start = c(1, 1, 1),
    count = c(-1, -1, -1), ...)
}
\arguments{
  \item{object}{neuro netCDF filename}

  \item{value}{Value to write}

  \item{description}{Description}

  \item{id}{Idition}

  \item{var}{Variable in netCDF file to alter}

  \item{start}{Start index}

  \item{count}{Count}

  \item{...}{Additional arguments to lower level functions}
}
\description{
  Add volume to neuro netCDF file
}
\author{
  Klaus K. Holst
}

