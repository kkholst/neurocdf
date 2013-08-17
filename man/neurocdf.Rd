\name{neurocdf}
\alias{neurocdf}
\title{Create or prepare for reading of netCDF file containing neuroimaging data}
\usage{
  neurocdf(file, path, template, atlas, extra, new = FALSE,
    flipneg = TRUE, compression = 9, ...)
}
\arguments{
  \item{file}{Filename of netCDF file}

  \item{path}{Path to directories containing image files
  (see details for the mandatory file structure). All
  directories within 'path' will be used. Alternatively,
  the argument can be given as a list of directories to be
  used.}

  \item{template}{Optional neuro template}

  \item{atlas}{Optional neuro atlas}

  \item{extra}{Additional volumes to include in the Global
  variable}

  \item{new}{If TRUE 'file' is overwrited if it already
  exists}

  \item{flipneg}{If TRUE image files are forced to be
  oriented}

  \item{compression}{Level of compression (1-9 or NA:=none)
  left-right, bottom-top, back-front.}

  \item{...}{Additional arguments to lower level functions}
}
\description{
  Create netCDF file containing neuroimaging data
}
\author{
  Klaus K. Holst
}

