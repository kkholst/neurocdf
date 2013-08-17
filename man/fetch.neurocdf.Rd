\name{fetch.neurocdf}
\alias{fetch}
\alias{fetch.neurocdf}
\alias{fetchAtlas}
\alias{fetchGlobal}
\alias{fetchResult}
\alias{fetchSubject}
\alias{fetchTemplate}
\title{Extract slice or cube from neuro netCDF file}
\usage{
  \method{fetch}{neurocdf} (object, id = 1, type = 1,
    pos = c(type, id), var = "SubjectImage", slice = NULL,
    plane = 3, start, count, onlydesc = FALSE, ...)
}
\arguments{
  \item{object}{Filename of neuro netCDF file}

  \item{id}{Image number (of type 'type', e.g. individual)}

  \item{type}{Image type (only used with
  \code{var="SubjectImage"})}

  \item{pos}{Image number and type}

  \item{var}{Image variable in neuro netCDF file}

  \item{slice}{Slice in \code{plane}}

  \item{plane}{Number of string defining plane (optional)}

  \item{start}{Optional start position in neuro netCDF}

  \item{count}{Optional count in neuro netCDF}

  \item{onlydesc}{Only extract description}

  \item{...}{Arguments parsed on to lower level functions}
}
\description{
  Extract slice or cube from neuro netCDF file
}
\author{
  Klaus K. Holst
}

