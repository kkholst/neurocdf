\name{procImageFiles}
\alias{procImageFiles}
\title{Restructure directories with image files}
\usage{
  procImageFiles(dir, newpath, checknum = 2,
    omitpath = c(newpath, "."), IdBeforeType = FALSE, ...)
}
\arguments{
  \item{dir}{Path to process}

  \item{newpath}{New path}

  \item{checknum}{Check if number of files in each
  directory is equal to checknum (0 to disable)}

  \item{omitpath}{Omit the following paths during
  processing}

  \item{IdBeforeType}{If TRUE a directory structure:
  './subject-id/image-types/files' is expected. Otherwise
  './image-types/subject-id/files' will be assumed.}

  \item{...}{Additional arguments to lower level functions}
}
\description{
  Copy hdr+img/nifti-files to appropiate directory
  structure for use with neurocdf function
}
\author{
  Klaus K. Holst
}

