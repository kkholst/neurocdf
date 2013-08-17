\name{create.neuro}
\alias{create.neuro}
\title{Create new neuro object}
\usage{
  create.neuro(fileprefix, info = NULL,
    direction = c(1, 1, 1), flipneg = TRUE, ...)
}
\arguments{
  \item{fileprefix}{File prefix of NIFTI or Analyze format
  file}

  \item{info}{String to add to object}

  \item{direction}{Prefered direction of x,y,z axis
  (default is left-to-right, bottom-to-top, back-to-front)}

  \item{flipneg}{Axes are flipped if \code{direction} is
  different from direction as specified from the header
  (e.g. right-to-left is transformed to left-to-right)}

  \item{...}{Additional arguments to lower level functions}
}
\description{
  Create new neuro object
}
\author{
  Klaus K. Holst
}

