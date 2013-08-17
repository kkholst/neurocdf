\name{coord.neurocdf}
\alias{coord}
\alias{coord.neurocdf}
\alias{vxmm}
\title{Convert from/to voxel coordinates (vx) to/from real-world
coordinates (mm)}
\usage{
  \method{coord}{neurocdf} (object, vx, mm, ...)
}
\arguments{
  \item{object}{Neuro netCDF filename}

  \item{vx}{Voxel coordinate (matrix with 3 columns)}

  \item{mm}{Real world coordinate (matrix with 3 columns)}

  \item{...}{Additional arguments parsed to lower level
  functions}
}
\description{
  Convert from/to voxel coordinates (vx) to/from real-world
  coordinates (mm)
}
\author{
  Klaus K. Holst
}

