\name{slices.default}
\alias{slices.default}
\title{Plot neuroimaging slices in three planes}
\usage{
  \method{slices}{default} (object, olay = NULL,
    rlim1 = c(-Inf, Inf), rlim2 = NULL,
    col1 = gray.colors(255),
    col2 = rev(rainbow(15, start = 0, end = 0.69)),
    center = 1, rev.col2 = FALSE, center.global = TRUE,
    main = "Three Planes View", scale = 0.8, alpha = 0.5,
    cross = TRUE,
    layout = c("clockwise", "counterclockwise"),
    origin = c(45, 63, 36), voxelsize = c(2, 2, 2), start,
    var = "GlobalImage", id = 1, digits = 4, ...)
}
\arguments{
  \item{object}{Volume}

  \item{olay}{Overlay (optional)}

  \item{rlim1}{Limits of voxel-values to plot of volume}

  \item{rlim2}{Limits of voxel-values to plot of overlay}

  \item{col1}{Color of volume voxels}

  \item{col2}{Color of overlay voxels}

  \item{center}{Choice of color scale. 0: color range from
  0 to maximum value; 1: color range minimum to maximum
  value; 2: color range symmetric: (-1;1)*(signed maximum
  value); or a vector with the min and max value.}

  \item{rev.col2}{Reverse col2}

  \item{center.global}{color ramge from slice or volume}

  \item{main}{Main label}

  \item{scale}{scale}

  \item{alpha}{transparency}

  \item{cross}{if TRUE a cross is plotted at active voxel}

  \item{layout}{Layout}

  \item{origin}{origin (conversion to real-world
  coordinates)}

  \item{voxelsize}{voxel-size (conversion to real-world
  coordinates)}

  \item{start}{Start at voxel}

  \item{var}{Variable in neuro netCDF file}

  \item{id}{Image number in 'Variable'}

  \item{digits}{number of digits to show in legend}

  \item{...}{Additional arguments to lower level arguments}
}
\description{
  Plot neuroimaging slices in three planes
}
\details{
  Wrapper of misc3d::slices3d
}

