\name{volume3d}
\alias{volume3d}
\title{3D plot of brain volumes}
\usage{
  volume3d(rois = list(), rgl = TRUE, fill = FALSE,
    col.mesh = 0, screen = list(x = -60, y = 65, z = 20),
    x = seq(91), y = seq(109), z = seq(91), color = "red",
    perspective = FALSE, level = 1, alpha = 0.5, sub = 2,
    ...)
}
\arguments{
  \item{rois}{Regions}

  \item{rgl}{If TRUE rendered using RGL}

  \item{fill}{Graphics options (contour3d)}

  \item{col.mesh}{Graphics options (contour3d)}

  \item{screen}{Graphics options (contour3d)}

  \item{x}{x voxel-coordinates}

  \item{y}{y voxel-coordinates}

  \item{z}{z voxel-coordinates}

  \item{color}{Graphics options (contour3d)}

  \item{perspective}{Graphics options (contour3d)}

  \item{level}{Levels to plot}

  \item{alpha}{Transparency}

  \item{sub}{Down-sample by factor 2}

  \item{...}{Additional graphics options}
}
\description{
  3D plot of brain volumes
}
\author{
  Klaus K. Holst
}

