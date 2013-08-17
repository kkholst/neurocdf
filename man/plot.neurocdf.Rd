\name{plot.neurocdf}
\alias{plot.neurocdf}
\title{Plot neuro-netcdf data}
\usage{
  \method{plot}{neurocdf} (x, slice, mm, plane = 3,
    roi = NULL, col = gray.colors(255),
    roi.col = lava:::Col(rev(rainbow(15, start = 0, end = 0.69)), 0.5),
    roi.range, colorbar = TRUE, new = TRUE, overlay.img,
    overlay = NULL, overlay.var = "SubjectImage",
    overlay.col = rainbow(64, start = 0, end = 0.69),
    overlay.zlim, realworld = TRUE, upsample = FALSE, ...)
}
\arguments{
  \item{x}{Neuro netCDF filename}

  \item{slice}{Slice number (not needed if \code{mm} is
  given)}

  \item{mm}{Slice given in real world coordinates mm (MNI)}

  \item{plane}{Coordinate (valid values:
  1,2,3,x,y,z,median,sagittal,coronal,frontal,axial,transverse)}

  \item{roi}{Optional ROI from atlas to plot}

  \item{col}{Color of Template}

  \item{roi.col}{Color of ROI}

  \item{roi.range}{range of values in which to distribute
  roi.col}

  \item{colorbar}{if TRUE a horizontal colorbar (see
  \code{colorbar}) is added to the plot}

  \item{new}{Add new plot (default TRUE)}

  \item{overlay.img}{Image to overlay}

  \item{overlay}{Either a vector specifying the position of
  the overlay in the netCDF variable specified by
  \code{overlay.group}, or a matrix with columns of 1.
  coordinates and a column of 2. coordinates and optional a
  third column of values (defaults to 1)}

  \item{overlay.var}{netCDF variable from which to extract
  overlay}

  \item{overlay.col}{Color of overlay image}

  \item{overlay.zlim}{Range of z}

  \item{realworld}{Real-world coordinates}

  \item{upsample}{if TRUE image is upsampled (factor 2) and
  filtered to enhance resolution}

  \item{...}{Additional arguments to lower level functions}
}
\description{
  Plot elements from neuro-netcdf data
}
\author{
  Klaus K. Holst
}

