\name{do.neurocdf}
\alias{do}
\alias{do.neurocdf}
\title{Aggregate values in ROI}
\usage{
  \method{do}{neurocdf} (object, roi, fun, type = 1,
    margin = 2, na.rm = TRUE, na.rm.margin = 2,
    slice = NULL, plane = 3, x, y, z, realworld = FALSE,
    atlas = 2, var = "GlobalImage", mc.cores = 4,
    chunksize = 1000/mc.cores, exclude, ...)
}
\arguments{
  \item{object}{Filename of (neuro-)netcdf file}

  \item{roi}{Region of interest. Either a string or number
  defining the region in the atlas of 'x', or a nx3 matrix
  with voxel positions defining the region. If an attribute
  \code{type="mm"} is set, the voxel positions are treated
  as real world coordinates, and in the other case as voxel
  indices (vx).}

  \item{fun}{Optional function to apply. If missing the
  actual image values in each voxel is returned.}

  \item{type}{The image type number (if 0 all images will
  be used)}

  \item{margin}{Function applied row-wise (1) or
  column-wise (2). If 0 only the ROI coordinates are
  returned.}

  \item{na.rm}{Should NA values be removed
  (complete-cases)}

  \item{na.rm.margin}{Complete-cases defined from columns
  (2) or rows (1)}

  \item{slice}{Slice number}

  \item{plane}{Coordinate (row 1, column 2, slice 3)}

  \item{x}{row}

  \item{y}{column}

  \item{z}{slice}

  \item{realworld}{If TRUE real world coordinates are
  returned as the element \code{mm}.}

  \item{atlas}{Position of atlas among 'var' images}

  \item{var}{The variable in the neuro netCDF file from
  which the atlas is chosen.}

  \item{mc.cores}{Number of cores to be used in parallel
  computing}

  \item{chunksize}{Chunk size (parallel computing)}

  \item{exclude}{Index of subjects to exclude}

  \item{...}{Additional arguments parsed to lower level
  functions}
}
\value{
  A list with voxel coordinates (vx), real world
  coordinates (mm) if \code{realworld=TRUE}, and aggregated
  image values \code{val}.
}
\description{
  Aggregate values in ROI
}
\details{
  A kxn matrix is extracted where n is the number of voxels
  in the region, and k is the number of subjects/images.
  Optionally a function can be applied over columns of rows
  of this matrix.
}
\author{
  Klaus K. Holst
}

