##' Summary of neuro netCDF file
##'
##' @title Summary of neuro netCDF file
##' @param object neuro netCDF filename
##' @param ... Additional arguments parsed to lower level functions
##' @author Klaus K. Holst
##' @S3method summary neurocdf
##' @method summary neurocdf
summary.neurocdf <- function(object,...) {
  if (!file.exists(object)) stop("netCDF file not found")
  nc <- with(neurocdf:::neuro.env, openNCDF)(object)
  ids <- as.vector(with(neurocdf:::neuro.env, getvarNCDF)(nc,"SubjectDescription",
                              start=c(1,1,1,1),count=c(-1,1,1,-1)))
  types <- as.vector(with(neurocdf:::neuro.env, getvarNCDF)(nc,"SubjectDescription",
                                start=c(1,2,1,1),count=c(-1,1,-1,1)))
  descs <- with(neurocdf:::neuro.env, getvarNCDF)(nc,"SubjectDescription",
                      start=c(1,3,1,1),count=c(-1,1,-1,-1))
  ROI <- with(neurocdf:::neuro.env, getvarNCDF)(nc,"AtlasDescription")
  with(neurocdf:::neuro.env, closeNCDF)(nc)
  dims <- dim(object)
  res <- c(list(types=types,ids=ids,descs=descs, ROI=ROI), dims)
  return(res)
}
