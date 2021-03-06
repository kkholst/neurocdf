##' @export
type <- function(object,...) UseMethod("type")

##' Extract different 'types' of neuro netCDF files
##'
##' @title Extract different 'types' of neuro netCDF files
##' @param object netCDF filename
##' @param ... Additional low level argument parsed on to lower level functions
##' @author Klaus K. Holst
##' @aliases type type.neurocdf
##' @S3method type neurocdf
##' @method type neurocdf
type.neurocdf <- function(object,...) {
  if (!file.exists(object)) stop("netCDF file not found")
  nc <- with(neuro.env, openNCDF)(object)
  types <- as.vector(with(neuro.env, getvarNCDF)(nc,"SubjectDescription",c(1,2,1,1),c(-1,1,-1,1)))
  with(neuro.env, closeNCDF)(nc)
  return(types)
}
