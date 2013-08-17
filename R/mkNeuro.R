##' Create brain volume or slice
##'
##' @title Create brain volume or slice
##' @param object neuro netCDF filename or array of voxels
##' @param value Image values (optional)
##' @param plane Plane to alter (optional)
##' @param slice Slice to alter (optional)
##' @param ... Additional arguments parsed on to lower level functions
##' @author Klaus K. Holst
##' @export
mkNeuro <- function(object,value,plane=NULL,slice,...) {
  if (is.character(object)) {
    object <- array(NA,dim=dim.neurocdf(object)$dim)
  }  
  dim <- dim(object)
  if (missing(value)) return(object)
  if (!is.array(value)) stop("Wrong type of 'value'")
  
  if (!missing(slice) & !is.null(plane)) { ## Insert a slice
    plane <- planeval(plane)
    xy <- setdiff(1:3,slice)
    if (all(dim(value)==dim(xy))) {
      ii <- lapply(dim,seq); ii[[plane]] <- slice
      object[ii[[1]],ii[[2]],ii[[3]]] <- value
      return(object)
    }
    if (ncol(value)==4 | ncol(value)==3) value <- value[value[,plane]==slice,]
  }
  if (length(dim(value))==3) {
    if (!all(dim(value)==dim(object))) stop("Wrong dimension of 'value'")
    idx <- !is.na(value)
    object[idx] <- value[idx]
    return(object)
  } 

  if (ncol(value)!=4 & ncol(value)!=3) stop("Wrong dimension of 'value'")
  if (ncol(value)==4) {
    object[value[,1:3]] <- value[,4]
  } 
  if (ncol(value)==3) {
    object[value] <- 1
  }
  if (length(dim(value))==3) {
    if (!all(dim(value)==dim(object))) stop("Wrong dimension of 'value'")
    idx <- !is.na(value)
    object[idx] <- value[idx]
  }
  return(object)
}
