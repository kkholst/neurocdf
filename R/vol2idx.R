
##' Convert volume to index
##'
##' Convert 3d-volume to matrix with 4 columns with x,y,z
##' coordinates and the actua
##' 
##' @param x Volume
##' @param ... Additional low-level arguments
##' @export
vol2idx <- function(x,...) {
  ii <- which(!is.na(x),arr.ind=TRUE)
  return(cbind(ii,x[ii]))
}
