##' @export
coord <- function(object,...) UseMethod("coord")

##' Convert from/to voxel coordinates (vx) to/from real-world
##' coordinates (mm)
##'
##' @title Convert from/to voxel coordinates (vx) to/from real-world
##' coordinates (mm)
##' @aliases vxmm 
##' @param object Neuro netCDF filename
##' @param vx Voxel coordinate (matrix with 3 columns)
##' @param mm Real world coordinate (matrix with 3 columns)
##' @param ... Additional arguments parsed to lower level functions
##' @author Klaus K. Holst
##' @aliases coord coord.neurocdf
##' @S3method coord neurocdf
##' @method coord neurocdf
coord.neurocdf <- function(object,vx,mm,...) {  
  if (missing(object)) {
    vxmm(vx,mm,...)
  }
  dim <- dim.neurocdf(object)
  if (missing(vx) & missing(mm)) return(dim)
  vxmm(vx,mm,origin=dim$origin,voxelsize=dim$voxelsize,...)
}

##' @export
vxmm <- function(vx,mm,origin=c(45,63,36),voxelsize=c(2,2,2),...) {
  if (!missing(vx)) {
    if (is.list(vx)) {
      for (i in 1:3) vx[[i]] <- (vx[[i]]-1-origin[i])*voxelsize[i]
      return(structure(vx,type="mm"))
    }
    if (NCOL(vx)!=3) stop("Matrix with 3 columns expected")
    return(structure(t(apply(vx,1,function(x) (x-1-origin)*voxelsize)),type="mm"))
  }
  if (!missing(mm)) {
    if (is.list(mm)) {
      for (i in 1:3) mm[[i]] <- mm[[i]]/voxelsize[i]+origin[i]
      return(structure(mm,type="vx"))
    }
    if (NCOL(mm)!=3) stop("Matrix with 3 columns expected")
    return(structure(t(apply(mm,1,function(x) x/voxelsize+origin)),type="vx"))
  }
}
