##' Extract dimensions of neuro netCDF file
##'
##' @title Extract dimensions of neuro netCDF file
##' @param x neuro netCDF filename
##' @param ... Additional arguments parsed to lower level functions
##' @author Klaus K. Holst
##' @S3method dim neurocdf
##' @method dim neurocdf
dim.neurocdf <- function(x,...) {
  if (!file.exists(x)) stop("netCDF file not found")
  nc <- with(neuro.env, openNCDF)(x)
  Dim <- nc$var$SubjectImage$varsize
  origin <- as.vector(with(neuro.env, getvarNCDF)(nc,"Origin"))
  voxelsize <- as.vector(with(neuro.env, getvarNCDF)(nc,"VoxelSize"))
  ## numImages <- with(nc$dim, numImages$len)
  ## numTypes <- with(nc$dim, numTypes$len)
  ## numGlobal <- with(nc$dim, numImages$len)
  numGlobal <- nc$dim$nGlobal$len
  ##  numResult <- ifelse(length(nc$dim$nResult$vals)==1,0,nc$dim$nResult$len)
  numResult <- nc$dim$nResult$len
  res <- (list(dim=Dim[1:3], ntype=Dim[4], nid=Dim[5], Dim=Dim, origin=origin, voxelsize=voxelsize, global=numGlobal, result=numResult
               ))
  with(neuro.env, closeNCDF)(nc)
  return(res)  
}
