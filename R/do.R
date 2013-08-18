##' @export
do <- function(object,...) UseMethod("do")

##' Aggregate values in ROI
##'
##' A kxn matrix is extracted where n is the number of voxels in the
##' region, and k is the number of subjects/images. Optionally a
##' function can be applied over columns of rows of this matrix.
##' @title Aggregate values in ROI
##' @param object Filename of (neuro-)netcdf file
##' @param roi Region of interest. Either a string or number defining
##' the region in the atlas of 'x', or a nx3 matrix with voxel
##' positions defining the region. If an attribute \code{type="mm"} is
##' set, the voxel positions are treated as real world coordinates,
##' and in the other case as voxel indices (vx).
##' @param fun Optional function to apply. If missing the actual image
##' values in each voxel is returned.
##' @param type The image type number (if 0 all images will be used)
##' @param margin Function applied row-wise (1) or column-wise (2). If
##' 0 only the ROI coordinates are returned.
##' @param na.rm Should NA values be removed (complete-cases)
##' @param na.rm.margin Complete-cases defined from columns (2) or rows (1)
##' @param slice Slice number
##' @param plane Coordinate (row 1, column 2, slice 3)
##' @param x row
##' @param y column
##' @param z slice
##' @param realworld If TRUE real world coordinates are returned as
##' the element \code{mm}.
##' @param atlas Position of atlas among 'var' images
##' @param var The variable in the neuro netCDF file from which
##' the atlas is chosen.
##' @param mc.cores Number of cores to be used in parallel computing 
##' @param chunksize Chunk size (parallel computing)
##' @param exclude Index of subjects to exclude
##' @param ... Additional arguments parsed to lower level functions
##' @author Klaus K. Holst
##' @return A list with voxel coordinates (vx), real world coordinates
##' (mm) if \code{realworld=TRUE}, and aggregated image values
##' \code{val}.
##' @aliases do do.neurocdf
##' @S3method do neurocdf
##' @method do neurocdf
do.neurocdf <- function(object,roi,fun,type=1,margin=2,na.rm=FALSE,na.rm.margin=2,slice=NULL,plane=3,x,y,z,realworld=FALSE,atlas=2,var="GlobalImage",mc.cores=4,chunksize=1000/mc.cores,exclude,...) {
  if (!file.exists(object)) stop("netCDF file not found")
  if (is.character(type)) {
    types <- type.neurocdf(object)
    type <- which(types%in%type)
  }
  plane <- planeval(plane)
  plotpos <- setdiff(1:3,plane)
  atlasvol <- fetchAtlas(object,id=atlas,var=var,slice=slice,plane=plane)  
  ROI <- attributes(atlasvol)$ROI  
  if (missing(roi)) { ## Process everything
      if (missing(fun)) { ## Then just show ROIs embedded in neurocdf-file
          return(ROI) 
      }
      typecount <- 1; 
      if (type==0) {
          type <- 1; typecount <- length(neurocdf:::type.neurocdf(object))
      }
      dummy <- rep(seq(dim(object)$nid),each=typecount)
      if (typecount>1) dummy <- matrix(dummy,ncol=typecount)
      fdummy <- fun(dummy)
      dim.fun <- length(fdummy)
      nc <- with(neurocdf:::neuro.env, openNCDF)(object)
      dim.vol <- dim(object)$dim
      ## ProcZ <- function(i,...) {
      ##     message(".",appendLF = FALSE)         
      ##     ##          val <- with(neurocdf:::neuro.env, getvarNCDF)(nc,"SubjectImage",c(1,1,i,type,1),c(dim.vol[1:2],1,1,-1))          
      ##     nc <- with(neurocdf:::neuro.env, openNCDF)(object)
      ##     val <- with(neurocdf:::neuro.env,
      ##                 getvarNCDF)(nc,"SubjectImage",
      ##                             c(1,1,i,type,1),
      ##                             c(-1,-1,1,typecount,-1))
      ##     with(neurocdf:::neuro.env,closeNCDF)(nc)          
      ##     dval <- dim(val)
      ##     val <- array(aperm(val,c(3,1,2)),dim=c(dval[3],prod(dval[1:2])))
      ##     suppressWarnings(fval <- apply(val,margin,
      ##                                    function(x)
      ##                                    tryCatch(fun(x),
      ##                                             error=function(...)
      ##                                             rep(NA,dim.fun))))
      ##     if (dim.fun>1) fval <- t(fval) else fval <- cbind(fval)
      ##     return(fval)
      ## }

      mychunks <- split(seq(prod(dim.vol[1:2])),
                        ceiling(seq(prod(dim.vol[1:2]))/chunksize))
      pb <- txtProgressBar(style=3,width=40)
      val <- c()      
      for (i in seq(dim.vol[3])) {
          prog <- i/dim.vol[3]
          if (prog>getTxtProgressBar(pb))
              setTxtProgressBar(pb,prog)
          slice <- with(neurocdf:::neuro.env,
                        getvarNCDF)(nc,"SubjectImage",
                                    c(1,1,i,type,1),
                                    c(-1,-1,1,typecount,-1))
          dslice <- dim(slice) 
          if (typecount==1) {
              dslice <- c(dslice[1:2],1,dslice[3])
              slice <- array(slice,dim=dslice)              
          }
          if (!missing(exclude)) 
                  slice <- slice[,,,-exclude]

          slice <- array(aperm(slice,c(4,3,1,2)),
                         dim=c(dslice[4],dslice[3],prod(dslice[1:2])))
          ## slice <- array(aperm(slice,c(3,1,2)),
          ##                dim=c(dslice[3],prod(dslice[1:2])))
          procz <- function(jj) {
              t(apply(slice[,,jj,drop=FALSE],3,function(x)
                    tryCatch(fun(x),
                             error=function(...)
                             rep(NA,dim.fun))))
          }
          ## system.time(suppressWarnings(
          ##     val <- c(val,foreach(j=mychunks,.combine="rbind") %dopar% procz(j))))
          suppressWarnings(
              val <- c(val,parallel:::mclapply(mychunks,procz,mc.cores=mc.cores)))
          ## benchmark(foreach(j=mychunks,.combine="rbind") %dopar% procz(j),
          ##           mclapply(mychunks,procz,mc.cores=8))
      }
      close(pb)
      ## val <- mclapply(seq(91),ProcZ,mc.cores=1)
      if (dim.fun==1) {
          val <- lapply(val,t)
      }
      val1 <- do.call("rbind",val)
      ## val <- matrix(unlist(val),byrow=TRUE,ncol=ncol(val[[1]]))      
      val1 <- array(val1,dim=c(dim.vol,ncol(val1)))
      with(neurocdf:::neuro.env,closeNCDF)(nc)
      return(val1)
  }
  
  if (is.character(roi)) {
      browser()
      roi <- ROI[na.omit(match(roi,ROI[,2])),1]
      if (length(roi)==0) {
        return(ROI)
      }
      roi <- which(atlasvol==roi,arr.ind=TRUE)
  }
  if (is.matrix(roi)) {
    sliceroi <- roi
    if (ncol(sliceroi)<3) stop("nx3 matrix of x,y,z coordinates expected")
    if (ncol(sliceroi)>3) sliceroi <- sliceroi[,1:3]
  } else {
    sliceroi <- do.call("rbind",lapply(roi,function(x) which(atlasvol==x,arr.ind=TRUE)))
    if (length(sliceroi)>0)
    if (!missing(slice)) {
      sliceroi <- (cbind(sliceroi,slice))[,order(c(plotpos,plane)),drop=FALSE]
    }
  }
  if (length(sliceroi)==0) return(list(roi=NULL,value=NULL))  
  res <- list(vx=sliceroi)
  if (realworld) res <- c(res,list(mm=coord.neurocdf(object,vx=res[[1]])))
  if (margin==0) return(res)

  nc <- with(neurocdf:::neuro.env, openNCDF)(object)
  val <- rbind(apply(sliceroi,1,function(x) with(neurocdf:::neuro.env, getvarNCDF)(nc,"SubjectImage",c(x,type,1),c(1,1,1,1,-1))))
  with(neurocdf:::neuro.env,closeNCDF)(nc)
  if (na.rm) {
    nas <- apply(val,na.rm.margin,function(x) any(is.na(x)))
    idx <- which(!nas)
    if (na.rm.margin==1) {
      val <- val[idx,,drop=FALSE]
    }  else {
      res$vx <- res$vx[idx,,drop=TRUE]
      if(!is.null(res$mm)) res$mm <- res$mm[idx,,drop=TRUE]
      val <- val[,idx,drop=FALSE]
    }
  }

  if (!missing(fun)) {
    if (is.null(mc.cores) || mc.cores<1) {
      val <- apply(val,margin,fun)
    } else {
      if (margin==2) val <- t(val)
      i <- 0 ## Stupid check-parser
      suppressWarnings(val <- parallel:::mclapply(seq(nrow(val)), function(i) tryCatch(fun(val[i,]),error=function(...) NA)))
      X1 <- val[[1]]
      K <- length(X1)
      if (is.vector(X1)) {
        if (all(unlist(lapply(val,length))==K)) {
          if (K==1) val <- unlist(val)
          else {
            val <- matrix(unlist(val),byrow=TRUE,ncol=K)
            colnames(val) <- names(X1)
          }
        }
      }
    }
  }
  res <- c(res, list(value=val))
  return(res)
}



