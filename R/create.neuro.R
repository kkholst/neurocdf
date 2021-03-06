##' Create new neuro object
##'
##' @title Create new neuro object
##' @param fileprefix File prefix of NIFTI or Analyze format file
##' @param info String to add to object
##' @param direction Prefered direction of x,y,z axis (default is
##' left-to-right, bottom-to-top, back-to-front)
##' @param flipneg Axes are flipped if \code{direction} is different
##' from direction as specified from the header (e.g. right-to-left is
##' transformed to left-to-right)
##' @param ... Additional arguments to lower level functions
##' @author Klaus K. Holst
##' @export
create.neuro <- function(fileprefix,info=NULL,direction=c(-1,1,1),flipneg=TRUE,...) {
  hdr <- vol <- txt <- NULL
  nii <- paste(fileprefix,"nii",sep=".")
  if (file.exists(nii)) {
    hdr <- vol <- nii
  } else {
    hdr <- paste(fileprefix,"hdr",sep=".")
    vol <- paste(fileprefix,"img",sep=".")    
    if (!file.exists(hdr)) {
      hdr <- vol <- fileprefix
      if (!file.exists(hdr))
        stop("Could not find NIFTI or Analyze format (lower-case .nii or .hdr file)")
    }
  }
  hdr <- path.expand(hdr)
  vol <- path.expand(vol)
  ## TODO: Rewrite to only depend on oro.nifti 
  my.hdr <- AnalyzeFMRI:::f.read.header(hdr)
  my.vol <- AnalyzeFMRI:::f.read.volume(vol)[,,,1]
  ## my.hdr <- oro.nifti:::readNIfTI(hdr)
  ## my.vol <- my.hdr@.Data
  ## my.hdr@.Data <- nifti()
  dim <- dim(my.vol)
  if (!is.null(my.hdr$srow.x)) {
  ## if (!is.null(my.hdr@srow_x)) {
      my.O <- with(my.hdr, cbind(srow.x,srow.y,srow.z))
      ## my.O <- cbind(my.hdr@srow_x,my.hdr@srow_y,my.hdr@srow_z)
      my.delta <- diag(my.O[1:3,1:3])
      my.o <- abs(my.O[4,]/my.delta)
  } else {
      my.delta <- my.hdr$pixdim[1:3+1]
      my.o <- my.hdr$originator[1:3]
      ## my.delta <- my.hdr@pixdim[1:3+1]
      ## my.o <- my.hdr@originator[1:3]
  }
  descrip <- my.hdr$descrip
  ## descrip <- my.hdr@descrip
  my.o <- my.o+1
  anyflip <- NULL
  allidx <- lapply(dim,function(x) seq(x))

  if (flipneg) {
    for (i in 1:3) {
      if (sign(my.delta[i])!=direction[i]) {
        anyflip <- c(anyflip,i)
        warning("Flipping axis ",i," of ", info)
        allidx[[i]] <- rev(allidx[[i]])
        old.o <- my.o[i]
        my.o[i] <- dim[i]+1-my.o[i]
        if (old.o!=my.o[i]) warning("Origo translated! ",i)
      }
    }
    my.delta <- abs(my.delta)*direction
    if (length(anyflip)>0) my.vol <- my.vol[allidx[[1]],allidx[[2]],allidx[[3]]]
  }
  res <- list(dim=dim,info=info,hdr=my.hdr,vol=my.vol,
              voxelsize=my.delta,origin=my.o,
              hdrdesc=descrip,direction=direction,flipped=anyflip,...)
  class(res) <- "neuro"
  return(res)
}

