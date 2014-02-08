##' Plot elements from neuro-netcdf data
##'
##' @title Plot neuro-netcdf data
##' @param x Neuro netCDF filename
##' @param slice Slice number (not needed if \code{mm} is given)
##' @param mm Slice given in real world coordinates mm (MNI)
##' @param plane Coordinate (valid values: 1,2,3,x,y,z,median,sagittal,coronal,frontal,axial,transverse)
##' @param roi Optional ROI from atlas to plot 
##' @param col Color of Template 
##' @param roi.col Color of ROI
##' @param roi.range range of values in which to distribute roi.col
##' @param colorbar if TRUE a horizontal colorbar (see \code{colorbar}) is added to the plot
##' @param new Add new plot (default TRUE)
##' @param overlay.img Image to overlay
##' @param overlay Either a vector specifying the position of the
##' overlay in the netCDF variable specified by \code{overlay.group},
##' or a matrix with columns of 1. coordinates and a column of
##' 2. coordinates and optional a third column of values (defaults to
##' 1)
##' @param overlay.var netCDF variable from which to extract overlay
##' @param overlay.col Color of overlay image
##' @param overlay.zlim Range of z
##' @param realworld Real-world coordinates
##' @param upsample if TRUE image is upsampled (factor 2) and filtered
##' to enhance resolution
##' @param ... Additional arguments to lower level functions
##' @author Klaus K. Holst
##' @S3method plot neurocdf
##' @export
##' @method plot neurocdf
plot.neurocdf <- function(x,slice,mm,plane=3,roi=NULL,col=gray.colors(255),
                      roi.col=lava:::Col(rev(rainbow(15,start=0,end=0.69)),0.5),
                      roi.range,
                      colorbar=TRUE,
                      new=TRUE, 
                      overlay.img,
                      overlay=NULL,overlay.var="SubjectImage",
                      overlay.col=rainbow(64,start=0,end=0.69),
                      overlay.zlim,
                      realworld=TRUE,upsample=FALSE,...) {
  if (is.character(x)) {
    if (!file.exists(x)) stop("netCDF file not found")
    dim <- dim.neurocdf(x)
    ddim <- dim$dim
    origin <- dim$origin
    vsize <- dim$voxelsize
  } else {
    if (!is.array(x) && length(dim(x))!=3) stop("Wrong type of image")
    ddim <- dim(x)
    origin <- ddim/2
  }
  allslices <- lapply(ddim,seq)  
  plane <- planeval(plane)    
  xyz <- allslices  
  if (realworld) xyz <- vxmm(xyz,origin=origin)
  
  plotpos <- setdiff(1:3,plane)
  n1 <- ddim[plotpos[1]];  n2 <- ddim[plotpos[2]]
  xx <- xyz[[plotpos[1]]]; yy <- xyz[[plotpos[2]]]
  if (!missing(mm)) {
    slice <- approx(xyz[[plane]],allslices[[plane]],mm)$y    
  }
  if (missing(slice)) stop("Slice must be given")
  if (slice<1 | slice>length(allslices[[plane]])) stop("Slice not within valid range")
  
  plotslices <- allslices; plotslices[[plane]] <- slice
  dots <- list(...)
  if (is.null(dots$xlab)) dots$xlab <- ifelse(missing(mm),"","mm")
  if (is.null(dots$ylab)) dots$ylab <- ifelse(missing(mm),"","mm")

  if (new) {
    if (is.character(x)) {
      template <- fetchTemplate(x,slice=slice,plane=plane)
    } else {
      template <- x[plotslices[[1]],plotslices[[2]],plotslices[[3]]]
    }
    args <- c(list(x=xx,y=yy,
                   z=template,
                   useRaster=TRUE,col=col),
              dots)
    if (upsample) {
      uimg <- upsamp(args$z,x=args$x,y=args$y)
      args$z <- uimg; args$x <- attributes(uimg)$x; args$y <- attributes(uimg)$y; 
    }
    do.call("image",args)   
  }
  
  if (!is.null(roi)) {
    if (!is.matrix(roi)) {
      sliceroi <- do.neurocdf(x,roi=roi,slice=slice,plane=plane,margin=0,...)$vx
      sliceroi <- sliceroi[,plotpos,drop=FALSE]
      if (length(sliceroi)>0) {
        roiimage <- matrix(NA,length(xx),length(yy))
        roiimage[sliceroi] <- 1
        if (upsample) {
          roiimage <- upsamp(roiimage,x=xx,y=yy)
          xx <- attributes(roiimage)$x
          yy <- attributes(roiimage)$y
        }
        image(xx,yy,z=roiimage,add=TRUE,useRaster=TRUE,xlab="",ylab="",
              col=roi.col,...)
      }       
    } else {
      img <- matrix(NA,n1,n2)
      if (ncol(roi)==2) {
        img[roi] <- 1
      } else  {
        if (ncol(roi)==4) {
          roi <- roi[roi[,plane]==slice,c(plotpos,4),drop=FALSE]
        } else {
          roi <- roi[roi[,plane]==slice,plotpos,drop=FALSE]
          if (nrow(roi)>0) roi <- cbind(roi,1)
        }
        if (nrow(roi)>0) {
          img[roi[,1:2,drop=FALSE]] <- roi[,3]            
        }
      }
      if (nrow(roi)>0) {
        if (upsample) {
          roiimage <- upsamp(img,x=xx,y=yy)
          xx <- attributes(img)$x
          yy <- attributes(img)$y
        }
        mi <- min(img,na.rm=TRUE); ma <- max(img,na.rm=TRUE)
        M0 <- M <- max(abs(c(mi,ma)))        
        if (!missing(roi.range)) M0 <- roi.range
        colrg <- ((img + M)/(2*M)-0.5)*(M/M0) + 0.5
        vol2 <- floor((length(roi.col) - 0.01) * colrg + 1)
        roi.col2 <- roi.col[sort(unique(vol2idx(vol2)[,3]))]
        image(xx,yy,vol2,add=TRUE,useRaster=TRUE,xlab="",ylab="",col=roi.col2,...)
        if (colorbar) {
            colorbar(roi.col,direction="horizontal",y.range=min(yy)+c(0,3),x.range=range(xx),values=c(-M0,M0),label.offset=2,srt=0)
        }
      }
    }
  }

   
  if (!is.null(overlay)) {    
    img <- fetch(x,var=overlay.var,pos=overlay,plane=plane,slice=slice)
    if (nrow(img)>0) {
      if (upsample) {
        img <- upsamp(img,x=xx,y=yy)
        xx <- attributes(img)$x
        yy <- attributes(img)$y
      }
      image(xx,yy,img,add=TRUE,useRaster=TRUE,xlab="",ylab="",col=overlay.col,...)
    }
  }
    
  if (!missing(overlay.img)) {
    if (length(dim(overlay.img))==3) {
      if (!all(dim(overlay.img)==dim$dim))
        stop("Wrong dimension of overlay image")
      overlay.img <- overlay.img[plotslices[[1]],plotslices[[2]],plotslices[[3]]]
    }
    if (!all(dim(overlay.img)==c(length(xx),length(yy))))
      stop("Wrong dimension of overlay image")
    if (upsample) {
      overlay.img <- upsamp(overlay.img,x=xx,y=yy)
      xx <- attributes(overlay.img)$x
      yy <- attributes(overlay.img)$y
    }

    mi <- min(overlay.img,na.rm=TRUE); ma <- max(overlay.img,na.rm=TRUE)
    mm <- max(abs(mi),abs(ma))
    col <- floor((length(overlay.col)-0.01)*(overlay.img+mm)/(2*mm)+1)
    ##    mycol <- t(col2rgb(overlay.img[col]))
    mycol <- t(col2rgb(col)) #[overlay.img]))
    mycol[is.na(mycol)] <- "transparent"
    mycol <- array(rgb(mycol, maxColorValue = 255), dim = dim(overlay.img))

    image(xx,yy,overlay.img,add=TRUE,useRaster=TRUE,xlab="",ylab="",col=mycol,...)  
  }
  
}

##' @S3method plot array
plot.array <- function(x,...) plot.neurocdf(x,...)
