##' @S3method plot neuro
plot.neuro <- function(x,slice,mm,coord=1,slices,col=(gray.colors(255)),threshold,...) {
  allslices <- with(x,list(seq(dim(vol)[1]),seq(dim(vol)[2]),seq(dim(vol)[3])))
  xyz <- allslices
  for (i in 1:3) xyz[[i]] <- (xyz[[i]]-x$origin[i])*x$direction[i]*x$voxelsize[i]

  if (is.character(coord)) coord <- switch(tolower(coord),
                                           x=,
                                           median=,
                                           sagittal=1,
                                           y=,
                                           coronal=,
                                           frontal=2,
                                           z=,
                                           transverse=,
                                           axial=3
                                           )
  
  plotpos <- setdiff(1:3,coord)
  if (!missing(mm)) {
    slice <- approx(xyz[[coord]],allslices[[coord]],mm)$y    
  }
  if (missing(slice)) stop("Slice must be given")
  if (slice<1 | slice>length(allslices[[coord]])) stop("Slice not within valid range")
  plotslices <- allslices; plotslices[[coord]] <- slice
  xx <- xyz[[plotpos[1]]]; yy <- xyz[[plotpos[2]]]
  zz <- x$vol[plotslices[[1]],plotslices[[2]],plotslices[[3]]]
  if (!missing(threshold))  zz[zz<=threshold] <- NA
  
  dots <- list(...)
  if (is.null(dots$xlab)) dots$xlab <- ifelse(missing(mm),"","mm")
  if (is.null(dots$ylab)) dots$ylab <- ifelse(missing(mm),"","mm")

  args <- c(list(x=xx,y=yy,
                 z=zz,
                 useRaster=TRUE,
                 col=col),
            dots)
  do.call("image",args)
  invisible(args)
}
