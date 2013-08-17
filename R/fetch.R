##' @export
fetch <- function(object,...) UseMethod("fetch")

##' Extract slice or cube from neuro netCDF file
##'
##' @title Extract slice or cube from neuro netCDF file
##' @param object Filename of neuro netCDF file
##' @param id Image number (of type 'type', e.g. individual)
##' @param type Image type (only used with \code{var="SubjectImage"})
##' @param pos Image number and type 
##' @param var Image variable in neuro netCDF file
##' @param slice Slice in \code{plane}
##' @param plane Number of string defining plane (optional)
##' @param start Optional start position in neuro netCDF
##' @param count Optional count in neuro netCDF
##' @param onlydesc Only extract description
##' @param ... Arguments parsed on to lower level functions
##' @author Klaus K. Holst
##' @aliases fetch fetch.neurocdf fetchAtlas fetchGlobal fetchTemplate
##' fetchResult fetchSubject
##' @S3method fetch neurocdf
##' @method fetch neurocdf
fetch.neurocdf <- function(object,id=1,type=1,pos=c(type,id),var="SubjectImage",
                     slice=NULL,plane=3,start,count,onlydesc=FALSE,...) {
  if (!file.exists(object)) stop("netCDF file not found")
  nc <- with(neuro.env,openNCDF)(object)
  if (is.character(id)) {
    ids <- id.neurocdf(object)
    id <- which(ids%in%id)
  }
  if (var=="SubjectImage") {
    if (is.character(type)) {
      types <- type.neurocdf(object)
      type <- which(types%in%type)
    }
  }
  if (missing(start)) {
    start <- c(1,1,1,pos)
    count <- c(-1,-1,-1,1,1)
    if (!is.null(slice)) {
      plane <- planeval(plane)
      start[plane] <- slice; count[plane] <- 1
    }
    if (is.null(id) | id[1]==0) {
      start[5] <- 1; count[5] <- -1
      }
    if (is.null(type) | type[1]==0) {
      start[4] <- 1; count[4] <- -1
    }    
    if (var!="SubjectImage") {
      type <- NULL
      if (length(pos)>1)
        pos <- pos[2]
      start <- start[-4]
      count <- count[-4]
    }
  }
  if (missing(count)) stop("Count needed")
  vardesc <- gsub("Image","Description",var)
  desc <- as.vector(with(neuro.env, getvarNCDF)(nc,vardesc,start=c(1,1,pos),count=c(-1,-1,rep(1,length(pos)))))
  desc <- desc[desc!="" & desc!="NA"]
  idv <- paste("-", var,"  id=",id,sep="")
  if (!is.null(type)) idv <- paste(idv,"  type=",type,sep="");
  idv <- cbind(idv)
  colnames(idv) <- rownames(idv) <- ""
  if (onlydesc) {      
      with(neuro.env, closeNCDF)(nc)
      return(list(description=as.vector(desc),info=list(idv)))
  }
  img <- with(neuro.env, getvarNCDF)(nc,var,start=start,count=count)
  with(neuro.env, closeNCDF)(nc)
  res <- structure(img,description=as.vector(desc),info=list(idv))
  class(res) <- c("neuro.vol","array")
  return(res)
}


##' @export
fetchAtlas <- function(object,roi,id=2,var="GlobalImage",...) {
  rid <- id; if (var!="GlobalImage") rid <- NULL
  ROI <- neurocdf:::roi(object,id=rid)
  if (nrow(ROI)>0) {
    rownames(ROI) <- seq(nrow(ROI)); colnames(ROI) <- "ROI"
  }
  res <- structure(neurocdf:::fetch.neurocdf(object,var=var,id=id,...),ROI=ROI,info=list(ROI=ROI))  
  if (!missing(roi)) {
    roi <- na.omit(match(roi,ROI[,2]))
    if (length(roi)>0) {
      idx <- res%in%roi
      res[idx] <- 1
      res[!idx] <- NA
    }
  }
  return(res)
}


##' @export
fetchGlobal <- fetchAtlas

##' @export
fetchTemplate <- function(object,id=1,var="GlobalImage",...) fetch(object,var=var,id=id,...)


##' @export
fetchResult <- function(object,id=dim.neurocdf(object)$result,var="ResultImage",...) fetch(object,var=var,id=id,...)


##' @export
fetchSubject <- function(object,...) fetch(object,...)
