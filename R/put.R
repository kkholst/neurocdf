##' @export
put <- function(object,...) UseMethod("put")

##' Add volume to neuro netCDF file
##'
##' @title Add volume to neuro netCDF file
##' @param object neuro netCDF filename
##' @param value Value to write
##' @param description Description
##' @param id Idition
##' @param var Variable in netCDF file to alter 
##' @param start Start index
##' @param count Count
##' @param ... Additional arguments to lower level functions
##' @author Klaus K. Holst
##' @S3method put neurocdf
##' @method put neurocdf
put.neurocdf <- function(object,value,description,id,var="ResultImage",
                     start=c(1,1,1),count=c(-1,-1,-1),...) {
  dim <- dim.neurocdf(object)
  if (missing(id)) {
    if (var=="ResultImage") {
        id <- dim$result+1
        if (dim$result==1) {
            nc <- with(neuro.env, openNCDF)(object,write=FALSE)
            desc <- with(neuro.env, getvarNCDF)(nc,varid="ResultDescription")[1]
            with(neuro.env, closeNCDF)(nc)
            if (desc=="Empty") id <- 1
        } 
    } else stop("Id needed")
  }
  if (missing(value)) stop("Values needed")
  img <- mkNeuro(object,value,...)
  start <- c(start,id)
  count <- c(count,rep(1,length(id)))
  if (var=="ResultImage") {
    if (id<0 | id>dim$result+1) stop("Id out of range")
  } else {
    if (id<0 | id>tail(nc$var[[var]]$varsize,1)) stop("Idition out of range")
  }
  nc <- with(neuro.env, openNCDF)(object,write=TRUE)
  with(neuro.env, putvarNCDF)(nc,varid=var,vals=img,start=start,count=count)  
  if (var=="ResultImage") {
      if (missing(description)) description <- "No description"
      desc <- with(neuro.env, getvarNCDF)(nc,varid="ResultDescription")
      old <- desc[seq(length(description)),id]
      nadd <- nchar(old)-nchar(description); nadd[nadd<1] <- 0
      newd <- description
      for (i in seq(length(newd)))
          if (nadd[i]>0) newd[i] <- paste(newd[i],paste(rep(" ",nadd[i]),collapse=""),sep="")
      desc[seq(length(old)),id] <- newd
      with(neuro.env, putvarNCDF)(nc,varid="ResultDescription",vals=desc,
                 start=c(1,1,1),count=c(-1,-1,-1))
  }  
  invisible(with(neuro.env, closeNCDF)(nc))
}
