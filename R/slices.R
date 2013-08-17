##' @export
slices <- function(object,...) UseMethod("slices")

##' Plot neuroimaging slices in three planes 
##'
##' Wrapper of misc3d::slices3d
##' @title Plot neuroimaging slices in three planes 
##' @param object Volume
##' @param olay Overlay (optional)
##' @param rlim1 Limits of voxel-values to plot of volume
##' @param rlim2 Limits of voxel-values to plot of overlay
##' @param col1 Color of volume voxels
##' @param col2 Color of overlay voxels
##' @param center Choice of color scale. 0: color range from 0 to maximum value;
##' 1: color range minimum to maximum value;
##' 2: color range symmetric: (-1;1)*(signed maximum value);
##' or a vector with the min and max value.
##' @param rev.col2 Reverse col2
##' @param center.global color ramge from slice or volume
##' @param main Main label
##' @param scale scale
##' @param alpha transparency
##' @param cross if TRUE a cross is plotted at active voxel
##' @param layout Layout
##' @param origin origin (conversion to real-world coordinates)
##' @param voxelsize voxel-size (conversion to real-world coordinates)
##' @param start Start at voxel
##' @param var Variable in neuro netCDF file 
##' @param id Image number in 'Variable'
##' @param digits number of digits to show in legend
##' @param ... Additional arguments to lower level arguments
##' @export
##' @S3method slices default
##' @method slices default
slices.default <- function (object, olay = NULL, rlim1 = c(-Inf, Inf),
    rlim2 = NULL, col1 = gray.colors(255), col2 =
    rev(rainbow(15,start=0,end=0.69)), center=1, rev.col2=FALSE, center.global=TRUE, main = "Three Planes View", scale =
    0.8, alpha = 0.5, cross = TRUE, layout = c(
    "clockwise","counterclockwise"), origin=c(45,63,36), voxelsize=c(2,2,2), start, 
    var="GlobalImage", id=1, digits=4, ...)  
{
    if (!require(tkrplot)) stop("tkrplot required")
    if (is.character(object)) {
        dd <- neurocdf:::dim.neurocdf(object)
        voxelsize <- dd$voxelsize
        origin <- dd$origin
        if (!is.null(olay) && length(dim(olay))<3) 
            olay <- mkNeuro(object,olay)
        object <- fetch(object,var=var,id=id,...)
    }
    if (!is.null(olay) & center.global) {
        mi.glob <- min(olay,na.rm=TRUE)
        ma.glob <- max(olay,na.rm=TRUE)
    }    
    if (rev.col2) col2 <- rev(col2)

  mkimg <- function(which) {
        switch(which,
               x = {
                 i <- 1
                 j <- 2
                 k <- 3
               }, y = {
                 i <- 2
                 j <- 1
                 k <- 3
               }, z = {
                 i <- 3
                 j <- 1
                 k <- 2
               })
        f <- function() {
            opar = par(mar = c(0, 0, 0, 0))
            on.exit(par(opar))
            if (!(is.array(col)))                
                image(misc3d:::vslice(vol, which, bb[i], bb[4]), col = col, 
                  zlim = rlim1)
            else {
                v <- switch(which, x = matrix(1:(d[2] * d[3]), 
                  nrow = d[2]), y = matrix(1:(d[1] * d[3]), nrow = d[1]), 
                  z = matrix(1:(d[1] * d[2]), nrow = d[1]))
                image(v, col = misc3d:::vslice(col, which, bb[i], bb[4]))
            }
            lines(rep(bb[j]/d[j], 100), seq(0, 1, len = 100))
            lines(seq(0, 1, len = 100), rep(bb[k]/d[k], 100))
        }
        tkrplot(tt, f, hscale = 0.8, vscale = 0.8)
    }
    mkscale <- function(i) {
        f <- function(...) {
            b <- as.numeric(tclvalue(bbv[[i]]))
            if (b != bb[i]) {
                bb[i] <<- b
                if (cross || i == 4) 
                  for (j in 1:3) tkrreplot(img[[j]])
                else tkrreplot(img[[i]])                
                tkrreplot(infobox)
                ##                tkconfigure(le, textvariable = bb[i])
            }
        }
        fr <- tkframe(tt)
        s <- tkscale(fr, command = f, from = 1, to = d[i], resolution = 1,
                     variable = bbv[[i]], showvalue = FALSE, orient = "horiz")
        le <- tkentry(fr,textvariable=bbv[[i]],width=4)
        l1 <- tklabel(fr, text = dn[i])
        l2 <- tkbutton(fr,text="Goto",command=f)
        tkgrid(l1, s, le, l2)
        fr
    }
    move <- function(which) {
        if (lay == "clockwise") {
            switch(which, x = {
                i <- 1
                j <- 2
                k <- 3
            }, y = {
                i <- 2
                j <- 1
                k <- 3
            }, z = {
                i <- 3
                j <- 1
                k <- 2
            })
        }
        else {
            switch(which, y = {
                i <- 1
                j <- 2
                k <- 3
            }, x = {
                i <- 2
                j <- 1
                k <- 3
            }, z = {
                i <- 3
                j <- 1
                k <- 2
            })
        }
        tkbind(img[[i]], "<Button-1>", function(x, y) {
            wid <- as.integer(tkwinfo("width", img[[i]]))
            hei <- as.integer(tkwinfo("height", img[[i]]))
            if (lay == "clockwise" || which == "z") 
                bb[j] <<- round(as.numeric(x)/wid * d[j])
            else bb[i] <<- round(as.numeric(x)/wid * d[i])
            bb[k] <<- d[k] - round(as.numeric(y)/hei * d[k])
            ##bb <- round(bb)
            for (j in 1:3) {
              tclvalue(bbv[[j]]) <<- as.character(round(bb[j]))
              tkrreplot(img[[j]])
            }
            tkrreplot(infobox)
        })
    }
    overlay <- function(object, olay, rlim1, rlim2, col1, col2, 
        alpha) {
        choose1 <- !is.na(object) & (object <= rlim1[2] & object >= rlim1[1])
        object <- floor((length(col1) - 0.01) * (object - min(object,na.rm=TRUE))/
                      (max(object,na.rm=TRUE) - min(object,na.rm=TRUE)) + 1)
        objectc <- col1[object]
        objectc[!choose1] <- "white"
        choose2 <- !is.na(olay) & (olay <= rlim2[2] & olay >= rlim2[1])

        if (center.global) {
            mi <- mi.glob; ma <- ma.glob
        } else {        
            mi <- min(olay[choose2]); ma <- max(olay[choose2])
        }

        m <- max(abs(c(mi,ma)))        
        if (center==0) {
            if (ma<0) colrg <- (olay+mi)/mi else colrg <- olay/ma
        }
        if (center==1) colrg <- (olay - mi)/(ma-mi)
        if (center==2) {
            colrg <- (olay + m)/(2*m)
        }

        olay <- floor((length(col2) - 0.01) * colrg + 1)        
        olayc <- col2[olay]
        olayc[!choose2] <- "transparent"
        alpha <- as.vector(ifelse(choose2, alpha, 0))
        col <- t(col2rgb(objectc))*(1-alpha) + t(col2rgb(olayc))*alpha
        array(rgb(col, maxColorValue = 255), dim = dim(object))
    }
    if (!require(tkrplot)) 
        stop("tkrplot is required.")
    if (missing(rlim1)) 
        rlim1 <- range(object, na.rm = TRUE)
    if (is.null(olay)) {
        vol <- object
        col <- col1
    }
    else {
        if (!all(dim(object == olay))) 
            stop("two layers have to have the same dimensions")
        if (missing(rlim2)) { 
          rlim2 <- range(olay, na.rm = TRUE)                 
        }
        choose2 <- !is.na(olay) & (olay <= rlim2[2] & olay >= rlim2[1])
        if (center.global) {
            mi <- mi.glob; ma <- ma.glob
        } else {
            mi <- min(olay[choose2]); ma <- max(olay[choose2])
        }
        attributes(rlim2)$min <- mi
        attributes(rlim2)$max <- ma
        m <- max(abs(c(mi,ma)))

        if (center==2) {            
          attributes(rlim2)$min <- -m
          attributes(rlim2)$max <- m
        }
        if (center==0) {
            if (ma>0) {
                attributes(rlim2)$min <- 0
                attributes(rlim2)$max <- ma
            } else {
                attributes(rlim2)$min <- mi
                attributes(rlim2)$max <- 0
            }
        }
        col <- overlay(object, olay, rlim1, rlim2, col1, col2, 
            alpha)
        vol <- array(0, dim = dim(object))
    }
    lay <- match.arg(layout)
    layout <- switch(lay, counterclockwise = c(2, 1, 3,4), clockwise = c(1, 
        2, 3 ,4))
    direct <- c("x", "y", "z")
    d <- dim(vol)
    dn <- c(direct, "t")
    tt <- tktoplevel()
    tktitle(tt) <- main
    if (missing(start)) {
      bb <- c(round(d[1:3]/2), 1)
    } else bb <- start
    bbv <- lapply(bb, tclVar)
    s <- lapply(layout[1:3], mkscale)
    suppressWarnings(img <- lapply(direct[layout[1:3]], mkimg))
    tkgrid(img[[1]], img[[2]])
    tkgrid(s[[1]], s[[2]])  
    info <- function() {
      op <- par(mar=c(0,0,0,0))
      on.exit(par(op))
      bb <- round(bb)
      mm <- vxmm(rbind(bb[1:3]))
      y1 <- object[rbind(bb[1:3])]
      y2 <- NA
      if (!is.null(olay))
        y2 <- olay[rbind(bb[1:3])]      
      plot(0,0,type="n",xlab="",ylab="",axes=FALSE)
      mytext <- paste("vx = (",paste(bb[1:3],collapse=","),")",sep="")
      text(0.1,0.7,mytext,pos=NULL)
      mytext <- paste("mm = (",paste(mm[1:3],collapse=","),")",sep="")
      text(0.1,0.5,mytext,pos=NULL)
      text(0.1,0.3,paste("Volume =", y1))
      if (!is.na(y2)) {
        text(0.1,0.1,paste("Overlay =", formatC(y2)))
        ##       mi <- min(olay,na.rm=TRUE); ma <- max(olay,na.rm=TRUE)
        ##        colrg <- (olay - mi)/(ma-mi)    
        nlut <- length(col2)
        delta <- 0.8
        scale <- (2*delta)/nlut
        M <- max(abs(rlim2))
        dM <- (attributes(rlim2)$max-attributes(rlim2)$min)/nlut
        for (i in seq_len(nlut+1)-1) {
          xx = -delta + (i-1)*scale
          if (i>0)
            rect(xx,-0.3,xx+scale,-0.4, col=col2[i], border=NA)
          rnd <- 10^digits
          rund <- round((attributes(rlim2)$min+dM*i)*rnd)/rnd
          text(xx+(1+0.5)*scale,-0.2,rund,cex=0.7,srt=45)
        }
      }
    }
  suppressWarnings(infobox <- tkrplot(tt,info,hscale=0.8,vscale=0.8))
  tkgrid(img[[3]],infobox)
  
  if (length(d) == 4 && d[4] > 1) 
    tkgrid(s[[3]], mkscale(4))
  else tkgrid(s[[3]])
  lapply(direct[layout[1:3]], move)
  invisible(environment())
}
