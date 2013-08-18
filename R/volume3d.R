##' 3D plot of brain volumes
##'
##' @title 3D plot of brain volumes
##' @param rois Regions
##' @param rgl If TRUE rendered using RGL
##' @param fill Graphics options (contour3d)
##' @param col.mesh Graphics options (contour3d)
##' @param screen Graphics options (contour3d)
##' @param x x voxel-coordinates
##' @param y y voxel-coordinates
##' @param z z voxel-coordinates
##' @param color Graphics options (contour3d)
##' @param perspective Graphics options (contour3d)
##' @param level Levels to plot
##' @param alpha Transparency
##' @param sub Down-sample by factor 2
##' @param ... Additional graphics options
##' @author Klaus K. Holst
##' @export
volume3d <- function(rois=list(),rgl=TRUE,
                  fill=FALSE,col.mesh=0,screen=list(x=-60,y=65,z=20),
                  x=seq(91),y=seq(109),z=seq(91),color="red",
                  perspective=FALSE,level=1, alpha=0.5, sub=2, ...) {
  dots <- list(...)
  mask <- get(data(neuro.mask))
  x2 <- x; y2 <- y; z2 <- z
  if (length(x)>sub & sub>1) x2 <- x[seq(1,length(x), by = sub)]
  if (length(y)>sub & sub>1) y2 <- y[seq(1,length(y), by = sub)]
  if (length(z)>sub & sub>1) z2 <- z[seq(1,length(z), by = sub)]
  scene <- list()
  if (is.null(dots$add) || !dots$add) {
    V <- mask$vol
    V <- V[x2,y2,z2]
    A <- misc3d:::contour3d(V,x2,y2,z2, level=1, color="white", alpha=alpha,
                   smooth=5, draw=FALSE, material="dull",
                   col.mesh=0,fill=TRUE)
    scene <- list(A)
  }
  R0 <- array(dim=c(length(x),length(y),length(z))); R0[] <- 0
  if (!is.list(rois)) rois <- list(rois)
  for (i in seq_len(length(rois))) {
    R <- rois[[i]]
    if (is.matrix(R)) {
      R <- mkNeuro(R0,value=rois[[i]]);
    }      
    R[is.na(R)] <- 0
    R <- R[x,y,z,drop=FALSE];
    ## B <- misc3d:::contour3d(R,x,y,z, level=1, alpha=alpha, draw=FALSE, material="dull",col.mesh=col.mesh,fill=fill,smooth=1,...)
    B <- misc3d:::contour3d(R,x,y,z, level=1, alpha=alpha, smooth=1, draw=FALSE,color=color[i], ...)

    scene <- c(scene, list(B))
  }
  if (rgl) {
    misc3d:::drawScene.rgl(scene,...)
  } else {
    misc3d:::drawScene(scene,engine="standard")##,fill=fill,col.mesh=col.mesh,screen=screen,
##              perspective=perspective)
  }
  invisible(scene)
}
