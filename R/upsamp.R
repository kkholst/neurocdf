##' @export
upsamp <- function(A,x,y,d=2,theta=0.28,...) {
  B <- array(NA,dim=dim(A)*d)
  for (i in (1:ncol(A)-1)) {
    col <- rep(NA,nrow(B))
    col[seq(nrow(A))*d] <- A[,i+1]
    B[,(d*i)+1] <- col
  }
  res <- fields:::image.smooth(B,theta=theta,...)$z
  if (!missing(x)) {    
    attributes(res)$x <- seq(min(x),max(x),length.out=nrow(res))
  }  
  if (!missing(y)) {
    attributes(res)$y <- seq(min(y),max(y),length.out=ncol(res))
  }
  return(res)  
}

