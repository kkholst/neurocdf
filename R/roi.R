##' @export
roi <- function(x,id=NULL,...) {
    nc <- with(neuro.env, openNCDF)(x)
    ROI <- with(neuro.env, getvarNCDF)(nc,"AtlasDescription")
    with(neuro.env, closeNCDF)(nc)
  if (is.null(id)) {
    id <- seq_len(dim.neurocdf(x)$global)
  }
  res <- lapply(id,function(x) {
    res <- data.frame(num=as.numeric(ROI[,1,x]),
                      label=ROI[,2,x])
    return(res[!is.na(res[,1]),])
  })
  if (length(id)==1) res <- res[[1]]
  return(res)
}
