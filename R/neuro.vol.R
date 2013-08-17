##' @S3method print neuro.vol
print.neuro.vol <- function(x,...) {
  if (!is.null(ii <- attributes(x)$info)) {
    for (i in ii)
      print(i,quote=FALSE)
  }
  cat("Dimension: ", dim(x), "\n\n")
  cat("Description:\n\t")
  cat(attributes(x)$description,sep="\n\t")
  cat("\n")
}

##' @S3method plot neuro.vol
plot.neuro.vol <- function(x,...) plot.neurocdf(x,...)
