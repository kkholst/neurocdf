##' @S3method print neuro 
print.neuro <- function(x,...) {
  print(x$info)
  print(dim(x$vol))
}
