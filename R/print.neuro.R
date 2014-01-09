##' @S3method print neuro 
print.neuro <- function(x,...) {
    if (!is.null(x$info)) cat(x$info,"\n")
    cat("Dimensions: ", paste(dim(x$vol),sep=" "),"\n")
}
