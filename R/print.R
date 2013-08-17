trim <- function (x, leading = TRUE, trailing = TRUE, space = "[:space:]") {
    if (leading) {
        pattern <- paste("^[", space, "]*", sep = "", collapse = "")
        x <- sub(pattern = pattern, replacement = "", x = x)
    }
    if (trailing) {
        pattern <- paste("[", space, "]*$", sep = "", collapse = "")
        x <- sub(pattern = pattern, replacement = "", x = x)
    }
    return(x)
}


##' @S3method print neurocdf
print.neurocdf <- function(x,...) {
    s <- summary(x,...)
    cat("\n",length(s$ids), " subjects; Show with 'id' method\n",sep="")
    cat(length(s$types), " volume(s) per subject\n",sep="")
    cat("\n")
    type <- cbind(s$types,s$desc[,,1]);
    rownames(type) <- rep("",nrow(type))
    colnames(type) <- c("Short name","Description")
    print(type,quote=FALSE)
    glob <- c()
    for (i in seq(s$global)) {
        desc <- fetchGlobal(x,id=i,onlydesc=TRUE)        
        mydesc <- trim(desc$description)
        if (length(mydesc)==0) mydesc <- ""
        glob <- rbind(glob,cbind(i,mydesc))
    }
    colnames(glob) <- c("GlobalImage","Description")
    rownames(glob) <- rep("",nrow(glob))
    res <- c()
    for (i in seq_len(s$result)) {
        desc <- fetchResult(x,id=i,onlydesc=TRUE)
        mydesc <- trim(desc$description)
        if (length(mydesc)==0) mydesc <- ""
        res <- rbind(res,cbind(i,mydesc))
    }
    colnames(res) <- c("ResultImage","Description")
    rownames(res) <- rep("",nrow(res))
    cat("\nGlobal images:\n")
    print(glob,quote=FALSE)
    cat("\nResult images:\n")
    print(res,quote=FALSE)
    cat("\n")
}
