##' Copy hdr+img/nifti-files to appropiate directory structure for use with neurocdf function
##'
##' @title Restructure directories with image files
##' @param dir Path to process
##' @param newpath New path
##' @param checknum Check if number of files in each directory is equal to checknum (0 to disable)
##' @param omitpath Omit the following paths during processing
##' @param IdBeforeType If TRUE a directory structure: './subject-id/image-types/files'  is expected. Otherwise './image-types/subject-id/files' will be assumed.
##' @param ... Additional arguments to lower level functions
##' @author Klaus K. Holst
##' @export
procImageFiles <- function(dir,newpath,checknum=2,omitpath=c(newpath,"."),IdBeforeType=FALSE,...) {
    dirs <- setdiff(list.dirs(dir,recursive=FALSE,full.names=FALSE),omitpath)
    for (d in dirs) {
        d0 <- sub(paste(dir,"/",sep=""),"",d)
        iddirs <- list.dirs(d,recursive=FALSE)
        for (oldpath in iddirs) {
            ii <- sub(paste(d,"/",sep=""),"",oldpath)
            files <- list.files(oldpath,full.names=TRUE)
            if (checknum!=0)
                if (length(files)!=checknum) stop(checknum,
                              " file(s) expected: ", oldpath)
            message(oldpath)
            if (!IdBeforeType) {
                topath <- paste(newpath,ii,d0,sep="/")
            } else {
                topath <- paste(newpath,d0,ii,sep="/")
            }
            dir.create(topath,recursive=TRUE,showWarnings=TRUE)
            file.copy(files,topath)
        }
    }
    return(invisible(NULL))
}

##' Assuming that the files are on the prefered form
##' './subject-id/image-types/files' but where 'image-types' are for
##' some reason not matched across different subject-ids. This
##' function will then rename the 'image-types' for each 'subject-id'
##' to the 'newnames'. The image-type directories in each subject-id
##' is lexically sorted before the renaming procedure!
##' 
##' @title Rename image directories
##' @param dir Path to process
##' @param newnames New names of image-directories
##' @param ... Additional arguments to lower level functions
##' @author Klaus K. Holst
##' @export
renameImageFiles <- function(dir,newnames,...) {
    if (!file.exists(dir)) stop("path does not exist")
    dirs <- sort(list.dirs(path,full.names=TRUE,recursive=FALSE))
    dd <- lapply(dirs,
                 function(d) sort(list.files(d,recursive=FALSE,include.dirs=TRUE)))
    names(dd) <- dirs
    if (missing(newnames))
        stop("New names must be supplied")
    if (!all(unlist(lapply(dd,function(x) length(x)==length(newnames)))))
        stop("Subject directories with different number of image-directories than 'newnames'")
    renamed <- c()
    for (d in dirs) {
        for (i in 1:2) {
            rename <- paste(d,c(dd[[d]][i],newnames[i]),sep="/")
            renamed <- rbind(renamed,rename)
            file.rename(rename[1],rename[2])
        }
    }
    return(renamed)
}

