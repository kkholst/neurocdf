##' Create netCDF file containing neuroimaging data
##'
##' @title Create or prepare for reading of netCDF file containing neuroimaging data
##' @param file Filename of netCDF file
##' @param path Path to directories containing image files (see
##' details for the mandatory file structure). All directories within
##' 'path' will be used. Alternatively, the argument can be given as a
##' list of directories to be used.
##' @param template Optional neuro template
##' @param atlas Optional neuro atlas
##' @param extra Additional volumes to include in the Global variable
##' @param new If TRUE 'file' is overwrited if it already exists
##' @param flipneg If TRUE image files are forced to be oriented
##' @param compression Level of compression (1-9 or NA:=none)
##' left-right, bottom-top, back-front.
##' @param ... Additional arguments to lower level functions
##' @author Klaus K. Holst
##' @export
neurocdf <- function(file,path,template,atlas,extra,new=FALSE,flipneg=TRUE,compression=9,...) {
    if (file.exists(file) & !new) {
        class(file) <- "neurocdf"
        return(file)        
    }
    if (missing(path)) stop("netCDF file not found or 'path' argument needed")
  
    if (is.null(path)) {
        nid <- ntype <- 1
    }  else {       
        if (length(path)==1 & !is.list(path)) {
            if (!file.exists(path)) stop("path does not exist")
            dirs <- sort(list.dirs(path,full.names=TRUE,recursive=FALSE))
        } else {
##            browser()
            dirs <- unlist(path)
            if (!all(unlist(lapply(dirs,file.exists)))) stop("Directory does not exists")
        }
        d <- dirs[1]
        
        allsets <- list.files(dirs,recursive=FALSE,include.dirs=TRUE) 
        R <- length(allsets)
        allsets <- sort(unique(allsets))
        ##sort(list.dirs(d,full.names=TRUE,recursive=FALSE))  
        ntype <- length(allsets)
        
        nid <- length(dirs)
        if (nid==0) stop("No subject directories found")
        if (ntype==0) stop("No image directories found")
    }
    
    if (file.exists(file)) {
        if (new) {
            file.remove(file)
        } else stop("File aready exists")
    }
    if (!missing(template)) {
        if (is.character(template)) {
            template <- create.neuro(template,flipneg=flipneg,info="Template",...)
        }
    } else {    
        template <- get(data("neuro.template"))
    }
    
    if (!missing(atlas)) {
        if (is.character(atlas)) {
            atlas <- create.neuro(atlas,flipneg=flipneg,info="Atlas",...)       }    
    } else {
        atlas <- get(data("neuro.atlas116"))
    }

    dimX <-  with(neurocdf:::neuro.env, dimdefNCDF)("X","mm",(seq_len(dim(template$vol)[1])-template$origin[1])*sign(template$voxelsize[1])*template$voxelsize[1])
    dimY <-  with(neurocdf:::neuro.env, dimdefNCDF)("Y","mm",(seq_len(dim(template$vol)[2])-template$origin[2])*sign(template$voxelsize[2])*template$voxelsize[2])
    dimZ <-  with(neurocdf:::neuro.env, dimdefNCDF)("Z","mm",(seq_len(dim(template$vol)[3])-template$origin[3])*sign(template$voxelsize[3])*template$voxelsize[3])
    dim3 <- with(neurocdf:::neuro.env, dimdefNCDF)("Coord","units",1:3)
    dim2 <- with(neurocdf:::neuro.env, dimdefNCDF)("Pair","units",1:2)
    dimInfo <- with(neurocdf:::neuro.env, dimdefNCDF)("Info","units",1:16)
    dimROI <- with(neurocdf:::neuro.env, dimdefNCDF)("nROI","units",1:512)
    dimStr <- with(neurocdf:::neuro.env, dimdefNCDF)("String","units",1:128)
    dimType <- with(neurocdf:::neuro.env, dimdefNCDF)( "nType", "units", seq_len(ntype), unlim=FALSE)
    dimT <- with(neurocdf:::neuro.env, dimdefNCDF)( "nSubject", "units", seq_len(nid), unlim=FALSE)
    nglobal <- 2
    natlas <- 1
    if (class(atlas)!="neuro") {
        natlas <- length(atlas)
        nglobal <- natlas+1
    } else {
        atlas <- list(atlas)
    }
    dimTglobal <- with(neurocdf:::neuro.env, dimdefNCDF)( "nGlobal", "units", seq_len(nglobal), unlim=FALSE)
    dimRes <- with(neurocdf:::neuro.env, dimdefNCDF)( "nResult", "units", 1 , unlim=TRUE)
    
    prec <- "single"
    misval <- with(neurocdf:::neuro.env, misval)
    img <- with(neurocdf:::neuro.env, vardefNCDF)( "SubjectImage", "units", list(dimX,dimY,dimZ,dimType,dimT), prec=prec ,missval=misval,compression=compression)
    global <- with(neurocdf:::neuro.env, vardefNCDF)( "GlobalImage", "units", list(dimX,dimY,dimZ,dimTglobal) ,missval=misval,compression=compression) ##,missval=NA)
    results <- with(neurocdf:::neuro.env, vardefNCDF)( "ResultImage", "units", list(dimX,dimY,dimZ,dimRes) ,missval=misval,compression=compression) ##,missval=NA)
    
    origovar <- with(neurocdf:::neuro.env, vardefNCDF)("Origin", "units", dim3, missval=misval)
    vsizevar <- with(neurocdf:::neuro.env, vardefNCDF)("VoxelSize", "units", dim3, missval=misval)


    if (get("netcdf",envir=neurocdf:::neuro.env)==4) {
        atlasdesc <- with(neurocdf:::neuro.env, vardefNCDF)("AtlasDescription", "units", list(dimStr,dimROI,dim2,dimTglobal), prec="char")
        descvar <- with(neurocdf:::neuro.env, vardefNCDF)("SubjectDescription", "units", list(dimStr,dimInfo,dimType,dimT), prec="char")
        globaldesc <- with(neurocdf:::neuro.env, vardefNCDF)("GlobalDescription", "units", list(dimStr,dimInfo,dimTglobal), prec="char")
        resultdesc <- with(neurocdf:::neuro.env, vardefNCDF)( "ResultDescription", "units", list(dimStr,dimInfo,dimRes) , prec="char") ##,missval=NA)
    } else {
        atlasdesc <- with(neurocdf:::neuro.env, vardefNCDF)("AtlasDescription", "units", list(dimStr,dimROI,dim2,dimTglobal), prec="char", missval=NA)
        descvar <- with(neurocdf:::neuro.env, vardefNCDF)("SubjectDescription", "units", list(dimStr,dimInfo,dimType,dimT), prec="char", missval=NA)
        globaldesc <- with(neurocdf:::neuro.env, vardefNCDF)("GlobalDescription", "units", list(dimStr,dimInfo,dimTglobal), prec="char", missval=NA)
        resultdesc <- with(neurocdf:::neuro.env, vardefNCDF)( "ResultDescription", "units", list(dimStr,dimInfo,dimRes) , prec="char", missval=NA) ##,missval=NA)
    }

    nc <- with(neurocdf:::neuro.env, createNCDF)(file, list(img,descvar,global,globaldesc,atlasdesc,results,resultdesc,vsizevar,origovar) )
    with(neurocdf:::neuro.env, putvarNCDF)(nc, "GlobalImage", template$vol, start=c(1,1,1,1),count=c(-1,-1,-1,1))
    desc <- ifelse(is.null(template$info),"Template",template$info)
    lab <- list(cbind("",""))

    
    for (i in seq_len(length(atlas))) {
        with(neurocdf:::neuro.env, putvarNCDF)(nc, "GlobalImage", atlas[[i]]$vol, start=c(1,1,1,i+1),count=c(-1,-1,-1,1))
        desc <- c(desc,              
                  ifelse(is.null(atlas[[i]]$info),paste("Atlas",i),
                         atlas[[i]]$info))
        if (!is.null(atlas[[i]]$desc)) {
            if (NCOL(atlas[[i]]$desc)==1) {
                lab <- c(lab,list(as.matrix(
                    cbind(seq_len(length(atlas[[i]]$desc)),
                          atlas[[i]]$desc))))
            } else {
                lab <- c(lab,list(as.matrix(atlas[[i]]$desc)))
            }
        } else lab <- c(lab,list(cbind("","")))
    }
    nlab <- max(unlist(lapply(lab,nrow)))
    for (i in seq_len(length(lab)))
        if (NROW(lab[[i]])<nlab) lab[[i]] <- rbind(lab[[i]],matrix(rep("",2*(nlab-NROW(lab[[i]]))),ncol=2))  
    lab <- array(do.call("cbind",lab),c(nlab,2,nglobal))

    with(neurocdf:::neuro.env, putvarNCDF)(nc, "GlobalDescription", desc, start=c(1,1,1),count=c(-1,1,length(desc)))
    with(neurocdf:::neuro.env, putvarNCDF)(nc,"AtlasDescription",lab,start=c(1,1,1,1),count=c(-1,nrow(lab),2,nglobal))
    with(neurocdf:::neuro.env, putvarNCDF)(nc,"ResultDescription","Empty",start=c(1,1,1),count=c(-1,1,1))

    
    with(neurocdf:::neuro.env, putvarNCDF)(nc,"Origin",template$origin,start=1,count=-1)
    with(neurocdf:::neuro.env, putvarNCDF)(nc,"VoxelSize",abs(template$voxelsize),start=1,count=-1)

    if (is.null(path)) {
        res <- with(neurocdf:::neuro.env, closeNCDF)(nc)
        return(neurocdf(file))
    }

        
    StrMatrix <- array(dim=c(dimInfo$len,ntype,nid))
    StrMatrix[] <- ""
    id <- 0
    path0 <- NULL
    if (!is.list(path))
        path0 <- path.expand(path)
    pb <- txtProgressBar(style=2,width=10)
    count <- 0; 
    for (d in dirs) {
        id <- id+1
        sets <- sort(list.files(d,include.dirs=TRUE,recursive=FALSE))
        sets0 <- setdiff(allsets,sets)
        newStr <- c()
        myid <- tail(strsplit(d,"/")[[1]],1)
        if (substr(myid,1,1)=="/") myid <- substr(myid,2,nchar(myid))
        for (s in allsets) {
            ##            s0 <- gsub(paste(path.expand(d),"/",sep=""),"",s)
            if (s %in% sets) {            
                count <- count+1
                setTxtProgressBar(pb, count/R); flush.console()
                if (is.null(path0)) {
                    message(" ", round(count/R*1000)/10, "%  ", d ,appendLF=FALSE); 
                } else {
                    message(" ", round(count/R*1000)/10, "%  ", sub(path0,"",s),appendLF=FALSE);                     
                }
                flush.console()            
                files <- sort(list.files(
                                         path.expand(paste(d,s,sep="/"))
                                         ,full.names=TRUE))
                if (length(files)==2 | length(files)==1) {
                    fsplit <- strsplit(files[1],".",fixed=TRUE)[[1]]
                    fileprefix <- paste(fsplit[-length(fsplit)],collapse=".")
                    filepostfix <- fsplit[length(fsplit)]
                    if (filepostfix=="gz") {
                        if (length(files)>1) stop("Compressed file and more than one file in directory: ", files)
                        system(paste("cd ", dirname(files[1]), "; gunzip -c * > ", fileprefix))
                    }
                    infile <- create.neuro(fileprefix,flipneg=flipneg,info=paste("Image",files[1]),...)
                    if (filepostfix=="gz") {
                        system(paste("cd ", dirname(files[1]), "; rm ",fileprefix))
                    }
                    if (!all(abs(infile$voxelsize)==abs(template$voxelsize))) stop("Voxelsize of atlas and template does not agree")
                } else stop("Wrong file structure (one image-file (Analyze or NIFTI) pr directory")
            }
            img <- match(s,allsets)
            val <- c(myid,s,infile$hdrdesc)
            newStr <- cbind(newStr,val)
            ## infile$voxelsize <- abs(infile$voxelsize)
            ## infile$vol <- infile$vol[rev(seq_len(dim(infile$vol)[1])),,]
            offset <- c(1,1,1)+template$origin-infile$origin
            if (s %in% sets) {
                with(neurocdf:::neuro.env, putvarNCDF)(nc,"SubjectImage",infile$vol,start=c(offset,img,id),count=c(dim(infile$vol),1,1))
            } else {
                with(neurocdf:::neuro.env, putvarNCDF)(nc,"SubjectImage",array(NA,dim(template$vol)),start=c(1,1,1,img,id),count=c(-1,-1,-1,1,1))
            }
        }      
        StrMatrix[1:3,,id] <- newStr
    }
    message("")
    close(pb)    
    with(neurocdf:::neuro.env, putvarNCDF)(nc,"SubjectDescription",StrMatrix,start=c(1,1,1,1),count=c(-1,-1,-1,-1))

    ## Attributes
    ## sets <- gsub(paste(d,"/",sep=""),"",sets)  
    ## for (i in seq_len(length(sets))) {
    ##   res <- att.put.(nc, 0, ## global attributes
    ##                       paste("type",i,sep=""), 
    ##                       sets[i])    
    ## }
    res <- with(neurocdf:::neuro.env, closeNCDF)(nc)
    ##return(invisible(res))
    return(neurocdf(file))
}
