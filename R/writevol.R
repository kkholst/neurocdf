##' @export
writevol <- function(x,filename="test",ANALYZE=FALSE,flip=TRUE,gzipped=FALSE,template,...) {
    if (missing(template)) {
        template <- system.file("brains/con_0007.hdr.gz",package="neurocdf")
        template <- gsub(".hdr","",template)
    }
    res <- oro.nifti::readNIfTI(template)
    ## oro.nifti::writeNIfTI(res,filename="con_0007",gzipped=TRUE,onefile=TRUE)
    ## require(Rniftilib)
    ## ff <- gsub(".hdr","",system.file("brains/con_0007.hdr",package="neurocdf"))
    ## L <- nifti.image.read(ff)
    ## nifti.set.filenames(L, filename, check=0, set_byte_order=1)
    ## L[] <- x
    ## nifti.image.write(L)
    ## return(NULL)
    ## require(AnalyzeFMRI)
    ## L <- f.read.nifti.header(system.file("brains/single_subj_T1.nii",package="neurocdf"))
    ## L$scl.slope <- NULL
    ## L$scl.inter <- NULL
    ## L$datatype <- 16
    ## L$filename <- NULL
    ## f.write.nifti(x[],filename,size="float",L=L)
    ## return(NULL)
    hdr <- list()
    keep <- c(
        ## "vox_offset",
        "scl_slope","scl_inter",
        ## "intent_code","qform_code","sform_code",
        "quatern_b","quatern_c","quatern_d",
        "intent_p1","intent_p2","intent_p3",
        "srow_x","srow_y","srow_z",
        "qoffset_x","qoffset_y","qoffset_z",
        "xyzt_units",
        "pixdim")
    for (i in keep) {
        new <- list(slot(res,i)); names(new) <- i
        hdr <- c(hdr, new)
    }
    hdr$datatype <- 16
    hdr$dim <- dim(x)
    ##if (flip) hdr$srow_x <- -hdr$srow_x
    if (ANALYZE) {
        out <- do.call(oro.nifti::anlz, c(list(img=x[]),hdr))
        oro.nifti::writeANALYZE(out,filename=filename,gzipped=gzipped)
    } else {
        out <- do.call(oro.nifti::nifti, c(list(img=x[]),hdr))
        oro.nifti::writeNIfTI(out,filename=filename,gzipped=gzipped,onefile=TRUE)
    }
}
