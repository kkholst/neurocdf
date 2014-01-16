##' @export
writevol <- function(x,filename="test",ANALYZE=FALSE,flip=TRUE,gzipped=FALSE,...) {
    ## res <- oro.nifti::readNIfTI(system.file("brains/single_subj_T1.nii",package="neurocdf")) ## slotNames(b2)
    ## res <- oro.nifti::readNIfTI("con_0007") ## slotNames(b2)

    L <- Rniftilib::nifti.image.read(system.file("brains/con_0011.hdr",package="neurocdf"))
    Rniftilib::nifti.set.filenames(L, filename, check=0, set_byte_order=1)
    L[] <- x[]
    Rniftilib::nifti.image.write(L)
    return(NULL)
    
    hdr <- list()
    omit <- c(1:9,14,15,30,31,43,44,45,46,24,25)
    keep <- c(10,11,12,13,14,16,17,18,19,20,21,22,23,27,
              28,29,
              32,33,34,35,36,37,38,39,
              40,41,42)

    L <- list()
    for (i in slotNames(res)[-1]) {
        new <- list(slot(res,i)); names(new) <- i
        L <- c(L,new)
    }
        
##     keep <- c(
##         "vox_offset",
##         "scl_slope","scl_inter",
##         "intent_code","qform_code","sform_code",
## #        "quatern_b","quatern_c","quatern_d",
##         "intent_p1","intent_p2","intent_p3",
##         "srow_x","srow_y","srow_z",
##         "qoffset_x","qoffset_y","qoffset_z",
## #        "xyzt_units",
##         "pixdim")
##     for (i in keep) {
##         new <- list(slot(res,i)); names(new) <- i
##         hdr <- c(hdr, new)
##     }
    hdr$datatype <- 16
    hdr$dim <- dim(x)
    browser()
    
    ## L <- f.read.nifti.header(system.file("brains/single_subj_T1.nii",package="neurocdf"))
    ## L$scl.slope <- NULL
    ## L$scl.inter <- NULL
    ## L$datatype <- 16
    ## L$filename <- NULL
    ## f.write.nifti(x[],filename,size="float",L=L)
    ## return(NULL)
    ## hdr <- list(datatype=16, dim=dim(res@.Data), 
    ##             srow_x=res@srow_x, srow_y=res@srow_y, srow_z=res@srow_z,
    ##             qoffset_x=res@qoffset_x, qoffset_y=res@qoffset_y, qoffset_z=res@qoffset_z,
    ##             xyzt_units=res@xyzt_units,
    ##             pixdim=res@pixdim
    ##             )
    ##if (flip) hdr$srow_x <- -hdr$srow_x
    if (ANALYZE) {
        out <- do.call(oro.nifti::anlz, c(list(img=x[]),hdr))
        oro.nifti::writeANALYZE(out,filename=filename,gzipped=gzipped)
    } else {
        out <- do.call(oro.nifti::nifti, c(list(img=x[]),hdr))
        oro.nifti::writeNIfTI(out,filename=filename,gzipped=gzipped,onefile=TRUE)
    }
}
