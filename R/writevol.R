##' @export
writevol <- function(x,filename="test",ANALYZE=FALSE,...) {
    res <- oro.nifti::readNIfTI(system.file("brains/single_subj_T1.nii",package="neurocdf")) ## slotNames(b2)
    res@datatype <- 16
    res@pixdim <- 16
    res@.Data <- x
    if (ANALYZE) {
        oro.nifti::writeANALYZE(oro.nifti::anlz(res),filename=filename,gzipped=FALSE)
    } else {
        oro.nifti::writeNIfTI(oro.nifti::nifti(res),filename=filename,gzipped=FALSE,onefile=TRUE)
    }
}

## slotNames(b)
## b@extents
## b@pixdim
## b2@pixdim
## b@vox_offset
## b2@vox_offset
## lapply(slotNames(b),function(x) eval(parse(text=paste("b@",x,sep=""))))
