##' @export
writevol <- function(x,filename="test",ANALYZE=FALSE,flip=TRUE,gzipped=FALSE,...) {
    res <- oro.nifti::readNIfTI(system.file("brains/single_subj_T1.nii",package="neurocdf")) ## slotNames(b2)

    hdr <- list(datatype=16, dim=dim(res@.Data), 
                srow_x=res@srow_x, srow_y=res@srow_y, srow_z=res@srow_z,
                qoffset_x=res@qoffset_x, qoffset_y=res@qoffset_y, qoffset_z=res@qoffset_z,
                xyzt_units=res@xyzt_units)
    if (flip) hdr$srow_x <- -hdr$srow_x
    if (ANALYZE) {
        out <- do.call(oro.nifti::anlz, c(list(img=x[]),hdr))
        oro.nifti::writeANALYZE(out,filename=filename,gzipped=gzipped)
    } else {
        out <- do.call(oro.nifti::nifti, c(list(img=x[]),hdr))
        oro.nifti::writeNIfTI(out,filename=filename,gzipped=gzipped,onefile=TRUE)
    }
}

