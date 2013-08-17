neuro.env <- new.env()

.onAttach <- function(...) {
##    if (suppressWarnings(require(ncdf4))) {
        ## packageStartupMessage("Using ncd4")
        assign("netcdf",4,envir=neuro.env)
        assign("createNCDF",function(...) ncdf4:::nc_create(...,force_v4=TRUE),
               envir=neuro.env)
        assign("openNCDF",ncdf4:::nc_open,
               envir=neuro.env)
        assign("closeNCDF",ncdf4:::nc_close,
               envir=neuro.env)
        assign("putvarNCDF",ncdf4:::ncvar_put,
               envir=neuro.env)
        assign("getvarNCDF",function(...) {
            res <- ncdf4:::ncvar_get(...)
            ##    res[res>=1e30]
            return(res)
        },envir=neuro.env)
        assign("dimdefNCDF",ncdf4:::ncdim_def,
               envir=neuro.env)
        assign("vardefNCDF",ncdf4:::ncvar_def,
               envir=neuro.env)
        assign("misval",1e30,
               envir=neuro.env)
        ##  } else { ## No longer support netcdf3
        ## if (!require(ncdf)) warning("ncdf4 or ncdf required!") else {
        ##     ## packageStartupMessage("Using ncdf (3)")
        ##     assign("netcdf",3,envir=neuro.env)
        ##     assign("createNCDF",ncdf:::create.ncdf,
        ##            envir=neuro.env)
        ##     assign("openNCDF",ncdf:::open.ncdf,
        ##            envir=neuro.env)
        ##     assign("closeNCDF",ncdf:::close.ncdf,
        ##        envir=neuro.env)
        ##     assign("putvarNCDF",ncdf:::put.var.ncdf,
        ##            envir=neuro.env)
        ##     assign("getvarNCDF",function(...) {
        ##         res <- ncdf:::get.var.ncdf(...)
        ##         if (is.character(res[1])) {
        ##             res[which(res=="")] <- NA
        ##         } else {
        ##             res[res>=1e30] <- NA
        ##     }
        ##         return(res)
        ##     },envir=neuro.env)
        ##     assign("dimdefNCDF",ncdf:::dim.def.ncdf,
        ##            envir=neuro.env)
        ##     assign("vardefNCDF",ncdf:::var.def.ncdf,
        ##            envir=neuro.env)
        ##     assign("misval",1e30,
        ##            envir=neuro.env)
        ## }
    ##}
}
