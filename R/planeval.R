##' @export
planeval <- function(coord,...)  {
  unlist(lapply(coord,function(z) {
                if (is.numeric(z)) return(z)
                switch(tolower(z),
                       x=,
                       median=,
                       sagittal=1,
                       y=,
                       coronal=,
                       frontal=2,
                       z=,
                       transverse=,
                       axial=3
                       )
       }))
}
