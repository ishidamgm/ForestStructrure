#### 00_gis_source2.R
# source("00_gis_source2.R")
#library(rgl)

if(0){

  #' convert image matrix to raster matrix
  #'
  #' @param m
  #'
  #'
  #' @return
  #' @export
  #'
  #' @examples
  img2ras <- function(m){t(m)[ncol(m):1,] }   ####

  #' Title
  #'
  #' @param m
  #'
  #'
  #' @return
  #' @export
  #'
  #' @examples
  img2ras <- function(m){t(m)[ncol(m):1,] }   #### convert image matrix to raster matrix

  #' ras2img
  #'
  #' @param m
  #'
  #' @return
  #' @export
  #'
  #' @examples
  ras2img <- function(m){t(m)[,nrow(m):1]}

  #' xy2ij
  #'
  #' @param x
  #' @param y
  #' @param r
  #'
  #' @return
  #' @export
  #'
  #' @examples
  xy2ij<-function(x,y,r){
    ext <- extent(r)
    x1 <- ext[1] ; x2 <- ext[2] ;y1 <- ext[3] ; y2 <- ext[4]
    dx <- x2-x1 ; dy <- y2-y1
    rn <- dim(r)[1];cn <- dim(r)[2]
    stpx_ <- dx/cn ;　  stpy_ <- dy/rn

    i<-floor((y2-y)/stpy_)+1 ; i[i>rn] <- rn
    j<-floor((x-x1)/stpy_)+1 ; j[j>cn] <- cn
    return(cbind(i,j))
  }


  #' surface3dr
  #' draw surface 3d from raster data
  #' @param r
  #'
  #' @return
  #' @export
  #'
  #' @examples
  #' r<-raster("07ke013_dchm_NA_filled.tif")
  #' plot(r)
  #' surface3dr(r,col="green")
  #'
  #
  surface3dr<-function(r,...){
    ext.<-extent(r)
    rcn.<-dim(r)
    xaxis.<- seq(ext.[1],ext.[2],length=rcn.[2])
    yaxis.<- seq(ext.[3],ext.[4],length=rcn.[1])
    m<-ras2img(as.matrix(r))
    surface3d(xaxis.,yaxis.,m,...)
  }
}


