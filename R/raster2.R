  # raster2
#
# GIS function samples
#
# Rstudio package


#' convert image matrix to raster matrix
#'
#' @param m　　"image" matrix
#'
#'
#' @return　"raster" matrix
#' @export
#'
#' @examples
#'
#' img2ras(img)
img2ras <- function(m){t(m)[ncol(m):1,] }   ####



#' ras2img
#'
#'　convert from "raster" matrix to "image" matrix
#'　
#' @param m "raster" matrix
#'
#' @return  "image" matrix
#' @export
#'
#' @examples
#' ras2img(ras)
ras2img <- function(m){t(m)[,nrow(m):1]}


#' Return  reference number i,j of raster matrix from x, y coordinates
#'
#' @param x  coordinates
#' @param y  coordinates
#' @param r  class of raster data
#'
#' @return  integer matrix of reference number i,j
#' @export
#'
#' @examples
#' xy2ij(x,y,r)
xy2ij<-function(x,y,r){
  ext <- extent(r)
  x1 <- ext[1] ; x2 <- ext[2] ;y1 <- ext[3] ; y2 <- ext[4]
  dx <- x2-x1 ; dy <- y2-y1
  rn <- dim(r)[1];cn <- dim(r)[2]
  stpx_ <- dx/cn ;stpy_ <- dy/rn

  i<-floor((y2-y)/stpy_)+1 ; i[i>rn] <- rn
  j<-floor((x-x1)/stpy_)+1 ; j[j>cn] <- cn
  return(cbind(i,j))
}




#' Return cell number of raster matrix from x,y coordinates
#' @param x  coordinates
#' @param y  coordinates
#' @param r  class of raster data
#'
#' @return integer of cell number
#' @export
#'
#' @examples
#'
#' xy2n(x,y,r)
#'
xy2n<-function(x,y,r){
  ij<-xy2ij(x,y,r)
  i <- ij[,1] ; j <- ij[,2]
  n <-ncol(r)*(i-1)+j
  return(n)
}


#' crop xyz data matrix or data frame form extent c(x1,x2,y1,y2)
#'
#' @param xyz matrix of x,y,z coordinates
#' @param ext matrix of x,y,z coordinates
#'
#' @return xyz data matrix or data frame
#' @export
#'
#' @examples
#' xyz_ext(dtm)
xyz_ext <- function(xyz,ext){
  x <- xyz[,1] ; y <- xyz[,2]
  i <- ext[1] <= x & x<= ext[2] & ext[3] <= y & y<= ext[4]
  return(xyz[i,])
}

#' surface3dr
#' draw surface 3d from raster data
#' @param r
#'
#' @return No return values, only draw surface3d.
#' @export
#'
#' @examples
#' .dir<-system.file("extdata",package="raster2")
#' filename <- paste0(.dir,"/07KE0024_office.tif")
#' r<-raster::raster(filename)
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


#' dsm_files
#'
#' @param ext_
#' @param files
#' @param extents
#' @param rect_drow
#'
#' @return character vector of file names
#' @export
#'
#' @examples
#'
#'
#' ext_dsm<-read.csv("extents_dsm.csv")
#' ext_ <- c(5100,5200,-1400,1100)
#' dsm_files(ext_)
dsm_files<-function(ext_=c(5100,5200,-1400,1100),files=ext_dsm$f,extents=ext_dsm[,c("x1","x2","y1","y2")],rect_drow=T){
  if(rect_drow)rect(ext_[1],ext_[3],ext_[2],ext_[4],border="red")
  i<-which( (x1<=ext_[1] & x2<=ext_[2]) & (y1<=ext_[3] & y2<=ext_[4]))
  return(ext_dsm$f[i])
}


#' Return data frame of RGB color for pixels from `magick` image data
#'
#' @param x x coordinates
#' @param y y coordinates
#' @param rgbm  matrix of RGB color generated from `magick`
#' @param x1 starting point of x
#' @param xstp step of x sampling interval
#' @param y1 starting point of y
#' @param ystp xstp step of y sampling interval
#'
#' @return data frame of RGB color for pixels
#'
#' @export
#'
#' @examples
#' img<-magick::image_read("07KE0024_office.tif")
#' plot(img)
#' rgb <- as.integer(img[[1]])
#' x1<-3780;x2<-4000;y1<--1170;y2<- -950　###   平面直角座標系の位山宿舎の範囲
#' xstp <- 0.5 ; ystp <- 0.5　#画素
#' load("KuraiyamaOfficeDSM_xyz.RData")
#' dsm<-xyz
#' rgb_ <- xy_rgb(dsm$x,dsm$y,rgb,x1,0.5,y1,0.5)
#' open3d()
#' points3d(dsm,col=rgb(rgb_,maxColorValue = 255))

xy_rgb <- function(x,y,rgbm=rgb,x1=x1,xstp=0.5,y1=y1,ystp=0.5){
  r<-rgbm[,,1];g<-rgbm[,,2];b<-rgbm[,,3]
  i <- cbind((y2-y)%/%ystp+1,(x-x1)%/%xstp+1 )#length(y) 193600
  return(data.frame(r=r[i],g=g[i],b=b[i]))
}

#' Return a vector of Digital Crown Height Model(DCHM) from DSM x,y,z, and DTM raster
#'
#' @param x DSM x
#' @param y DSM y
#' @param r DTM raster
#'
#' @return a vector of Digital Crown Height Model(DCHM)
#' @export
#'
#' @examples
#' # load ortho GeoTif ####
#'.dir<-system.file("extdata",package="raster2")
#' filename <- paste0(.dir,"/07KE0024_office.tif")
#'  b<-raster::brick(filename)
#' raster::plotRGB(b)
#' # 3D graphics ####
#' v<-raster::getValues(b)
#' rgb_<-v[cellFromXY(b, dsm[,c("x","y")]),]
#' rgl::open3d()
#' rgl::points3d(dsm,col=rgb(rgb_,maxColorValue = 255))
#' # DTM
#' filename <- paste0(.dir,"/dtm.tif")
#' dtm_raster<-raster::raster(filename)
#' contour(dtm_raster)
#' # dchm_vector
#' dchm <- dchm_vector(x=dsm$x,y=dsm$y,dtm_raster)
#' rgl::open3d()
#' rgl::points3d(dchm,col=rgb(rgb_,maxColorValue = 255))
dchm_vector <- function(x=dsm$x,y=dsm$y,dtm_raster=r){
  r <- dtm_raster
  n <- cellFromXY(r, dsm[,c("x","y")])
  z_ <- getValues(r)
  h <- dsm$z-z_[n]
  dchmv<-data.frame(x,y,h)
  return(dchmv)
}

#' Return a raster of Digital Crown Height Model(DCHM) from DSM x,y,z, and DTM raster
#'
#' @param x DSM x
#' @param y DSM y
#' @param r DTM raster
#'
#' @return a raster of Digital Crown Height Model(DCHM)
#' @export
#'
#' @examples
#' # load ortho GeoTif ####
#'.dir<-system.file("extdata",package="raster2")
#' filename <- paste0(.dir,"/07KE0024_office.tif")
#'  b<-raster::brick(filename)
#' raster::plotRGB(b)
#' # 3D graphics ####
#' v<-raster::getValues(b)
#' rgb_<-v[cellFromXY(b, dsm[,c("x","y")]),]
#' rgl::open3d()
#' rgl::points3d(dsm,col=rgb(rgb_,maxColorValue = 255))
#' filename <- paste0(.dir,"/dtm.tif")
#' dtm_raster<-raster::raster(filename)
#' contour(dtm_raster)
#' CHM<-dchm_raster(dsm$x,y=dsm$y,dtm_raster)
#' plot(CHM)
#' # ForestTools::vwf
#' lin <- function(x){x * 0.05 + 0.6}
#' ttops <- ForestTools::vwf(CHM = CHM, winFun = lin, minHeight = 2)
#' plot(ttops, col = "blue", pch = 20, cex = 0.5, add = TRUE) # plot of ttops
#' # Plot crowns
#' crowns <- ForestTools::mcws(treetops = ttops, CHM = CHM, minHeight = 1.5)
#' plot(crowns, col = sample(rainbow(50), length(unique(crowns[])), replace = TRUE), legend = FALSE, xlab = "", ylab = "", xaxt='n', yaxt = 'n')
#'
#' tt<-data.frame(ttops,sf::st_coordinates(ttops)) #write.csv(tt,file="Kuraiyama_Office_ttop.csv")
#' head(tt)
#' #### 地盤高データ追加
#'
#' Z<-raster::getValues(dtm_raster)[cellFromXY(dtm_raster,tt[,c("X","Y")])]
#' tt<-data.frame(tt,Z)
#' head(tt)
#' ####　地盤高水平化
#' dchm <- dchm_vector(x=dsm$x,y=dsm$y,dtm_raster)
#' rgl::open3d()
#' rgl::points3d(dchm[dchm$h <0.5,],col="red")
#' rgl::points3d(dchm[dchm$h >= 0.5 & dchm$h <2 ,],col="green")
#' rgl::points3d(dchm,col=rgb(rgb_,maxColorValue = 255))
#' n<-nrow(tt)
#' i<-rep(seq(n),each=2)
#' trunk_xyz<-tt[i,c("X","Y","height")]
#' trunk_xyz$height[2*seq(n)]<-0
#' rgl::segments3d(trunk_xyz)
#'
#' #####　地盤高　+ dsm + オルソ　+　立木
#'
#' rgl::open3d()
#' h<-dchm$h
#' rgl::points3d(dsm[h >= 0.5 & h < 2 ,],col="green")
#' rgl::points3d(dsm[h <0.5,],col="magenta")
#' rgl::points3d(dsm,col=rgb(rgb_,maxColorValue = 255))
#' n<-nrow(tt)
#' i<-rep(seq(n),each=2)
#' trunk_xyz_dsm<-tt[i,c("X","Y","Z")]
#' trunk_xyz_dsm$Z[2*seq(n)]<-tt$Z+tt$height
#' rgl::segments3d(trunk_xyz_dsm)
#' rgl::points3d(tt$X,tt$Y,tt$Z+tt$height,col="red",size=10)

dchm_raster <- function(x=dsm$x,y=dsm$y,dtm_raster=r){
  r <- dtm_raster
  n <- cellFromXY(r, dsm[,c("x","y")])
  z_ <- getValues(r)
  h <- dsm$z-z_[n]
  v<-rep(r@nrows*r@ncols,0)
  maxh<-tapply(h,n,max)#
  k<-as.numeric(names(maxh))
  v[k]<-maxh
  #hist(h)
  dchmr<-setValues(r,v)
  return(dchmr)
}
