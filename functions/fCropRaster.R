##Function to crop one- or multi.dimensional raster files
#########################################################################################################
##Libraries
#########################################################################################################
loadandinstall <- function(mypkg) {
  for(i in seq(along=mypkg)){
    if (!is.element(mypkg[i],installed.packages()[,1])){install.packages(mypkg[i])}
    library(mypkg[i], character.only=TRUE)
  }
}
packages <- sort(c("raster"))
loadandinstall(packages)
#########################################################################################################
##Function
#########################################################################################################
fCropRaster <- function(RU.DIR,
                        RU.SHP,
                        RASTER.DIR,
                        RASTER.GRD,
                        RASTER.FRM,
                        RASTER.EPSG,
                        MULTI=TRUE,
                        EXTENT=TRUE){
s <- shapefile(file.path(RU.DIR,RU.SHP),verbose=TRUE)
if(MULTI==TRUE){
r <- stack(paste(RASTER.DIR,RASTER.GRD,RASTER.FRM,sep=""))}
if(MULTI==FALSE){
r <- raster(paste(RASTER.DIR,RASTER.GRD,RASTER.FRM,sep=""))}
crs(r) <- CRS(paste("+init=epsg:",RASTER.EPSG,sep=""))
#reproject shape file
s <- spTransform(s, r@crs)
if(EXTENT==FALSE){
r.crop <- crop(r, s)}
if(EXTENT==TRUE){
r.crop <- crop(r, extent(s))}
#r.crop = projectRaster(r.crop, crs = "+init=epsg:25832", method = "bilinear")
if(RASTER.FRM==".tif"){
writeRaster(r.crop, filename=paste(RASTER.DIR,RASTER.GRD,"_CROP",RASTER.FRM,sep=""), format="GTiff", overwrite=TRUE)}
if(RASTER.FRM==".asc"){
  writeRaster(r.crop, filename=paste(RASTER.DIR,RASTER.GRD,"_CROP",RASTER.FRM,sep=""), format="ascii", overwrite=TRUE)}
if(RASTER.FRM==".sdat"){
  writeRaster(r.crop, filename=paste(RASTER.DIR,RASTER.GRD,"_CROP",RASTER.FRM,sep=""), format="SAGA", overwrite=TRUE)}
}
