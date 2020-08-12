print("Mosaicing of BKG DEM files")
#######################################################################################################
#packages
#######################################################################################################
loadandinstall <- function(mypkg) {if (!is.element(mypkg,
                                                   installed.packages()[,1])){install.packages(mypkg)};
  library(mypkg, character.only=TRUE)}
pk <- c("raster",
        "sf")
for(i in pk){loadandinstall(i)}
#######################################################################################################
#function
#######################################################################################################
fMosaicBKG <- function(RASTER.DIR,
                    VECTOR.FILE,
                    VECTOR.GRID,
                    MOSAIC.DIR,
                    MOSAIC.NAME,
                    AGGREGATE,
                    RASTER.FRM){
#----------------------------------------------------------------------------------------------
print("Import data")
#----------------------------------------------------------------------------------------------
v.f <- st_read(file.path(VECTOR.FILE))
v.g <- st_read(file.path(VECTOR.GRID))
v.f <- st_transform(v.f, st_crs(v.g))  
#----------------------------------------------------------------------------------------------
print("Intersection")
#----------------------------------------------------------------------------------------------
t <- st_intersection(v.g,v.f)  
l.grd <- paste(as.character(t$KACHEL),".asc.zip",sep="")
#----------------------------------------------------------------------------------------------
print("unzip all selected grid tiles files")
#----------------------------------------------------------------------------------------------
setwd(RASTER.DIR)
pb <- txtProgressBar(min=1, max=length(l.grd), style=3)
  for (i in 1:length(l.grd)){
  unzip(l.grd[i])
  setTxtProgressBar(pb, i)
  }
#----------------------------------------------------------------------------------------------
print("Import all selected raster files")
#----------------------------------------------------------------------------------------------
l.grd <- list.files(pattern=paste("*\\.asc$",sep=""),full.names=TRUE, recursive=TRUE)
l.rf <- lapply(1:length(l.grd),
                    function(x) {
                      raster(l.grd[x])
                    })
#----------------------------------------------------------------------------------------------
print("Apply the mosaic function")
#----------------------------------------------------------------------------------------------
l.rf$fun <- mean
r.mosaic <- do.call(mosaic,l.rf)
crs(r.mosaic) <- CRS("+init=epsg:25832")
v.f <- shapefile(VECTOR.FILE)
v.f <- spTransform(v.f, r.mosaic@crs)
r.mosaic <- crop(r.mosaic, extent(v.f))
#------------------------------------------------------------------------------- 
print("Aggregate cell size")
#------------------------------------------------------------------------------- 
if(AGGREGATE>1){
r.mosaic <- aggregate(r.mosaic,fact=AGGREGATE, fun=mean)}
#export mosaic result

crs(r.mosaic) <- CRS("+init=epsg:25832")
if(RASTER.FRM=="asc"){
writeRaster(r.mosaic,
            filename =paste(MOSAIC.DIR,MOSAIC.NAME,".asc",sep=""), 
            overwrite=TRUE, 
            format = "ascii")
}
  
if(RASTER.FRM=="tif"){
writeRaster(r.mosaic,
            filename =paste(MOSAIC.DIR,MOSAIC.NAME,".tif",sep=""), 
            overwrite=TRUE, 
            format = "GTiff")
}  
}
