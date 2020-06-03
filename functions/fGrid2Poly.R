print("Function to convert raster cells to polygons")
#########################################################################################################
##Libraries
#########################################################################################################
loadandinstall <- function(mypkg) {
  for(i in seq(along=mypkg)){
    if (!is.element(mypkg[i],installed.packages()[,1])){install.packages(mypkg[i])}
    library(mypkg[i], character.only=TRUE)
  }
}
packages <- sort(c("raster",
                   "stars",
                   "magrittr",
                    "sf"))
loadandinstall(packages)
#########################################################################################################
##Function
#########################################################################################################
fGrid2Poly <- function(RASTER.DIR,
                       RASTER.FILE,
                       RASTER.EPSG,
                       RASTER.FRM,
                       AGG=TRUE,
                       AGG.FCT=1,
                       OUT.DIR){
r <- raster(paste(RASTER.DIR,RASTER.FILE,RASTER.FRM,sep=""))
crs(r) <- CRS(paste("+init=EPSG:",RASTER.EPSG,sep=""))
r <- (r*0)+1
if(AGG==TRUE){
  r <- aggregate(r,fact=AGGREGATE, fun=mean)
}
memory.limit(99999)
#https://cran.r-project.org/web/packages/stars/vignettes/stars5.html
x <- st_as_stars(r)  
x <- st_as_sf(x)
x$ID <- 1:nrow(x)
write_sf(x, paste(RASTER.DIR,RASTER.FILE,".shp",sep=""), delete_layer = TRUE)
}
