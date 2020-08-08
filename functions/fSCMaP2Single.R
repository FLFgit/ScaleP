print("Converting multi-dimensional SCMaP file to single raster files")
#########################################################################################################
##Libraries
#########################################################################################################
loadandinstall <- function(mypkg) {
  for(i in seq(along=mypkg)){
    if (!is.element(mypkg[i],installed.packages()[,1])){install.packages(mypkg[i])}
    library(mypkg[i], character.only=TRUE)
  }
}
packages <- sort(c("raster","BBmisc"))
loadandinstall(packages)
#function to normalize raster files
rescale <- function(x, x.min = NULL, x.max = NULL, new.min = 0, new.max = 1) {
  if(is.null(x.min)) x.min = min(x)
  if(is.null(x.max)) x.max = max(x)
  new.min + (x - x.min) * ((new.max - new.min) / (x.max - x.min))
}
#########################################################################################################
##Function
#########################################################################################################
fSCMaP2Single <- function(SCMaP.DIR,
                       SCMaP.FILE,
                       SCMaP.PF,
                       SCMaP.FRM,
                       OUT.DIR,
                       EXTENT=TRUE,
                       RESCALE=TRUE,
                       OUT.FRM){
#-----------------------------------------------------------------------------------------------------
print("Import SCMaP file")
#-----------------------------------------------------------------------------------------------------
r <- stack(paste(SCMaP.DIR,SCMaP.FILE,SCMaP.FRM,sep=""))
if(EXTENT==TRUE){
#-----------------------------------------------------------------------------------------------------
print("Optional: Export extent shape file")
#-----------------------------------------------------------------------------------------------------
e <- extent(r)
p <- as(e, 'SpatialPolygons')
crs(p) <- crs(r)
setwd(file.path(SCMaP.DIR))
shapefile(p, paste(SCMaP.FILE,".shp",sep=""))
}
#-----------------------------------------------------------------------------------------------------
print("Generating single RS files in SAGA format")
#-----------------------------------------------------------------------------------------------------
pb <- txtProgressBar(min=0, max=length(r@layers), style=3)
for(i in 1:length(r@layers)){
   #Convert values < 0 to NA
   r[[i]][r[[i]] < 0] <- NA
   #Optional: Rescale pixel values between 0 and 1
   if(RESCALE==TRUE){
  r[[i]] <- rescale(r[[i]], x.min = cellStats(r[[i]],stat = "min", na.rm=TRUE), 
                   x.max = cellStats(r[[i]],stat = "max", na.rm=TRUE), 
                   new.min = 0, new.max = 1)
   }
   #export file
   if(OUT.FRM==".sgrd"){
   writeRaster(r[[i]],paste(OUT.DIR,SCMaP.PF,i,sep=""),
                format="SAGA",
                overwrite=TRUE)
   }
   if(OUT.FRM==".tif"){
     writeRaster(r[[i]],paste(OUT.DIR,SCMaP.PF,i,sep=""),
                 format="GTiff",
                 overwrite=TRUE)
   }
   if(OUT.FRM==".asc"){
     writeRaster(r[[i]],paste(OUT.DIR,SCMaP.PF,i,sep=""),
                 format="ascii",
                 overwrite=TRUE)
   }
    setTxtProgressBar(pb, i)
  }
}
