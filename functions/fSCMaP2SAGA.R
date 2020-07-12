print("Converting multi.dimensional SCMaP file to single SAGA GRID files")
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
fSCMaP2SAGA <- function(SCMaP.DIR,
                       SCMaP.FILE,
                       SCMaP.PF,
                       SCMaP.FRM,
                       OUT.DIR,
                       EXTENT=TRUE){
#-----------------------------------------------------------------------------------------------------
print("Import SCMaP file")
#-----------------------------------------------------------------------------------------------------
r <- stack(paste(SCMaP.DIR,SCMaP.FILE,SCMaP.FRM,sep=""))

if(EXTENT==TRUE){
#-----------------------------------------------------------------------------------------------------
print("Export extent shape file")
#-----------------------------------------------------------------------------------------------------
e <- extent(r)
p <- as(e, 'SpatialPolygons')
crs(p) <- crs(r)
setwd(file.path(SCMaP.DIR))
shapefile(p, "Extent2.shp")
}

#-----------------------------------------------------------------------------------------------------
print("Generating single RS files in SAGA format")
#-----------------------------------------------------------------------------------------------------
pb <- txtProgressBar(min=0, max=length(r@layers), style=3)
for(i in 1:length(r@layers)){
   r[[i]][r[[i]] < 0] <- NA
   writeRaster(r[[i]],paste(OUT.DIR,SCMaP.PF,i,sep=""),
                format="SAGA",
                overwrite=TRUE)
    setTxtProgressBar(pb, i)
  }
}

