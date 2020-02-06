print("Function: Color composite of terrain attributes")
fColorComposite <- function(
  DATA.DIR,
  RESULT.DIR,
  B1,
  B2,
  B3,
  SHD,
  ALPHA,
  H,
  W,
  RES){
#Working directory
setwd(file.path(DATA.DIR))
rgbRaster <- stack(c(B1,B2,B3))
SHD <- raster(SHD)

png(paste("MAP_",DEM,c(".png"),sep=""),width=W,height=H,res=RES)
par(xpd = T, mar = par()$mar + c(0,0,0,5))
plot(SHD,
     col = grey(100:1/100),
     box=FALSE,
     legend=FALSE,
     axes = TRUE)

plotRGB(rgbRaster, 1, 2, 3, 
        stretch='hist',
        axes=TRUE,
        alpha=255*ALPHA,
        add=TRUE)
dev.off()
#######################################################################################################
#Export
#######################################################################################################
setwd(file.path(RESULT.DIR))
output.name <- paste("CC_",substr(B1,1,nchar(B1)-4),"-",substr(B2,1,nchar(B2)-4),"-",substr(B3,1,nchar(B3)-4),sep="")
writeRaster(rgbRaster,paste(output.name,sep=""),"GTiff", overwrite=TRUE)
}
