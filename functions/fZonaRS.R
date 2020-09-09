print("Zonal statistics of raster stacks")
#########################################################################################################
##Libraries and functions
#########################################################################################################
loadandinstall <- function(mypkg) {
  for(i in seq(along=mypkg)){
    if (!is.element(mypkg[i],installed.packages()[,1])){install.packages(mypkg[i])}
    library(mypkg[i], character.only=TRUE)
  }
}
packages <- sort(c("dplyr",
                   "exactextractr",
                   "gtools",
                   "raster",
                   "rgdal",
                   "sf"))
loadandinstall(packages)


#########################################################################################################
##Function
#########################################################################################################
fZonaRS <- function(RASTER.DIR,
                    RASTER.FILE,
                    RU.DIR,
                    RU.SHP,
                    RS.PF){

#Import reference units and raster file
p <- st_read(paste(RU.DIR,RU.SHP,".shp",sep=""))
r <- stack(paste(RASTER.DIR,RASTER.FILE,sep=""))

#Generate data frame of zonal statistcis results
print("Zonal statistics")
o <- exact_extract(r,p,'mean')
o <- data.frame(o)
#Rename data frame
colnames(o) <- paste0(RS.PF,1:length(o))

#Combine shape file and data frame  
p <- bind_cols(p, o)

#Remove rows wit NA values
p <- na.omit(p)

#Exprt shape file
write_sf(p,paste(RU.DIR,RU.SHP,".shp",sep=""), delete_layer = TRUE)
}
