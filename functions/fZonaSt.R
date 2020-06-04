print("Zonal statistics of raster files")
#########################################################################################################
##Libraries and functions
#########################################################################################################
loadandinstall <- function(mypkg) {
  for(i in seq(along=mypkg)){
    if (!is.element(mypkg[i],installed.packages()[,1])){install.packages(mypkg[i])}
    library(mypkg[i], character.only=TRUE)
  }
}
packages <- sort(c("raster",
                   "rgdal",
                   "radiant.data", 
                   "rgeos",
                   "RSAGA",
                   "sf",
                   "sp",
                   "stats",
                   "stringr",
                   "tidyr",
                   "utils"))
loadandinstall(packages)


##Setting RSAGA environment
myenv <- rsaga.env(workspace="c:/Users/scien/_temp/", 
                   path="c:/_saga_222_x64",
                   modules = "c:/_saga_222_x64/modules")
print("RSAGA modules")
print(rsaga.get.libraries(path= myenv$modules))
rsaga.get.lib.modules("ta_lighting", env =  myenv, interactive = FALSE)
rsaga.get.usage("ta_lighting",0,env =  myenv)

#########################################################################################################
##Function
#########################################################################################################
fZonaSt <- function(RASTER.DIR,
                    RASTER.PF,
                    RU.DIR,
                    RU.SHP,
                    PM.PF){
setwd(RASTER.DIR)
l.g <- mixedsort(list.files(pattern=paste("^(",PM.PF,").*\\.sgrd$",sep="")),decreasing=FALSE)
#loop
pb <- txtProgressBar(min=0, max=length(l.g), style=3)
  for (i in 1:length(l.g)){
    rsaga.geoprocessor(
      lib="shapes_grid",
      module=2,
      param=list(GRIDS=file.path(RASTER.DIR,l.g[i]),
                 POLYGONS=paste(RU.DIR,RU.SHP,".shp",sep=""),
                 COUNT=0,
                 MIN=0,
                 MAX=0,
                 RANGE=0,
                 SUM=0,
                 VAR=0,
                 STDDEV=0,
                 QUANTILE=0,
                 NAMING=1),
      env=myenv)
    setTxtProgressBar(pb, i)
  }
}
