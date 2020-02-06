#######################################################################################################
#######################################################################################################
#######################################################################################################
print("Function for zonal statistics")
#######################################################################################################
#######################################################################################################
#######################################################################################################
ZonalStat <- function(RASTER.DIR,
                      SHAPE.DIR,
                      SHAPE.FILE,
                      OUT.DIR){
  #names of original attributes shape file
  names.s <- names(read.dbf(paste(SHAPE.DIR,substr(SHAPE.FILE,1,nchar(SHAPE.FILE)-4),".dbf",sep=""))$dbf)
  #-----------------------------------------------------------------------------------------------------
  print("Zonal statistic")
  #-----------------------------------------------------------------------------------------------------
  setwd(file.path(OUT.DIR))
  l.g <- mixedsort(list.files(pattern=paste("^(",DEM,").*\\.sgrd$",sep="")),decreasing=TRUE)
  l.g.df <- data.frame(ID=1:length(l.g),NAME=l.g)
  write.table(l.g.df,
              file=paste(OUT.DIR,"names_TerrainAttributes.txt",sep=""),
              sep=";",
              row.names = FALSE)
  
  
  
  
  setwd(file.path(W.DIR,RASTER.DIR))
  l.r <- mixedsort(list.files(pattern=paste("*.",RASTER.FORMAT,"$",sep="")))
  write.table(l.r,"terrainattributes.txt")
  #loop
  pb <- txtProgressBar(min=0, max=length(l.r), style=3)
  for (i in 1:length(l.r)){
    #import
    rsaga.esri.to.sgrd(
      in.grids=l.r[i], 
      out.sgrds=paste(substr(l.r[i],0,nchar(l.r[i])-4),c(".sgrd"),sep=""), 
      in.path=getwd(),env=myenv)
    rsaga.geoprocessor(
      lib="shapes_grid",
      module=2,
      param=list(GRIDS=file.path(W.DIR,RASTER.DIR,l.r[i]),
                 POLYGONS=paste(W.DIR,SHAPE.DIR,SHAPE.FILE,sep=""),
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
  #print("Import and rename shape file attributes")
  #shp <- shapefile(file.path(W.DIR,RESULT.DIR,SHAPE.FILE))
  #colnames(shp@data) <- c(names.s,substr(l.r,1,nchar(l.r)-4))
  ##filter
  #shp <-  shp[shp$GEOL!="1.#INF",]
  ##export
  #shapefile(shp, paste(W.DIR,RESULT.DIR,"TA_",SHAPE.FILE,sep=""), overwrite=TRUE)
}


