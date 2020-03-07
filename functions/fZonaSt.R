#######################################################################################################
#######################################################################################################
#######################################################################################################
print("Function for zonal statistics")
#######################################################################################################
#######################################################################################################
#######################################################################################################
fZonaSt <- function(TA.DIR,
                    TA,
                    POLYGON.DIR,
                    POLYGON.SHP,
                    OUT.DIR){
  setwd(file.path(POLYGON.DIR))
  #Import reference units)
  o <- st_read(paste(POLYGON.SHP,".shp",sep=""))
  o$ID <- 1:nrow(o)
  write_sf(o, paste(POLYGON.SHP,"_",TA,".shp",sep=""), delete_layer = TRUE)
  #names of original attributes shape file
  st_geometry(o) <- NULL
  names.s <- names(o)
  #-----------------------------------------------------------------------------------------------------
  print("Zonal statistic")
  #-----------------------------------------------------------------------------------------------------
  setwd(TA.DIR)
  l.g <- mixedsort(list.files(pattern=paste("^(",TA,").*\\.sgrd$",sep="")),decreasing=FALSE)
  #loop
  pb <- txtProgressBar(min=0, max=length(l.g), style=3)
  for (i in 1:length(l.g)){
    rsaga.geoprocessor(
      lib="shapes_grid",
      module=2,
      param=list(GRIDS=file.path(TA.DIR,l.g[i]),
                 POLYGONS=paste(POLYGON.DIR,POLYGON.SHP,"_",TA,".shp",sep=""),
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
 #-----------------------------------------------------------------------------------------------------
 print("Import and rename attributed shape file attributes")
 #-----------------------------------------------------------------------------------------------------
 o <- st_read(paste(OUT.DIR,POLYGON.SHP,"_",TA,".shp",sep=""))
 colnames(o) <- c(names.s,paste(str_sub(l.g, 1,-6)),paste("geometry"))
 o$ID <- 1:nrow(o)
 write_sf(o, paste(OUT.DIR,POLYGON.SHP,"_",TA,".shp",sep=""), delete_layer = TRUE)
 
 #------------------------------------------------------------------------------------------------------
 #Correlation matrix
 #------------------------------------------------------------------------------------------------------
 st_geometry(o) <- NULL
 write.csv2(cor(as.matrix(o[grepl(paste(TA,sep=""), names(o))])),
            file=paste(OUT.DIR,POLYGON.SHP,"_",TA,"_CorMatr.csv",sep=""))
 }
