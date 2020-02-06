print("fTerrA: Calculation of Terrain Attributes using SAGA GIS")
print("-------------------------------------------------------------------------------")
#------------------------------------------------------------------------------- 
fTerrA <- function(DEM.DIR,
                  DEM,
                  DEM.FRM,
                  OUT.DIR,
                  MBI=TRUE,
                  RSP=TRUE,
                  NH=TRUE,
                  CI=TRUE,
                  TPI=TRUE,
                  ASC=TRUE,
                  ZS=TRUE){
  #------------------------------------------------------------------------------- 
  print("Import DEM and export to SAGA format")
  #------------------------------------------------------------------------------- 
  rsaga.geoprocessor(
    lib="io_grid",
    module=1,
    param=list(FILE=paste(DEM.DIR,DEM,DEM.FRM,sep=""),
               GRID=paste(OUT.DIR,DEM,".sgrd",sep="")),
    env=myenv)
  
  
  #------------------------------------------------------------------------------- 
  print("Aggregate")
  #------------------------------------------------------------------------------- 
  r <- raster(paste(DEM.DIR,DEM,DEM.FRM,sep=""))
  r <- aggregate(r,fact=AGGREGATE, fun=mean)
  writeRaster(r,paste(OUT.DIR,DEM,"_",AGGREGATE,sep=""),format="SAGA")
  #------------------------------------------------------------------------------- 
  print("Convert grid cells to polygons")
  #------------------------------------------------------------------------------- 
  rsaga.geoprocessor(
    lib="shapes_grid",
    module=6,
    param=list(GRID=paste(OUT.DIR,DEM,"_",AGGREGATE,".sgrd",sep=""),
               POLYGONS=paste(OUT.DIR,DEM,"_",AGGREGATE,".shp",sep="")),
    env=myenv)
  #------------------------------------------------------------------------------- 
  print("Filling sinks")
  #-------------------------------------------------------------------------------   
  rsaga.geoprocessor(
    lib="ta_preprocessor",
    module=3,
    param=list(DEM=paste(OUT.DIR,DEM,".sgrd",sep=""),
               RESULT=paste(OUT.DIR,DEM,"_FILL.sgrd",sep="")),
    env=myenv)
  #------------------------------------------------------------------------------- 
  print("Compound analysis: CI, TWI, VDC, RSP")
  #-------------------------------------------------------------------------------
  rsaga.geoprocessor(
    lib="ta_compound",
    module=0,
    param=list(ELEVATION=paste(OUT.DIR,DEM,"_FILL.sgrd",sep=""),
               SHADE=paste(OUT.DIR,DEM,"_SHD.sgrd",sep=""),
               CONVERGENCE=paste(OUT.DIR,DEM,"_CI.sgrd",sep=""),
               WETNESS=paste(OUT.DIR,DEM,"_TWI.sgrd",sep=""),
               CHNL_DIST=paste(OUT.DIR,DEM,"_VDC.sgrd",sep=""),
               RSP=paste(OUT.DIR,DEM,"_RSP.sgrd",sep="")),
    env=myenv)
  #reciprocal transformation
  rsaga.grid.calculus(c(paste(OUT.DIR,DEM,"_TWI.sgrd",sep="")), paste(OUT.DIR,DEM,"_TWI10.sgrd",sep=""), ~(a/(a+10)),env=myenv)
  rsaga.grid.calculus(c(paste(OUT.DIR,DEM,"_VDC.sgrd",sep="")), paste(OUT.DIR,DEM,"_VDC10.sgrd",sep=""), ~(a/(a+10)),env=myenv)
  #------------------------------------------------------------------------------- 
  print("Slope calculation (degrees)")
  #-------------------------------------------------------------------------------
  rsaga.geoprocessor(lib="ta_morphometry",
                     module=0,
                     param=list(ELEVATION=paste(OUT.DIR,DEM,"_FILL.sgrd",sep=""),
                                SLOPE=paste(OUT.DIR,DEM,"_SLP.sgrd",sep=""),
                                METHOD=6,
                                UNIT_SLOPE=1),
                     env=myenv)
  rsaga.grid.calculus(c(paste(OUT.DIR,DEM,"_SLP.sgrd",sep="")), paste(OUT.DIR,DEM,"_SLP10.sgrd",sep=""), ~(a/(a+10)),env=myenv)
  #------------------------------------------------------------------------------- 
  print("TCI")
  #-------------------------------------------------------------------------------
  rsaga.geoprocessor(
    lib="ta_hydrology",
    module=24, 
    param=list(DISTANCE=paste(OUT.DIR,DEM,"_VDC.sgrd",sep=""),
               TWI=paste(OUT.DIR,DEM,"_TWI.sgrd",sep=""),
               TCILOW=paste(OUT.DIR,DEM,"_TCI.sgrd",sep="")),
    env=myenv) 
  #------------------------------------------------------------------------------- 
  print("Calculation of Mass Balance Index (MBI) variants")
  #-------------------------------------------------------------------------------
  if(MBI==TRUE){
  P.MBI <- c(0.0001,0.0006,0.001,0.003,0.006,0.001)
  sink(paste(OUT.DIR,"P_MBI.txt",sep=""))
  print(P.MBI)
  sink()
  pb <- txtProgressBar(min=0, max=length(P.MBI), style=3)  
  for(i in 1:length(P.MBI)){
    rsaga.geoprocessor(
      lib="ta_morphometry",
      module=10,
      param=list(DEM=paste(OUT.DIR,DEM,"_FILL.sgrd",sep=""),
                 HREL=paste(OUT.DIR,DEM,"_VDC.sgrd",sep=""),
                 MBI=c(paste(OUT.DIR,DEM,'_MBI',i,c(".sgrd"),sep="")),#ouput
                 TSLOPE=10,
                 TCURVE=P.MBI[i]),#transfer variables
      env=myenv)
    #mbi histogram visualization
    #random sampling of mbi points and density plot
    rsaga.geoprocessor(
      lib="shapes_grid", 
      module=4, 
      param=list(GRID=c(paste(OUT.DIR,DEM,'_MBI',i,c(".sgrd"),sep="")), 
                 FREQ=50, 
                 POINTS=paste(OUT.DIR,DEM,"_MBI_point",sep="")),
      env=myenv)
    
    mbi.p <- shapefile(paste(OUT.DIR,DEM,"_MBI_point.shp",sep=""))
    head(mbi.p@data)
    
    pdf(paste(OUT.DIR,DEM,'_MBI',i,c(".pdf"),sep=""),width=4,height=4)
    plot(density(mbi.p$VALUE, from=-2, to=2,na.rm=TRUE),
         main="", xlab=paste(DEM,'_MBI',i,sep=""))
    dev.off()
    setTxtProgressBar(pb, i)
  }
  }
#-------------------------------------------------------------------------------
print("calculation of Relative Slope Position (RSP) variants")
#-------------------------------------------------------------------------------
  if(RSP==TRUE){
  for(i in round(lseq(2,20,5),0)){
    rsaga.geoprocessor(
      lib="ta_morphometry", 
      module=14, 
      param=list(DEM=paste(OUT.DIR,DEM,"_FILL.sgrd",sep=""), 
                 NH=paste(OUT.DIR,DEM,"_NH",i,c(".sgrd"),sep=""),
                 W=0.5,
                 T=i,
                 E=2),
      env=myenv)
  }
  }
#-------------------------------------------------------------------------------
print("Calculation of Topographic position Index (TPI) variants")
#-------------------------------------------------------------------------------
  if(TPI==TRUE){
  for(i in round(lseq(20,1000,5),0)){
    rsaga.geoprocessor(lib="ta_morphometry",
                       module=18,
                       param=list(DEM=paste(OUT.DIR,DEM,"_FILL.sgrd",sep=""),
                                  TPI=paste(OUT.DIR,DEM,"_TPI",i,c(".sgrd"),sep=""),
                                  RADIUS_MIN=0,
                                  RADIUS_MAX=i),
                       env=myenv)
  }
  }
#-------------------------------------------------------------------------------
print("Calculation of Convergence Index (CI) variants")
#-------------------------------------------------------------------------------
  if(CI==TRUE){  
  for(i in round(lseq(1,50,9),0)){
    rsaga.geoprocessor(
      lib="ta_morphometry", 
      module=2, 
      param=list(ELEVATION=paste(OUT.DIR,DEM,"_FILL.sgrd",sep=""), 
                 CONVERGENCE=c(paste(OUT.DIR,DEM,"_CI",i,c(".sgrd"),sep="")),
                 RADIUS=i),
      env=myenv)
  }
  }
  
#-------------------------------------------------------------------------------
print("Calculation of Openness")
#-------------------------------------------------------------------------------
  rsaga.geoprocessor(
    lib="ta_lighting", 
    module=5, 
    param=list(DEM=paste(OUT.DIR,DEM,"_FILL.sgrd",sep=""), 
               POS=c(paste(OUT.DIR,DEM,"_TOP",c(".sgrd"),sep="")),
               NEG=c(paste(OUT.DIR,DEM,"_TON",c(".sgrd"),sep="")),
               METHOD=0),
    env=myenv)
#------------------------------------------------------------------------------- 
print("asc export abnd zonal statistics")
#-------------------------------------------------------------------------------

  #export names of terrain attributes
  setwd(file.path(OUT.DIR))
  l.g <- mixedsort(list.files(pattern=paste("^(",DEM,").*\\.sgrd$",sep="")),decreasing=TRUE)
  l.g.df <- data.frame(ID=1:length(l.g),NAME=l.g)
  write.table(l.g.df,
              file=paste(OUT.DIR,"names_TerrainAttributes.txt",sep=""),
              sep=";",
              row.names = FALSE)
    #progress bar
  
  
  
  if(ASC==TRUE){
  for(j in 1:length(l.g)){
    pb <- txtProgressBar(min=1, max=length(l.g), style=3)
    rsaga.sgrd.to.esri(in.sgrds=paste(OUT.DIR,l.g[j],sep=""), 
                       out.grids=c(paste('TA',j,c(".asc"),sep="")), 
                       prec=3, 
                       out.path=getwd(),
                       env=myenv)
  }
  }
  if(ZS==TRUE){
    for(j in 1:length(l.g)){
      pb <- txtProgressBar(min=1, max=length(l.g), style=3)
    rsaga.geoprocessor(
      lib="shapes_grid",
      module=2,
      param=list(GRIDS=paste(OUT.DIR,l.g[j],sep=""),
                 POLYGONS=paste(OUT.DIR,DEM,"_",AGGREGATE,".shp",sep=""),
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
    setTxtProgressBar(pb, j)
    } 
  }
}
  