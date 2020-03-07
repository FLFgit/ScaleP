print("fTerrA: Calculation of Terrain Attributes using SAGA GIS")
print("-------------------------------------------------------------------------------")
#------------------------------------------------------------------------------- 
fTerrA <- function(DEM.DIR,
                  DEM,
                  DEM.FRM,
                  OUT.DIR,
                  TA,
                  TCI=TRUE,
                  MBI=TRUE,
                  NH=TRUE,
                  TPI=TRUE,
                  VECTOR=TRUE,
                  MRVBF=TRUE,
                  AGGREGATE,
                  EPSG){
  #------------------------------------------------------------------------------- 
  #print("Import DEM and export to SAGA format")
  #------------------------------------------------------------------------------- 
  #rsaga.geoprocessor(
  #  lib="io_grid",
  #  module=1,
  #  param=list(FILE=paste(DEM.DIR,DEM,DEM.FRM,sep=""),
  #             GRID=paste(OUT.DIR,DEM,".sgrd",sep="")),
  #  env=myenv)

#------------------------------------------------------------------------------- 
print("1 | Import DEM and aggregate cell size")
#------------------------------------------------------------------------------- 
  r <- raster(paste(DEM.DIR,DEM,DEM.FRM,sep=""))
  #r[r < 0] <- NA
  plot(r)
  r <- aggregate(r,fact=AGGREGATE, fun=mean)
  crs(r) <- CRS(paste("+init=EPSG:",EPSG,sep=""))
  writeRaster(r,paste(OUT.DIR,DEM,"_AGGREGATE",AGGREGATE,sep=""),format="SAGA",overwrite=TRUE)

if(VECTOR==TRUE){
#------------------------------------------------------------------------------- 
print("2 | Convert aggregated grid cells to polygons")
#------------------------------------------------------------------------------- 
  rsaga.geoprocessor(
    lib="shapes_grid",
    module=6,
    param=list(GRID=paste(OUT.DIR,DEM,"_AGGREGATE",AGGREGATE,".sgrd",sep=""),
               POLYGONS=paste(OUT.DIR,DEM,"_AGGREGATE",AGGREGATE,".shp",sep="")),
    env=myenv)
}
#------------------------------------------------------------------------------- 
print("3 | Filling sinks")
#-------------------------------------------------------------------------------   
  rsaga.geoprocessor(
    lib="ta_preprocessor",
    module=3,
    param=list(DEM=paste(OUT.DIR,DEM,"_AGGREGATE",AGGREGATE,".sgrd",sep=""),
               RESULT=paste(OUT.DIR,DEM,"_AGGREGATE",AGGREGATE,"_FILL.sgrd",sep="")),
    env=myenv)
  
  
#-------------------------------------------------------------------------------
print("4 | Calculation of Shaded Relief and Openness Index")
#-------------------------------------------------------------------------------
rsaga.geoprocessor(
    lib="ta_lighting", 
    module=0, 
    param=list(ELEVATION=paste(OUT.DIR,DEM,"_AGGREGATE",AGGREGATE,"_FILL.sgrd",sep=""), 
               SHADE=c(paste(OUT.DIR,DEM,"_AGGREGATE",AGGREGATE,"_SHD",c(".sgrd"),sep="")),
               EXAGGERATION=20),
    env=myenv)  
  
rsaga.geoprocessor(
  lib="ta_lighting", 
  module=5, 
  param=list(DEM=paste(OUT.DIR,DEM,"_AGGREGATE",AGGREGATE,"_FILL.sgrd",sep=""), 
             POS=c(paste(OUT.DIR,DEM,"_AGGREGATE",AGGREGATE,"_TOP",c(".sgrd"),sep="")),
             NEG=c(paste(OUT.DIR,DEM,"_AGGREGATE",AGGREGATE,"_TON",c(".sgrd"),sep="")),
             METHOD=0),
  env=myenv)  

#------------------------------------------------------------------------------- 
print("5 | Slope calculation (degrees)")
#-------------------------------------------------------------------------------
rsaga.geoprocessor(lib="ta_morphometry",
                   module=0,
                   param=list(ELEVATION=paste(OUT.DIR,DEM,"_AGGREGATE",AGGREGATE,"_FILL.sgrd",sep=""),
                              SLOPE=paste(OUT.DIR,DEM,"_AGGREGATE",AGGREGATE,"_SLP.sgrd",sep=""),
                              METHOD=6,
                              UNIT_SLOPE=1),
                   env=myenv)

#-------------------------------------------------------------------------------            
print("6 | Calculation of SAGA Topographic Wetnessindex (TWI)")
#-------------------------------------------------------------------------------
  rsaga.geoprocessor(
    lib="ta_hydrology", 
    module=15, 
    param=list(DEM=paste(OUT.DIR,DEM,"_AGGREGATE",AGGREGATE,"_FILL.sgrd",sep=""),
               TWI=paste(OUT.DIR,DEM,"_AGGREGATE",AGGREGATE,"_TWI.sgrd",sep="")),
    env=myenv)

if(TCI==TRUE){
#-------------------------------------------------------------------------------            
print("7 | Calculation of variants of vertical distance to channel network (VDC) 
           and of terrain classification index (TCI)")
#-------------------------------------------------------------------------------
 print("5-1 | Calculation of flow accumulation")
  rsaga.geoprocessor(lib="ta_hydrology",
                     module=0, 
                     param=list(ELEVATION=paste(OUT.DIR,DEM,"_AGGREGATE",AGGREGATE,"_FILL.sgrd",sep=""),
                                CAREA=paste(OUT.DIR,DEM,"_AGGREGATE",AGGREGATE,"_CA.sgrd",sep=""),
                                METHOD=4),
                     env=myenv)
  
  print("5-2 | Calculation of channel network variants")
  P.CA <- round(lseq(10000,1000000,10),0)
  pb <- txtProgressBar(min=1, max=length(P.CA), style=3) 
  for(i in 1:length(P.CA)){
    print(paste("Loop ",i,"/",length(P.CA),sep=""))
    
    rsaga.geoprocessor("ta_channels",0,list(ELEVATION=paste(OUT.DIR,DEM,"_AGGREGATE",AGGREGATE,"_FILL.sgrd",sep=""),
                                            SHAPES=paste(OUT.DIR,DEM,"_AGGREGATE",AGGREGATE,"_channel-network_TH",P.CA[i],c(".shp"),sep=""),
                                            INIT_GRID=paste(OUT.DIR,DEM,"_AGGREGATE",AGGREGATE,"_CA.sgrd",sep=""),
                                            INIT_METHOD=2,
                                            INIT_VALUE=P.CA[i],
                                            DIV_CELLS=100,
                                            MINLEN=2),
                       env=myenv)

    print("5-3 | Add elevation grid Values to Shapes and rename elevation column name")
    rsaga.geoprocessor("shapes_grid",1,list(GRIDS=paste(OUT.DIR,DEM,"_AGGREGATE",AGGREGATE,"_FILL.sgrd",sep=""),
                                            SHAPES=paste(OUT.DIR,DEM,"_AGGREGATE",AGGREGATE,"_channel-network_TH",P.CA[i],c(".shp"),sep="")),
                       env=myenv)
    
    #Rename elevation column name:
    s <- shapefile(paste(OUT.DIR,DEM,"_AGGREGATE",AGGREGATE,"_channel-network_TH",P.CA[i],c(".shp"),sep=""))
    colnames(s@data) <- paste(c(names(s@data)[-length(s@data)],"E"))
    
    #Delete columns with elevation smaller 0:
    s <- s[which(s@data$E>=0),]
    shapefile(s,paste(OUT.DIR,DEM,"_AGGREGATE",AGGREGATE,"_channel-network_TH",P.CA[i],c(".shp"),sep=""),overwrite=TRUE)
    
    print("5-4 | Thin spline interpolation of elevation base level")  
    rsaga.geoprocessor("grid_spline",1,list(SHAPES=paste(OUT.DIR,DEM,"_AGGREGATE",AGGREGATE,"_channel-network_TH",P.CA[i],c(".shp"),sep=""),
                                            FIELD="E",
                                            TARGET_DEFINITION=1,
                                            TARGET_TEMPLATE=paste(OUT.DIR,DEM,"_AGGREGATE",AGGREGATE,"_FILL.sgrd",sep=""),
                                            TARGET_OUT_GRID=paste(OUT.DIR,DEM,"_AGGREGATE",AGGREGATE,"_BL_TH",P.CA[i],c(".sgrd"),sep=""),
                                            SEARCH_RANGE=1,
                                            SEARCH_POINTS_MAX=50,
                                            SEARCH_DIRECTION=0),
                       env=myenv)
    
    print("5-5 | VDC calculation")  
    rsaga.grid.calculus(c(paste(OUT.DIR,DEM,"_AGGREGATE",AGGREGATE,"_FILL.sgrd",sep=""), 
                          paste(OUT.DIR,DEM,"_AGGREGATE",AGGREGATE,"_BL_TH",P.CA[i],c(".sgrd"),sep="")), 
                          paste(OUT.DIR,DEM,"_AGGREGATE",AGGREGATE,"_VDC_TH",P.CA[i],c(".sgrd"),sep=""), 
                          ~(a-b),
                        env=myenv)
    
    #TCI calculation
    rsaga.geoprocessor(lib="ta_hydrology",
                       module=24, 
                       param=list(DISTANCE=paste(OUT.DIR,DEM,"_AGGREGATE",AGGREGATE,"_VDC_TH",P.CA[i],c(".sgrd"),sep=""),
                                  TWI=paste(OUT.DIR,DEM,"_AGGREGATE",AGGREGATE,"_TWI.sgrd",sep=""),
                                  TCILOW=paste(OUT.DIR,DEM,"_AGGREGATE",AGGREGATE,"_TCI_TH",P.CA[i],c(".sgrd"),sep="")),
                       env=myenv)
    setTxtProgressBar(pb, i)
  }
}


if(MBI==TRUE){
#------------------------------------------------------------------------------- 
print("8 | Calculation of Mass Balance Index (MBI) variants")
#-------------------------------------------------------------------------------
  P.MBI <- round(lseq(0.0001,0.1,10),4)
  sink(paste(OUT.DIR,"P_MBI.txt",sep=""))
  print(P.MBI)
  sink()
  pb <- txtProgressBar(min=1, max=length(P.MBI), style=3)  
  for(i in 1:length(P.MBI)){
    rsaga.geoprocessor(
      lib="ta_morphometry",
      module=10,
      param=list(DEM=paste(OUT.DIR,DEM,"_AGGREGATE",AGGREGATE,"_FILL.sgrd",sep=""),
                 #HREL=paste(OUT.DIR,DEM,"_AGGREGATE",AGGREGATE,"_VDC.sgrd",sep=""),
                 MBI=c(paste(OUT.DIR,DEM,"_AGGREGATE",AGGREGATE,'_MBI',i,c(".sgrd"),sep="")),#ouput
                 TSLOPE=10,
                 TCURVE=P.MBI[i]),#transfer variables
      env=myenv)
    #mbi histogram visualization
    #random sampling of mbi points and density plot
    rsaga.geoprocessor(
      lib="shapes_grid", 
      module=4, 
      param=list(GRID=c(paste(OUT.DIR,DEM,"_AGGREGATE",AGGREGATE,'_MBI',i,c(".sgrd"),sep="")), 
                 FREQ=50, 
                 POINTS=paste(OUT.DIR,DEM,"_AGGREGATE",AGGREGATE,"_MBI_point",sep="")),
      env=myenv)
    
    mbi.p <- shapefile(paste(OUT.DIR,DEM,"_AGGREGATE",AGGREGATE,"_MBI_point.shp",sep=""))
    head(mbi.p@data)
    
    pdf(paste(OUT.DIR,DEM,"_AGGREGATE",AGGREGATE,'_MBI',i,c(".pdf"),sep=""),width=4,height=4)
    plot(density(mbi.p$VALUE, from=-2, to=2,na.rm=TRUE),
         main="", xlab=paste(DEM,"_AGGREGATE",AGGREGATE,'_MBI',i,sep=""))
    dev.off()
    setTxtProgressBar(pb, i)
  }
  }

if(NH==TRUE){
#-------------------------------------------------------------------------------
print("9 | Calculation of Normalized Hight (NH) variants")
#-------------------------------------------------------------------------------
  P.NH <- round(lseq(2,100,10),0)
  pb <- txtProgressBar(min=1, max=length(P.NH), style=3) 
  for(i in 1:length(P.NH)){
    rsaga.geoprocessor(
      lib="ta_morphometry", 
      module=14, 
      param=list(DEM=paste(OUT.DIR,DEM,"_AGGREGATE",AGGREGATE,"_FILL.sgrd",sep=""), 
                 NH=paste(OUT.DIR,DEM,"_AGGREGATE",AGGREGATE,"_RSP",P.NH[i],c(".sgrd"),sep=""),
                 W=0.5,
                 T=P.NH[i],
                 E=2),
      env=myenv)
  setTxtProgressBar(pb, i)
  }
  }

  
if(TPI==TRUE){
#-------------------------------------------------------------------------------
print("10 | Calculation of Topographic Position Index (TPI) variants")
#-------------------------------------------------------------------------------
  P.TPI <- round(lseq(20,2000,10),0)
  pb <- txtProgressBar(min=1, max=length(P.TPI), style=3) 
  for(i in 1:length(P.TPI)){
    rsaga.geoprocessor(lib="ta_morphometry",
                       module=18,
                       param=list(DEM=paste(OUT.DIR,DEM,"_AGGREGATE",AGGREGATE,"_FILL.sgrd",sep=""),
                                  TPI=paste(OUT.DIR,DEM,"_AGGREGATE",AGGREGATE,"_TPI",P.TPI[i],c(".sgrd"),sep=""),
                                  RADIUS_MIN=0,
                                  RADIUS_MAX=P.TPI[i]),
                       env=myenv)
  setTxtProgressBar(pb, i)  
  }
  }

#-------------------------------------------------------------------------------
print("11 | Calculation of Multi-Resolution Valley Bottom Flatness (MRVBF) index")
#-------------------------------------------------------------------------------
if(MRVBF==TRUE){
   rsaga.geoprocessor(
      lib="ta_morphometry", 
      module=8, 
      param=list(DEM=paste(OUT.DIR,DEM,"_AGGREGATE",AGGREGATE,"_FILL.sgrd",sep=""), 
                 MRVBF=c(paste(OUT.DIR,DEM,"_AGGREGATE",AGGREGATE,"_MRVBF.sgrd",sep="")),
                 MAX_RES=100),
      env=myenv)
  }

#------------------------------------------------------------------------------- 
print("rename files")
#-------------------------------------------------------------------------------
  #export names of terrain attributes
  setwd(file.path(OUT.DIR))
  l.g <- mixedsort(list.files(pattern=paste("^(",DEM,").*\\.sgrd$",sep="")),decreasing=FALSE)
  l.g.df <- data.frame(TA=1:length(l.g),NAME=l.g)
  write.table(l.g.df,
              file=paste(OUT.DIR,"names_TerrainAttributes.txt",sep=""),
              sep=";",
              row.names = FALSE)
  #rename terrain attributes
  for(j in 1:length(l.g)){
    file.rename(l.g[j], paste(TA,j,".sgrd",sep=""))
  }
  l.g <- mixedsort(list.files(pattern=paste("^(",DEM,").*\\.sdat$",sep="")),decreasing=FALSE)
  for(j in 1:length(l.g)){
    file.rename(l.g[j], paste(TA,j,".sdat",sep=""))
  }
  l.g <- mixedsort(list.files(pattern=paste("^(",DEM,").*\\.mgrd$",sep="")),decreasing=FALSE)
  for(j in 1:length(l.g)){
    file.rename(l.g[j], paste(TA,j,".mgrd",sep=""))
  }
  #if(ASC==TRUE){
  #for(j in 1:length(l.g)){
  #  pb <- txtProgressBar(min=1, max=length(l.g), style=3)
  #  rsaga.sgrd.to.esri(in.sgrds=paste(OUT.DIR,l.g[j],sep=""), 
  #                     out.grids=c(paste('TA',j,c(".asc"),sep="")), 
  #                     prec=3, 
  #                     out.path=getwd(),
  #                     env=myenv)
  #}
  #}
}
