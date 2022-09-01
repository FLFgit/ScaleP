print("fTerrA: Calculation of Terrain Attributes using SAGA GIS")
#########################################################################################################
##Libraries and functions
#########################################################################################################
loadandinstall <- function(mypkg) {
  for(i in seq(along=mypkg)){
    if (!is.element(mypkg[i],installed.packages()[,1])){install.packages(mypkg[i])}
    library(mypkg[i], character.only=TRUE)
  }
}
packages <- sort(c("caret",
                   "caretEnsemble",
                   "classInt",
                   "doParallel",
                   "EFS",
                   "foreign",
                   "fpc",
                   "gstat",
                   "gtools",
                   "lattice",
                   "maptools",
                   "raster",
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

##Function for the creation of exponetial values
lseq <- function(from, to, length.out) {
  # logarithmic spaced sequence
  # blatantly stolen from library("emdbook"), because need only this
  exp(seq(log(from), log(to), length.out = length.out))
}
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
fTerrA <- function(DEM.DIR,
                   DEM,
                   DEM.FRM,
                   OUT.DIR,
                   TA,
                   TCI=TRUE,
                   P.CA1,
                   P.CA2,
                   P.CA3,
                   MBI=TRUE,
                   P.MBI1,
                   P.MBI2,
                   P.MBI3,
                   NH=TRUE,
                   P.NH1,
                   P.NH2,
                   P.NH3,
                   OPN=TRUE,
                   P.OPN1,
                   P.OPN2,
                   P.OPN3,
                   TPI=TRUE,
                   P.TPI1,
                   P.TPI2,
                   P.TPI3,
                   EPSG){
  #------------------------------------------------------------------------------- 
  print("1 | Import DEM")
  #------------------------------------------------------------------------------- 
  rsaga.geoprocessor(
    lib="io_grid",
    module=1,
    param=list(FILE=paste(DEM.DIR,DEM,DEM.FRM,sep=""),
               GRID=paste(OUT.DIR,DEM,".sgrd",sep="")),
    env=myenv)
  
  #r <- raster(paste(DEM.DIR,DEM,DEM.FRM,sep=""))
  #r[r < 0] <- NA
  #plot(r)
  #r <- aggregate(r,fact=AGGREGATE, fun=mean)
  #crs(r) <- CRS(paste("+init=EPSG:",EPSG,sep=""))
  #writeRaster(r,paste(OUT.DIR,DEM,sep=""),format="SAGA",overwrite=TRUE)
  #------------------------------------------------------------------------------- 
  print("2 | Filling sinks")
  #-------------------------------------------------------------------------------   
  rsaga.geoprocessor(
    lib="ta_preprocessor",
    module=3,
    param=list(DEM=paste(OUT.DIR,DEM,".sgrd",sep=""),
               RESULT=paste(OUT.DIR,DEM,"_FILL.sgrd",sep="")),
    env=myenv)
  
  #-------------------------------------------------------------------------------
  print("3 | Calculation of Shaded Relief and Openness Index")
  #-------------------------------------------------------------------------------
  rsaga.geoprocessor(
    lib="ta_lighting", 
    module=0, 
    param=list(ELEVATION=paste(OUT.DIR,DEM,"_FILL.sgrd",sep=""), 
               SHADE=paste(OUT.DIR,DEM,"_SHD.sgrd",sep=""),
               EXAGGERATION=20),
    env=myenv)  
  
  if(OPN==TRUE){
  P.OPN <- round(lseq(P.OPN1,P.OPN2,P.OPN3),0)
  for(i in 1:length(P.OPN)){  
  rsaga.geoprocessor(
    lib="ta_lighting", 
    module=5, 
    param=list(DEM=paste(OUT.DIR,DEM,"_FILL.sgrd",sep=""), 
               POS=paste(OUT.DIR,DEM,"_TOP",P.OPN[i],c(".sgrd"),sep=""),
               NEG=paste(OUT.DIR,DEM,"_TON",P.OPN[i],c(".sgrd"),sep=""),
               METHOD=0,
               RADIUS=P.OPN[i]),
    env=myenv)  
  }
    
    
  TARGET_OUT_GRID=paste(OUT.DIR,DEM,"_BL_TH",P.CA[i],c(".sgrd"),sep=""),
  #------------------------------------------------------------------------------- 
  print("4 | Slope calculation (degrees)")
  #-------------------------------------------------------------------------------
  rsaga.geoprocessor(lib="ta_morphometry",
                     module=0,
                     param=list(ELEVATION=paste(OUT.DIR,DEM,"_FILL.sgrd",sep=""),
                                SLOPE=paste(OUT.DIR,DEM,"_SLP.sgrd",sep=""),
                                METHOD=6,
                                UNIT_SLOPE=1),
                     env=myenv)
  
  #-------------------------------------------------------------------------------            
  print("5 | Calculation of SAGA Topographic Wetnessindex (TWI)")
  #-------------------------------------------------------------------------------
  rsaga.geoprocessor(
    lib="ta_hydrology", 
    module=15, 
    param=list(DEM=paste(OUT.DIR,DEM,"_FILL.sgrd",sep=""),
               TWI=paste(OUT.DIR,DEM,"_TWI.sgrd",sep="")),
    env=myenv)
  
  if(TCI==TRUE){
    #-------------------------------------------------------------------------------            
    print("6 | Calculation of variants of vertical distance to channel network (VDC) 
           and of terrain classification index (TCI)")
    #-------------------------------------------------------------------------------
    print("6-1 | Calculation of flow accumulation")
    rsaga.geoprocessor(lib="ta_hydrology",
                       module=0, 
                       param=list(ELEVATION=paste(OUT.DIR,DEM,"_FILL.sgrd",sep=""),
                                  CAREA=paste(OUT.DIR,DEM,"_CA.sgrd",sep=""),
                                  METHOD=4),
                       env=myenv)
    
    print("6-2 | Calculation of channel network variants")
    P.CA <- round(lseq(P.CA1,P.CA2,P.CA3),0)
    sink(paste(OUT.DIR,"P_CA.txt",sep=""))
    print(P.CA)
    sink()
    
    pb <- txtProgressBar(min=1, max=length(P.CA), style=3) 
    for(i in 1:length(P.CA)){
      print(paste("Loop ",i,"/",length(P.CA),sep=""))
      
      rsaga.geoprocessor("ta_channels",0,list(ELEVATION=paste(OUT.DIR,DEM,"_FILL.sgrd",sep=""),
                                              SHAPES=paste(OUT.DIR,DEM,"_channel-network_TH",P.CA[i],c(".shp"),sep=""),
                                              INIT_GRID=paste(OUT.DIR,DEM,"_CA.sgrd",sep=""),
                                              INIT_METHOD=2,
                                              INIT_VALUE=P.CA[i],
                                              DIV_CELLS=100,
                                              MINLEN=2),
                         env=myenv)
      
      print("6-3 | Add elevation grid Values to Shapes and rename elevation column name")
      rsaga.geoprocessor("shapes_grid",1,list(GRIDS=paste(OUT.DIR,DEM,"_FILL.sgrd",sep=""),
                                              SHAPES=paste(OUT.DIR,DEM,"_channel-network_TH",P.CA[i],c(".shp"),sep="")),
                         env=myenv)
      
      #Rename elevation column name:
      s <- shapefile(paste(OUT.DIR,DEM,"_channel-network_TH",P.CA[i],c(".shp"),sep=""))
      colnames(s@data) <- paste(c(names(s@data)[-length(s@data)],"E"))
      
      #Delete columns with elevation smaller 0:
      s <- s[which(s@data$E>=0),]
      shapefile(s,paste(OUT.DIR,DEM,"_channel-network_TH",P.CA[i],c(".shp"),sep=""),overwrite=TRUE)
      
      print("6-4 | Thin spline interpolation of elevation base level")  
      rsaga.geoprocessor("grid_spline",1,list(SHAPES=paste(OUT.DIR,DEM,"_channel-network_TH",P.CA[i],c(".shp"),sep=""),
                                              FIELD="E",
                                              TARGET_DEFINITION=1,
                                              TARGET_TEMPLATE=paste(OUT.DIR,DEM,"_FILL.sgrd",sep=""),
                                              TARGET_OUT_GRID=paste(OUT.DIR,DEM,"_BL_TH",P.CA[i],c(".sgrd"),sep=""),
                                              SEARCH_RANGE=1,
                                              SEARCH_POINTS_MAX=50,
                                              SEARCH_DIRECTION=0),
                         env=myenv)
      
      print("6-5 | VDC calculation")  
      rsaga.grid.calculus(c(paste(OUT.DIR,DEM,"_FILL.sgrd",sep=""), 
                            paste(OUT.DIR,DEM,"_BL_TH",P.CA[i],c(".sgrd"),sep="")), 
                          paste(OUT.DIR,DEM,"_VDC_TH",P.CA[i],c(".sgrd"),sep=""), 
                          ~(a-b),
                          env=myenv)
      
      #TCI calculation
      rsaga.geoprocessor(lib="ta_hydrology",
                         module=24, 
                         param=list(DISTANCE=paste(OUT.DIR,DEM,"_VDC_TH",P.CA[i],c(".sgrd"),sep=""),
                                    TWI=paste(OUT.DIR,DEM,"_TWI.sgrd",sep=""),
                                    TCILOW=paste(OUT.DIR,DEM,"_TCI_TH",P.CA[i],c(".sgrd"),sep="")),
                         env=myenv)
      setTxtProgressBar(pb, i)
    }
  }
  
  
  if(MBI==TRUE){
    #------------------------------------------------------------------------------- 
    print("7 | Calculation of Mass Balance Index (MBI) variants")
    #-------------------------------------------------------------------------------
    P.MBI <- round(lseq(P.MBI1,P.MBI2,P.MBI3),4)
    sink(paste(OUT.DIR,"P_MBI.txt",sep=""))
    print(P.MBI)
    sink()
    pb <- txtProgressBar(min=1, max=length(P.MBI), style=3)  
    for(i in 1:length(P.MBI)){
      rsaga.geoprocessor(
        lib="ta_morphometry",
        module=10,
        param=list(DEM=paste(OUT.DIR,DEM,"_FILL.sgrd",sep=""),
                   #HREL=paste(OUT.DIR,DEM,"_VDC.sgrd",sep=""),
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
  
  if(NH==TRUE){
    #-------------------------------------------------------------------------------
    print("8 | Calculation of Normalized Hight (NH) variants")
    #-------------------------------------------------------------------------------
    P.NH <- round(lseq(P.NH1,P.NH2,P.NH3),0)
    sink(paste(OUT.DIR,"P_NH.txt",sep=""))
    print(P.NH)
    sink()
    
    pb <- txtProgressBar(min=1, max=length(P.NH), style=3) 
    for(i in 1:length(P.NH)){
      rsaga.geoprocessor(
        lib="ta_morphometry", 
        module=14, 
        param=list(DEM=paste(OUT.DIR,DEM,"_FILL.sgrd",sep=""), 
                   NH=paste(OUT.DIR,DEM,"_NH",P.NH[i],c(".sgrd"),sep=""),
                   W=0.5,
                   T=P.NH[i],
                   E=2),
        env=myenv)
      setTxtProgressBar(pb, i)
    }
  }
  
  
  if(TPI==TRUE){
    #-------------------------------------------------------------------------------
    print("9 | Calculation of Topographic Position Index (TPI) variants")
    #-------------------------------------------------------------------------------
    P.TPI <- round(lseq(P.TPI1,P.TPI2,P.TPI3),0)
    pb <- txtProgressBar(min=1, max=length(P.TPI), style=3) 
    for(i in 1:length(P.TPI)){
      rsaga.geoprocessor(lib="ta_morphometry",
                         module=18,
                         param=list(DEM=paste(OUT.DIR,DEM,"_FILL.sgrd",sep=""),
                                    TPI=paste(OUT.DIR,DEM,"_TPI",P.TPI[i],c(".sgrd"),sep=""),
                                    RADIUS_MIN=0,
                                    RADIUS_MAX=P.TPI[i]),
                         env=myenv)
      setTxtProgressBar(pb, i)  
    }
  }
  
  #-------------------------------------------------------------------------------
  #print("11 | Calculation of Multi-Resolution Valley Bottom Flatness (MRVBF) index")
  #-------------------------------------------------------------------------------
  #if(MRVBF==TRUE){
  #   rsaga.geoprocessor(
  #      lib="ta_morphometry", 
  #      module=8, 
  #      param=list(DEM=paste(OUT.DIR,DEM,"_FILL.sgrd",sep=""), 
  #                 MRVBF=c(paste(OUT.DIR,DEM,"_MRVBF.sgrd",sep="")),
  #                 MAX_RES=1000),
  #      env=myenv)
  #  }
  
  #------------------------------------------------------------------------------- 
  print("rename files")
  #-------------------------------------------------------------------------------
  #export names of terrain attributes
  setwd(file.path(OUT.DIR))
  l.g <- mixedsort(list.files(pattern=paste("^(",DEM,").*\\.sgrd$",sep="")),decreasing=FALSE)
  l.g
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
