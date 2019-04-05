print("fTerrA: Calculation of Terrain Attributes using SAGA GIS")
print("-------------------------------------------------------------------------------")
#------------------------------------------------------------------------------- 
fTaSA <- function(W.DIR,
                 IN.DIR,
                 DEM,
                 FB,
                 OUT.DIR){
#------------------------------------------------------------------------------- 
print("Import DEM and export to SAGA format")
#------------------------------------------------------------------------------- 
rsaga.esri.to.sgrd(
  in.grids=paste(W.DIR,IN.DIR,DEM,".asc",sep=""), 
  out.sgrds=paste(W.DIR,OUT.DIR,DEM,".sgrd",sep=""), 
  env=myenv)
#------------------------------------------------------------------------------- 
print("Filling sinks")
#-------------------------------------------------------------------------------   
rsaga.geoprocessor(
  lib="ta_preprocessor",
  module=3,
  param=list(DEM=paste(W.DIR,OUT.DIR,DEM,".sgrd",sep=""),
             RESULT=paste(W.DIR,OUT.DIR,DEM,"_FILL.sgrd",sep="")),
  env=myenv)
#------------------------------------------------------------------------------- 
print("Compound analysis: CI, TWI, VDC, RSP")
#-------------------------------------------------------------------------------
rsaga.geoprocessor(
  lib="ta_compound",
  module=0,
  param=list(ELEVATION=paste(W.DIR,OUT.DIR,DEM,"_FILL.sgrd",sep=""),
             SHADE=paste(W.DIR,OUT.DIR,DEM,"_SHD.sgrd",sep=""),
             CONVERGENCE=paste(W.DIR,OUT.DIR,DEM,"_CI.sgrd",sep=""),
             WETNESS=paste(W.DIR,OUT.DIR,DEM,"_TWI.sgrd",sep=""),
             CHNL_DIST=paste(W.DIR,OUT.DIR,DEM,"_VDC.sgrd",sep=""),
             RSP=paste(W.DIR,OUT.DIR,DEM,"_RSP.sgrd",sep="")),
  env=myenv)
  #reciprocal transformation
rsaga.grid.calculus(c(paste(W.DIR,OUT.DIR,DEM,"_TWI.sgrd",sep="")), 
                    paste(W.DIR,OUT.DIR,DEM,"_TWI_T10.sgrd",sep=""), ~(a/(a+10)),
                    env=myenv)
rsaga.grid.calculus(c(paste(W.DIR,OUT.DIR,DEM,"_VDC.sgrd",sep="")), 
                    paste(W.DIR,OUT.DIR,DEM,"_VDC_T10.sgrd",sep=""), ~(a/(a+10)),
                    env=myenv)
#------------------------------------------------------------------------------- 
print("SLP (degrees)")
#-------------------------------------------------------------------------------
rsaga.geoprocessor(lib="ta_morphometry",
                   module=0,
                   param=list(ELEVATION=paste(W.DIR,OUT.DIR,DEM,"_FILL.sgrd",sep=""),
                              SLOPE=paste(W.DIR,OUT.DIR,DEM,"_SLP.sgrd",sep=""),
                              METHOD=6,
                              UNIT_SLOPE=1),
                   env=myenv) 
#reciprocal transformation
rsaga.grid.calculus(c(paste(W.DIR,OUT.DIR,DEM,"_SLP.sgrd",sep="")), 
                    paste(W.DIR,OUT.DIR,DEM,"_SLP_T10.sgrd",sep=""), ~(a/(a+10)),
                    env=myenv)
#------------------------------------------------------------------------------- 
print("TCI")
#-------------------------------------------------------------------------------
rsaga.geoprocessor(
  lib="ta_hydrology",
  module=24, 
  param=list(DISTANCE=paste(W.DIR,OUT.DIR,DEM,"_VDC.sgrd",sep=""),
             TWI=paste(W.DIR,OUT.DIR,DEM,"_TWI.sgrd",sep=""),
             TCILOW=paste(W.DIR,OUT.DIR,DEM,"_TCI.sgrd",sep="")),
  env=myenv)
#------------------------------------------------------------------------------- 
print("Field-based LS factor")
#-------------------------------------------------------------------------------
rsaga.geoprocessor(
  lib="ta_hydrology",
  module=25, 
  param=list(DEM=paste(W.DIR,OUT.DIR,DEM,"_FILL.sgrd",sep=""),
             FIELDS=paste(W.DIR,IN.DIR,FB,sep=""),
             UPSLOPE_AREA=paste(paste(W.DIR,OUT.DIR,DEM,"_FB-UA.sgrd",sep="")),
             UPSLOPE_LENGTH=paste(paste(W.DIR,OUT.DIR,DEM,"_FB-UL.sgrd",sep="")),
             UPSLOPE_SLOPE=paste(paste(W.DIR,OUT.DIR,DEM,"_FB-US.sgrd",sep="")),
             LS_FACTOR=paste(W.DIR,OUT.DIR,DEM,"_FB-LS.sgrd",sep=""),
             BALANCE=paste(W.DIR,OUT.DIR,DEM,"_BLC.sgrd",sep=""),
             METHOD=1,
             METHOD_AREA=3,
             METHOD_SLOPE=1),
  env=myenv)
#UPSLOPE_LENGTH=UL
#UPSLOPE_SLOPE=US
#------------------------------------------------------------------------------- 
print("Calculation of MBI variants")
#-------------------------------------------------------------------------------
P.MBI <- c(0.0001,0.0006,0.001,0.003,0.006,0.001)
sink(paste(W.DIR,OUT.DIR,"P_MBI.txt",sep=""))
print(P.MBI)
sink()
pb <- txtProgressBar(min=0, max=length(P.MBI), style=3)  
for(i in 1:length(P.MBI)){
  rsaga.geoprocessor(
    lib="ta_morphometry",
    module=10,
    param=list(DEM=paste(W.DIR,OUT.DIR,DEM,"_FILL.sgrd",sep=""),
               HREL=paste(W.DIR,OUT.DIR,DEM,"_VDC.sgrd",sep=""),
               MBI=c(paste(W.DIR,OUT.DIR,DEM,'_MBI',i,c(".sgrd"),sep="")),#ouput
               TSLOPE=10,
               TCURVE=P.MBI[i]),#transfer variables
    env=myenv)
  #mbi histogram visualization
  #random sampling of mbi points and density plot
  rsaga.geoprocessor(
    lib="shapes_grid", 
    module=4, 
    param=list(GRID=c(paste(W.DIR,OUT.DIR,DEM,'_MBI',i,c(".sgrd"),sep="")), 
               FREQ=50, 
               POINTS=paste(W.DIR,OUT.DIR,DEM,"_MBI_point",sep="")),
    env=myenv)
  
  mbi.p <- shapefile(paste(W.DIR,OUT.DIR,DEM,"_MBI_point.shp",sep=""))
  head(mbi.p@data)
  
  pdf(paste(W.DIR,OUT.DIR,DEM,'_MBI',i,c(".pdf"),sep=""),width=4,height=4)
  plot(density(mbi.p$VALUE, from=-2, to=2,na.rm=TRUE),
       main="", xlab=paste(DEM,'_MBI',i,sep=""))
  dev.off()
  setTxtProgressBar(pb, i)
}
#------------------------------------------------------------------------------- 
print("asc export")
#-------------------------------------------------------------------------------
setwd(file.path(W.DIR,OUT.DIR))
l.g <- mixedsort(list.files(pattern=paste("^(",DEM,").*\\.sgrd$",sep="")),decreasing=TRUE)
print(l.g)
pb <- txtProgressBar(min=1, max=length(l.g), style=3)
for(i in 1:length(l.g)){
  rsaga.sgrd.to.esri(in.sgrds=paste(W.DIR,OUT.DIR,l.g[i],sep=""), 
                   out.grids=paste(W.DIR,OUT.DIR,substr(l.g[i],1,nchar(l.g[i])-5),".asc",sep=""), 
                   prec=3,
                   env=myenv)
  setTxtProgressBar(pb, i)
}
}
