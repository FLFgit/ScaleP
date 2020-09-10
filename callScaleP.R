print("Functions for the [Scale]-specific [P]rediction of soil classes and numeric parameters")
#-----------------------------------------------------------------------------------------------------
#General packages
#-------------------------------------------------------------------------------
loadandinstall <- function(mypkg) {
        for(i in seq(along=mypkg)){
                if (!is.element(mypkg[i],installed.packages()[,1])){install.packages(mypkg[i])}
                library(mypkg[i], character.only=TRUE)
        }
}
packages <- sort(c("gtools",
                   "stats",
                   "stringr",
                   "tidyr",
                   "utils"))
loadandinstall(packages)


#-----------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------
print("Pre-Processing functions")
#-----------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------
#Function for mosaicing of BKG DEM tiles
#-----------------------------------------------------------------------------------------------------
source(".../SOIL-DE/FUNCTIONS/fMosaicBKG.R")
fMosaicBKG(RASTER.DIR=".../DATA/DEM_GERMANY/DHM10BKG/",
           VECTOR.FILE=".../SOIL-DE/INPUT/QP30/BOUNDARY/umring_querfur_gross_etrs89t.shp",
           VECTOR.GRID=".../DATA/DEM_GERMANY/DHM10BKG/dgm10_k20_utm32s.shp",
           MOSAIC.DIR=".../SOIL-DE/INPUT/QP30/DEM/",
           MOSAIC.NAME="QP30",
           AGGREGATE=3,
           RASTER.FRM="asc",
           EXTENT=TRUE,
           EXTENT.NAME="QP30EXTENT")

#-----------------------------------------------------------------------------------------------------
#Function to derive terrain attributes using SAGA GIS
#-----------------------------------------------------------------------------------------------------
source("d:/Dropbox/_git/SOIL-DE/FUNCTIONS/fTerrA.R")
fTerrA(DEM.DIR=".../SOIL-DE/INPUT/QP30/DEM/",
       DEM="QP30",
       DEM.FRM=".asc",
       OUT.DIR=".../SOIL-DE/INPUT/QP30/DEM/",
       TA="TA",
       EPSG=25832,
       TCI=TRUE,
       P.CA1=10000,
       P.CA2=1000000,
       P.CA3=10,
       MBI=TRUE,
       P.MBI1=0.0001,
       P.MBI2=0.1,
       P.MBI3=10,
       NH=TRUE,
       P.NH1=2,
       P.NH2=1000,
       P.NH3=10,
       TPI=TRUE,
       P.TPI1=20,
       P.TPI2=1000,
       P.TPI3=10)


#-----------------------------------------------------------------------------------------------------
#Function to crop one- or multi.dimensional raster files
#-----------------------------------------------------------------------------------------------------
source(".../SOIL-DE/FUNCTIONS/fCropRaster.R")
fCropRaster(RU.DIR=".../SOIL-DE/INPUT/QP30/BOUNDARY/",
            RU.SHP="QP30EXTENT",
            RASTER.DIR=".../DATA/Sachsen-Anhalt/SCMaP/",
            RASTER.FILE="SoilRepCompNorm_LSGer1984-2014_Sachsen-Anhalt_EPSG31468",
            RASTER.FRM=".tif",
            RASTER.EPSG=31468,
            MULTI=TRUE,
            EXTENT=FALSE)
#-----------------------------------------------------------------------------------------------------
#Function to filter shape files
#-----------------------------------------------------------------------------------------------------
source(".../SOIL-DE/FUNCTIONS/fFilterRU.R")
RU.DIR=".../SOIL-DE/INPUT/QP30/RU/"
setwd(RU.DIR)
l.s <- mixedsort(list.files(pattern=paste("^(L).*\\.shp$",sep="")),decreasing=TRUE)
for(i in l.s){
fFilterRU(RU.DIR,
          RU.SHP = substr(i,1,nchar(i)-4),
          COL.NAME = "Brightness")
}
#-----------------------------------------------------------------------------------------------------
#Function to convert raster cells to polygons"
#-----------------------------------------------------------------------------------------------------
#optional
source(".../SOIL-DE/FUNCTIONS/fGrid2Poly.R")
fGrid2Poly(RASTER.DIR=".../SOIL-DE/QP30/DEM/",
           RASTER.FILE="QP30",
           RASTER.EPSG=,
           RASTER.FRM=".asc",
           AGG=FALSE,
           AGG.FCT=3,
           OUT.DIR=".../SOIL-DE/QP30/RU/")

#-----------------------------------------------------------------------------------------------------
#Zonal statistics
#-----------------------------------------------------------------------------------------------------
source(".../SOIL-DE/FUNCTIONS/fZonaTA.R")
RU.DIR=".../SOIL-DE/OUTPUT/QP30/"
setwd(RU.DIR)
l.s <- mixedsort(list.files(pattern=paste("^(L).*\\.shp$",sep="")),decreasing=TRUE)

for(i in l.s){
        fZonaTA(RASTER.DIR = ".../SOIL-DE/INPUT/QP30/DEM/" ,
                TA.PF = "TA",
                RU.DIR,
                RU.SHP = substr(i,1,nchar(i)-4))
}
source(".../SOIL-DE/FUNCTIONS/fZonaRS.R")
for(i in l.s){
        fZonaRS(RASTER.DIR = ".../SOIL-DE/INPUT/QP30/SCMAP/",
                RASTER.FILE = "SCMAP1984-2014_epsg25832.tif",
                RS.PF = "SM",
                RU.DIR,
                RU.SHP = substr(i,1,nchar(i)-4))
}


#-----------------------------------------------------------------------------------------------------
#Convert csv tables with coordinates to shape files 
#-----------------------------------------------------------------------------------------------------
source(".../SOIL-DE/FUNCTIONS/fTab2Shp.R")
fTab2Shp(CSV.FILE=".../SOIL-DE/INPUT/QP30/AUGER/Bodendaten_Sachsen-Anhalt",
         X="RECHTS",
         Y="HOCH",
         S.EPSG=31468,
         T.EPSG=25832)
        
#-----------------------------------------------------------------------------------------------------
#Prediction of numerical parameters
#-----------------------------------------------------------------------------------------------------
source(".../SOIL-DE/FUNCTIONS/fNumP.R")
RU.DIR=".../SOIL-DE/OUTPUT/QP30/"
setwd(RU.DIR)
l.s <- mixedsort(list.files(pattern=paste("^(L).*\\.shp$",sep="")),decreasing=TRUE)

for(PM in c("TA", "SM", "TA|SM")){
for(i in l.s){
        fNumP(RU.DIR,
              RU.SHP=substr(i,1,nchar(i)-4),
              SAMPLE.DIR=".../SOIL-DE/INPUT/QP30/AUGER/",
              SAMPLE.SHP="HUMUS_QP30_EPSG25832",
              OUT.DIR=".../SOIL-DE/OUTPUT/QP30/TA/NumP/",
              EPSG=25832,
              M.TRAIN="rf",
              PART=0.75,
              T.PM="HUMUS",
              PM=PM,
              EXPORT = FALSE)
}
}

#-----------------------------------------------------------------------------------------------------
#Classification
#-----------------------------------------------------------------------------------------------------
source(".../SOIL-DE/FUNCTIONS/fClasP.R")
source(".../SOIL-DE/FUNCTIONS/fClassAcc.R")
RU.DIR=".../SOIL-DE/OUTPUT/QP30/"
setwd(RU.DIR)
l.s <- mixedsort(list.files(pattern=paste("^(L).*\\.shp$",sep="")),decreasing=TRUE)

PM = "TA"
#"SM", "TA|SM"
for(i in l.s){
        fClasP(RU.DIR,
               RU.SHP=substr(i,1,nchar(i)-4),
               SAMPLE.DIR=".../SOIL-DE/INPUT/QP30/AUGER/",
               SAMPLE.SHP="BA_QP30_EPSG25832",
               OUT.DIR=".../SOIL-DE/OUTPUT/QP30/TA/ClasP/",
               EPSG=25832,
               M.TRAIN="rf",
               PART=0.75,
               T.CLASS="BAHG",
               PM=PM,
               UPTRAIN=TRUE,
               EXPORT=FALSE)
}
#-----------------------------------------------------------------------------------------------------
#Recursive feature selection
#-----------------------------------------------------------------------------------------------------
source(".../SOIL-DE/FUNCTIONS/fRFE.R")
RU.DIR=".../SOIL-DE/OUTPUT/QP30/"
setwd(RU.DIR)
l.s <- mixedsort(list.files(pattern=paste("^(L).*\\.shp$",sep="")),decreasing=TRUE)
PM = "TA"
#"SM", "TA|SM"
for(i in l.s){
        fRFE(RU.DIR,
             RU.SHP=substr(i,1,nchar(i)-4),
             SAMPLE.DIR=".../SOIL-DE/INPUT/QP30/AUGER/",
             SAMPLE.SHP="HUMUS_QP30_EPSG25832",
             OUT.DIR=".../SOIL-DE/OUTPUT/QP30/TA/RFE/",
             T.PM="HUMUS",
             EPSG=25832,
             PM=PM,
             CLASS=FALSE,
             UPTRAIN=FALSE,
             PART=0.75,
             NORMALIZATION = TRUE)
}

for(i in l.s){
        fRFE(RU.DIR,
             RU.SHP=substr(i,1,nchar(i)-4),
             SAMPLE.DIR=".../SOIL-DE/INPUT/QP30/AUGER/",
             SAMPLE.SHP="HUMUS_QP30_EPSG25832",
             OUT.DIR=".../SOIL-DE/OUTPUT/QP30/TA/RFE/",
             T.PM="BA",
             EPSG=25832,
             PM=PM,
             CLASS=TRUE,
             UPTRAIN=TRUE,
             PART=0.75,
             NORMALIZATION = TRUE)
}
