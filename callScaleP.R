#######################################################################################################
#######################################################################################################
#######################################################################################################
print("Functions for the [Scale]-specific [P]rediction of soil classes and numeric parameters")   
#######################################################################################################
#######################################################################################################
#######################################################################################################
source("d:/Dropbox/_git/SOIL-DE_ScaleP/_function/fPackages.R")
source("d:/Dropbox/_git/SOIL-DE_ScaleP/_function/fTerrA.R")
source("d:/Dropbox/_git/SOIL-DE_ScaleP/_function/fColorComposite.R")
source("d:/Dropbox/_git/SOIL-DE_ScaleP/_function/fZonaSt.R")
source("d:/Dropbox/_git/SOIL-DE_ScaleP/_function/fClasP.R")
source("d:/Dropbox/_git/SOIL-DE_ScaleP/_function/fEvaluate.R")
#-----------------------------------------------------------------------------------------------------
#Calculation of multi-scale terrain attributes
#-------------------------------------------------------------------------------
fTerrA(DEM.DIR="d:/Dropbox/_git/SOIL-DE_ScaleP/_input/DEM/",
       DEM="DEM10",
       DEM.FRM=".asc",
       OUT.DIR="d:/Dropbox/_git/SOIL-DE_ScaleP/_output/",
       TA="TA",
       AGGREGATE=2,
       EPSG=31468,
       TCI=TRUE,
       MBI=TRUE,
       NH=TRUE,
       TPI=TRUE,
       VECTOR=TRUE,
       MRVBF=TRUE)
#-----------------------------------------------------------------------------------------------------
#Zonal statistics
#-------------------------------------------------------------------------------
fZonaSt(TA.DIR = "d:/Dropbox/_git/SOIL-DE_ScaleP/_output/TA/",
        TA = "TA",
        POLYGON.DIR = "d:/Dropbox/_git/SOIL-DE_ScaleP/_output/",
        POLYGON.SHP = "DEM10_AGGREGATE2",
        OUT.DIR = "d:/Dropbox/_git/SOIL-DE_ScaleP/_output/")

#-----------------------------------------------------------------------------------------------------
#Classification
#-------------------------------------------------------------------------------
fClasP(POLYGON.DIR="d:/Dropbox/_git/SOIL-DE_ScaleP/_output/",
       POLYGON.SHP="DEM10_AGGREGATE2_TA",
       TRAIN.DIR="d:/Dropbox/_git/SOIL-DE_ScaleP/_input/AUGER/",
       TRAIN.SHP="FIS_T1_EPSG31468",
       OUT.DIR="d:/Dropbox/_git/SOIL-DE_ScaleP/_output/",
       EPSG="31468",
       M.TRAIN="rf",
       PART=0.75,
       T.CLASS="BOART",
       PF.TA="TA",
       UPTRAIN=TRUE)
#-----------------------------------------------------------------------------------------------------
#Color composite
#-------------------------------------------------------------------------------
fColorComposite(DATA.DIR = OUT.DIR,
          RESULT.DIR = OUT.DIR,
          B1 = "TA3.asc",
          B2 = "TA23.asc",
          B3 = "TA15.asc",
          SHD= "TA16.asc",
          ALPHA=0.5,
          H=2000,
          W=2100,
          RES=300)
