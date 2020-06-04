print("Converting multi.dimensional SCMaP file to single SAGA GRID files")
#########################################################################################################
##Libraries
#########################################################################################################
loadandinstall <- function(mypkg) {
  for(i in seq(along=mypkg)){
    if (!is.element(mypkg[i],installed.packages()[,1])){install.packages(mypkg[i])}
    library(mypkg[i], character.only=TRUE)
  }
}
packages <- sort(c("raster"))
loadandinstall(packages)
#########################################################################################################
##Function
#########################################################################################################
fSCMaP2SAGA <- function(SCMaP.DIR,
                       SCMaP.FILE,
                       SCMaP.PF,
                       SCMaP.FRM,
                       OUT.DIR){
#-----------------------------------------------------------------------------------------------------
print("Generating single RS files")
#-----------------------------------------------------------------------------------------------------
r <- stack(paste(SCMaP.DIR,SCMaP.FILE,SCMaP.FRM,sep=""))
pb <- txtProgressBar(min=0, max=length(r@layers), style=3)
for(i in 1:length(r@layers)){
   r[[i]][r[[i]] < 0] <- NA
   writeRaster(r[[i]],paste(OUT.DIR,SCMaP.PF,i,sep=""),
                format="SAGA",
                overwrite=TRUE)
    setTxtProgressBar(pb, i)
  }
}
