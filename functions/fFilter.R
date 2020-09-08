print("Function to filter shape files")
#########################################################################################################
##Libraries and functions
#########################################################################################################
loadandinstall <- function(mypkg) {
  for(i in seq(along=mypkg)){
    if (!is.element(mypkg[i],installed.packages()[,1])){install.packages(mypkg[i])}
    library(mypkg[i], character.only=TRUE)
  }
}
packages <- sort(c("sf",
                   "sp",
                   "gtools"))
loadandinstall(packages)

#########################################################################################################
##Function
#########################################################################################################
fFilter <- function(RU.DIR,
                    RU.SHP,
                    COL.NAME){
  #------------------------------------------------------------------------------------------------------
  print("Import reference unit polygon file")
  #------------------------------------------------------------------------------------------------------
  R <- st_read(paste(RU.DIR,RU.SHP,".shp",sep=""))
  #R <- R[which(R$Brightness>0),]
  R <- R[which(R[[paste(COL.NAME)]]>0),]
  R$ID <- 1:nrow(R)
  setwd(RU.DIR)
  write_sf(R,paste(RU.SHP,"_F",sep="",".shp"), delete_layer = TRUE)
}
