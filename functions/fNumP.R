#######################################################################################################
#######################################################################################################
#######################################################################################################
print("Function for the [Num]erical [P]rediction of  parameters")   
#######################################################################################################
#######################################################################################################
#######################################################################################################
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
#########################################################################################################
#########################################################################################################
fNumP <- function(RU.DIR,
                  RU.SHP,
                  SAMPLE.DIR,
                  SAMPLE.SHP,
                  OUT.DIR,
                  EPSG,
                  M.TRAIN,
                  PART,
                  T.PM,
                  PM,
                  EXPORT=FALSE){
  #------------------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------------
  print("Import training data set and data partition")
  #------------------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------------
  setwd(file.path(SAMPLE.DIR))
  PT <- st_read(paste(SAMPLE.SHP,".shp",sep=""))
  #------------------------------------------------------------------------------------------------------
  #remove outliers
  #------------------------------------------------------------------------------------------------------
  #if(OUTLIER==TRUE){
  ##plot outlier detection result
  #setwd(file.path(OUT.DIR))
  #pdf(paste(RU.SHP,"-",SAMPLE.SHP,"_",T.PM,"_MODEL-",M.TRAIN,"_OUTLIER",c(".pdf"),sep=""), height=3.5,width=6)
  #par(mfrow = c(1, 2))
  #range.PT <- range(PT[[c(c(match(paste(T.PM,sep=""),names(PT))))]])
  #boxplot(as.numeric(PT[[c(c(match(paste(T.PM,sep=""),names(PT))))]]),
  #        na.rm=TRUE,
  #        ylab=paste(T.PM),
  #        ylim=range.PT,
  #        xlab=paste("N=",nrow(PT)),
  #        sub="mit Ausreißer")
  ##outlier detection considering the right side of a distribution
  #Q.L <- quantile(PT[[c(c(match(paste(T.PM,sep=""),names(PT))))]],probs=c(Q/100, 1-Q/100))[1]
  #Q.R <- quantile(PT[[c(c(match(paste(T.PM,sep=""),names(PT))))]],probs=c(Q/100, 1-Q/100))[2]
  #PT <- subset(PT,!(PT[[c(c(match(paste(T.PM,sep=""),names(PT))))]] < Q.L))
  #PT <- subset(PT,!(PT[[c(c(match(paste(T.PM,sep=""),names(PT))))]] > Q.R))
  #boxplot(PT[[c(c(match(paste(T.PM,sep=""),names(PT))))]],
  #        ylim=range.PT,
  #        ylab=paste(T.PM),
  #        sub="ohne Ausreißer",
  #        xlab=paste("N=",nrow(PT)))
  #dev.off()
  #}
  #------------------------------------------------------------------------------------------------------
  #Splitting data as training and test set. Using createDataPartition() function from caret
  #------------------------------------------------------------------------------------------------------
  set.seed(123)
  indxTrain <- createDataPartition(y = PT[[c(c(match(paste(T.PM,sep=""),names(PT))))]],p = PART,list = FALSE)
  training <- PT[indxTrain,]
  testing <- PT[-indxTrain,]
  #export shape files
  setwd(file.path(OUT.DIR))
  write_sf(training,paste(RU.SHP,"-",SAMPLE.SHP,"_",T.PM,"_MODEL-",M.TRAIN,"_part",PART*100,"_train",sep="",".shp"), delete_layer = TRUE)
  write_sf(testing,paste(RU.SHP,"-",SAMPLE.SHP,"_",T.PM,"_MODEL-",M.TRAIN,"_part",PART*100,"_test",sep="",".shp"), delete_layer = TRUE)
  #Checking distibution in original data and partitioned data
  setwd(file.path(OUT.DIR))
  training <- na.omit(training)
  testing <- na.omit(testing)
  #Density plots of training and test data set
  setwd(file.path(OUT.DIR))
  pdf(paste(RU.SHP,"-",SAMPLE.SHP,"_",T.PM,"_MODEL-",M.TRAIN,"_DP",c(".pdf"),sep=""), height=4.5,width=7)
  par(mfrow=c(1,2))
  #plot 1
  plot(density(training[[c(c(match(paste(T.PM,sep=""),names(training))))]]),
       main="Dichtefunktionen",
       ylim=range(density(training[[c(c(match(paste(T.PM,sep=""),names(training))))]])[2],
                  density(testing[[c(c(match(paste(T.PM,sep=""),names(testing))))]])[2]),
       lwd=2,
       cex.lab=1.2,
       xlab=paste(T.PM),
       ylab="")
  lines(density(testing[[c(c(match(paste(T.PM,sep=""),names(testing))))]]),
        col="red",
        lwd=2)
  #plot 2
  plot(ecdf(training[[c(c(match(paste(T.PM,sep=""),names(training))))]]),
       do.points=TRUE,col="black",
       main="ECDF plot",
       xlab=paste(T.PM),
       ylab=paste("f(",T.PM,")",sep=""),
       cex.lab=1.2)
  plot(ecdf(testing[[c(c(match(paste(T.PM,sep=""),names(testing))))]]),
       do.points=TRUE,col="red",add=TRUE,lwd=1.5)
  legend("bottomright",
         title="KS-Test",
         c(paste("D =",round(ks.test(training[[c(c(match(paste(T.PM,sep=""),names(training))))]],testing[[c(c(match(paste(T.PM,sep=""),names(testing))))]])$statistic,2)),
           paste("p = ",round(ks.test(training[[c(c(match(paste(T.PM,sep=""),names(training))))]],testing[[c(c(match(paste(T.PM,sep=""),names(testing))))]])$p.value,2))),
         bty="n",
         cex=1)
  dev.off()
  #------------------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------------
  print("Import reference unit polygon file and overlay with training data set")
  #------------------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------------
  PN <- st_read(paste(RU.DIR,RU.SHP,".shp",sep=""))
  head(PN)
  #------------------------------------------------------------------------------------------------------
  #Overlay polygon and traning data set
  #------------------------------------------------------------------------------------------------------
  st_crs(training) = as.numeric(EPSG)
  st_crs(PN) = as.numeric(EPSG)
  training <- st_intersection(training,PN)
  head(training)
  st_geometry(training) <- NULL
  #------------------------------------------------------------------------------------------------------
  #Subsetting data set for modelling 
  #------------------------------------------------------------------------------------------------------
  training <- data.frame(NUM=training[[paste(T.PM)]], training[grepl(paste(PM,sep=""), names(training))])
  training <- na.omit(training)
  head(training)
  
  #------------------------------------------------------------------------------------------------------
  paste("Calculating model")
  #------------------------------------------------------------------------------------------------------
  set.seed(123)
  ctrl <- trainControl(method="repeatedcv",
                       number=5,
                       savePredictions = TRUE,
                       allowParallel = TRUE)
  m.Fit <-   train(NUM ~ .,
                   data = training,
                   method = M.TRAIN,
                   trControl = ctrl,
                   preProc = c("center", "scale"),
                   importance = TRUE,
                   verbose = TRUE)
  write.csv2(as.data.frame(varImp(m.Fit)$importance), 
             file=paste(RU.SHP,"-",SAMPLE.SHP,"_",T.PM,"_MODEL-",M.TRAIN,"_VarImp",c(".csv"),sep=""))

  setwd(file.path(OUT.DIR))
  write.csv2(as.data.frame(m.Fit$results), 
             file=paste(RU.SHP,"-",SAMPLE.SHP,"_",T.PM,"_MODEL-",M.TRAIN,"_CV",c(".csv"),sep=""))
  

  training[[paste("NUM_SIM")]] <- predict(m.Fit,newdata = training)
  lm <-    train(NUM ~ NUM_SIM,data=training,method = "lm")
  setwd(file.path(OUT.DIR))
  
  write.csv2(as.data.frame(lm$results), 
             file=paste(RU.SHP,"-",SAMPLE.SHP,"_",T.PM,"_MODEL-",M.TRAIN,"_ACCtrain",c(".csv"),sep=""))
  
  
  lm.R2 <- round(lm$results$Rsquared,3)
  lm.RMSE <- round(lm$results$RMSE,3)

  
  setwd(file.path(OUT.DIR))
  training[[paste("NUM_SIM")]] <- predict(m.Fit,newdata = training)
  pdf(paste(RU.SHP,"-",SAMPLE.SHP,"_",T.PM,"_MODEL-",M.TRAIN,"_SPtrain",c(".pdf"),sep=""), height=4.5,width=4)
  plot(training$NUM,training$NUM_SIM,
       ylab="Prognose",
       xlab="Test",
       xlim=range(training$NUM,training$NUM_SIM),
       ylim=range(training$NUM,training$NUM_SIM)
       #xlim=c(0,500),
       #ylim=c(0,500)
  )
  legend("bottomright",   legend= c(as.expression(bquote({italic(R)^{2}} == .(lm.R2))), 
                                    as.expression(bquote({italic(RMSE)} == .(lm.RMSE)))), bty="n",cex=1)
  abline(lm(training$NUM_SIM~training$NUM),col="red")
  dev.off()
  
  #------------------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------------
  paste("External validation")
  #------------------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------------
  #Overlay polygon and test data set
  st_crs(testing) = as.numeric(EPSG)
  testing <- st_intersection(testing,PN)
  testing <- na.omit(testing)
  head(testing)
  testing$NUM <- testing[[paste(T.PM)]]
  testing[[paste("NUM_SIM")]] <- predict(m.Fit,newdata = testing)
  ##calculating accuracy metrics
  set.seed(123)
  lm <-    train(NUM ~ NUM_SIM,data=testing,method = "lm")
  
  
  setwd(file.path(OUT.DIR))
  write.csv2(as.data.frame(lm$results), 
             file=paste(RU.SHP,"-",SAMPLE.SHP,"_",T.PM,"_MODEL-",M.TRAIN,"_ACCtest",c(".csv"),sep=""))
  

  lm.R2 <- round(lm$results$Rsquared,3)
  lm.RMSE <- round(lm$results$RMSE,3)
  #plot external performance
  setwd(file.path(OUT.DIR))
  pdf(paste(RU.SHP,"-",SAMPLE.SHP,"_",T.PM,"_MODEL-",M.TRAIN,"_SPtest",c(".pdf"),sep=""), height=4.5,width=4)
  plot(testing$NUM,testing$NUM_SIM,
       ylab="Prognose",
       xlab="Test",
       xlim=range(training$NUM,training$NUM_SIM,testing$NUM,testing$NUM_SIM),
       ylim=range(training$NUM,training$NUM_SIM,testing$NUM,testing$NUM_SIM)
  )
  legend("bottomright",   legend= c(as.expression(bquote({italic(R)^{2}} == .(lm.R2))), 
                                    as.expression(bquote({italic(RMSE)} == .(lm.RMSE)))), bty="n",cex=1)
  abline(lm(testing$NUM_SIM~testing$NUM),col="red")
  dev.off()
  #------------------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------------
  paste("Prediction to the total data set and export")
  #------------------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------------
  ##prediction to the total data set
  PN[[paste(T.PM,"_SIM",sep="")]] <- predict(m.Fit,PN)


  if(EXPORT==TRUE){
  write_sf(PN, paste(RU.SHP,"-",SAMPLE.SHP,"_",T.PM,"_MODEL-",M.TRAIN,"_part",PART*100,sep="",".shp"), delete_layer = TRUE)
  }
}
