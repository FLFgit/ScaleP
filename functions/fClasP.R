#######################################################################################################
#######################################################################################################
#######################################################################################################
print("Functions for the [Scale]-specific [P]rediction of soil classes and numeric parameters")   
#######################################################################################################
#######################################################################################################
#######################################################################################################
fClasP <- function(POLYGON.DIR,
                   POLYGON.SHP,
                   TRAIN.DIR,
                   TRAIN.SHP,
                   OUT.DIR,
                   EPSG,
                   M.TRAIN,
                   PART,
                   T.CLASS,
                   PF.TA,
                   UPTRAIN=TRUE){
  #------------------------------------------------------------------------------------------------------
  print("Import training data set and data partition")
  #------------------------------------------------------------------------------------------------------
  setwd(file.path(TRAIN.DIR))
  #PT <- shapefile(paste(TRAIN.SHP,".shp",sep=""))#point data set
  PT <- st_read(paste(TRAIN.SHP,".shp",sep=""))
  #Splitting data as training and test set. Using createDataPartition() function from caret
  set.seed(123)
  indxTrain <- createDataPartition(y = PT[[c(c(match(paste(T.CLASS,sep=""),names(PT))))]],p = PART,list = FALSE)
  training <- PT[indxTrain,]
  testing <- PT[-indxTrain,]
  #export shape files
  setwd(file.path(OUT.DIR))
  write_sf(training,paste(TRAIN.SHP,"_",T.CLASS,"_MODEL-",M.TRAIN,"_part",PART*100,"_train",sep="",".shp"), delete_layer = TRUE)
  write_sf(testing,paste(TRAIN.SHP,"_",T.CLASS,"_MODEL-",M.TRAIN,"_part",PART*100,"_test",sep="",".shp"), delete_layer = TRUE)
  #Checking distibution in original data and partitioned data
  setwd(file.path(OUT.DIR))
  training <- na.omit(training)
  testing <- na.omit(testing)
  #Plot class proportions of training and test data set
  setwd(file.path(OUT.DIR))
  pdf(paste(TRAIN.SHP,"_",T.CLASS,"_MODEL-",M.TRAIN,"_BP",c(".pdf"),sep=""), height=7,width=6)
  par(mfrow = c(2, 1))
  #barplot (percentage)
  n.tab <- table(training[[c(c(match(paste(T.CLASS,sep=""),names(training))))]])
  xx <- barplot(n.tab/sum(n.tab)*100,
                ylab="Anteil [%]",
                sub=paste('Gesamtanzahl =',sum(n.tab)),
                xlab=paste(T.CLASS),
                las=2,
                ylim=c(0,100),
                main="Training")
  text(x = xx, y = n.tab/sum(n.tab)*100, label = round(n.tab/sum(n.tab)*100,1), pos = 3, cex = 0.8, col = "red")
  n.tab <- table(testing[[c(c(match(paste(T.CLASS,sep=""),names(testing))))]])
  xx <- barplot(n.tab/sum(n.tab)*100,
                ylab="Anteil [%]",
                sub=paste('Gesamtanzahl =',sum(n.tab)),
                xlab=paste(T.CLASS),
                las=2,
                ylim=c(0,100),
                main="Test")
  text(x = xx, y = n.tab/sum(n.tab)*100, label = round(n.tab/sum(n.tab)*100,1), pos = 3, cex = 0.8, col = "red")
  dev.off()
  #------------------------------------------------------------------------------------------------------
  print("Import reference unit polygon file")
  #------------------------------------------------------------------------------------------------------
  PN <- st_read(paste(POLYGON.DIR,POLYGON.SHP,".shp",sep=""))
  #------------------------------------------------------------------------------------------------------
  print("Overlay polygon and traning data set")
  #------------------------------------------------------------------------------------------------------
  st_crs(training) = as.numeric(EPSG)
  st_crs(PN) = as.numeric(EPSG)
  training <- st_intersection(training,PN)
  head(training)
  st_geometry(training) <- NULL
  #------------------------------------------------------------------------------------------------------
  #Subsetting data set for modelling 
  #------------------------------------------------------------------------------------------------------
  training <- data.frame(CLASS=training[[paste(T.CLASS)]], training[grepl(paste(PF.TA,sep=""), names(training))])
  training <- na.omit(training)
  #------------------------------------------------------------------------------------------------------
  paste("Model training")
  #------------------------------------------------------------------------------------------------------
  set.seed(123)
  ctrl <- trainControl(method="repeatedcv",
                       number=5,
                       classProbs=TRUE,
                       savePredictions = TRUE,
                       allowParallel = TRUE)
  
  set.seed(123)
  if(UPTRAIN==TRUE){
    up_train <- upSample(x = training,
                         y = training$CLASS)
    training <- up_train[c(1:(length(up_train)-1))]
  }
  m.Fit <-   train(CLASS ~ .,
                   data = training,
                   method = M.TRAIN,
                   trControl = ctrl,
                   preProc = c("center", "scale"),
                   importance = TRUE,
                   verbose = TRUE)
  
  write.csv2(as.data.frame(varImp(m.Fit)$importance), 
             file=paste(TRAIN.SHP,"_",T.CLASS,"_MODEL-",M.TRAIN,"_VarImp",c(".csv"),sep=""))
  #VI$SUM <- apply(VI, 1, sum)  
  #barplot(sort(VI$SUM,decreasing=TRUE))
  
  setwd(file.path(OUT.DIR))
  sink(paste(TRAIN.SHP,"_",POLYGON.SHP,"_",T.CLASS,"_MODEL-",M.TRAIN,"_CV",sep="",".txt"))
  print(m.Fit)
  sink()
  
  #------------------------------------------------------------------------------------------------------
  paste("External validation")
  #------------------------------------------------------------------------------------------------------
  #Overlay polygon and traning data set
  st_crs(testing) = as.numeric(EPSG)
  testing <- st_intersection(testing,PN)
  testing <- na.omit(testing)
  testing[[paste(T.CLASS,"_SIM",sep="")]] <- predict(m.Fit,newdata = testing)
  ##calculating export accuracy metrics
  head(testing)
  set.seed(123)
  acc.m <- Evaluate(actual=testing[[paste(T.CLASS,sep="")]], predicted=testing[[paste(T.CLASS,"_SIM",sep="")]])
  ##export
  write.table(acc.m$ConfusionMatrix, 
              file = paste(TRAIN.SHP,"_",POLYGON.SHP,"_",T.CLASS,"_MODEL-",M.TRAIN,"_CM",sep="",".csv"), 
              sep = ";", 
              dec=",")
  write.table(acc.m$AccMetrics, 
              file = paste(TRAIN.SHP,"_",POLYGON.SHP,"_",T.CLASS,"_MODEL-",M.TRAIN,"_AM",sep="",".csv"), 
              sep = ";", 
              dec=",")
  
  
  #------------------------------------------------------------------------------------------------------
  paste("Prediction to the total data set")
  #------------------------------------------------------------------------------------------------------
  ##prediction to the total data set
  PN[[paste(T.CLASS,"_SIM",sep="")]] <- predict(m.Fit,PN)
  #deriving probabilities
  PN[[paste(T.CLASS,"_PRB",sep="")]] <- apply(predict(m.Fit,PN,type="prob"), 1, max)
  head(PN)
  ##Plotting of final number area proportions 
  #area bar plot for all crop types
  PN$HECTARES <- st_area(PN)/10000
  agg <-aggregate(PN$HECTARES, by=list(PN[[c(match(paste(T.CLASS,"_SIM",sep=""),names(PN)))]]),FUN=sum, na.rm=TRUE)
  sum.agg <- sum(agg[[2]])
  
  #setwd(file.path(OUT.DIR))
  #pdf(paste(TRAIN.SHP,"_",POLYGON.SHP,"_",T.CLASS,"_MODEL-",M.TRAIN,"_PredictionBarplot",c(".pdf"),sep=""), 
  #    height=4,width=6)
  sum <- tapply(agg[[2]]/1000,agg[[1]],sum)
  #xx <- barplot(sum,
                #ylab="Area [x 1000 ha]",
                #sub=paste('Gesamtfläche =',round(sum.agg,0),"ha"),
                ##xlab=paste(T.CLASS,"(Prognose)"),
                #xlab="Bodenart",
                #las=2,
                #ylim=c(0,(max(sum, na.rm = TRUE)+10)))
  #text(x = xx, y = sum, label = round(sum,1), pos = 3, cex = 0.8, col = "red")
  #dev.off()
  
  setwd(file.path(OUT.DIR))
  pdf(paste(TRAIN.SHP,"_",POLYGON.SHP,"_",T.CLASS,"_MODEL-",M.TRAIN,"_BP",c(".pdf"),sep=""), height=4,width=6)
  xx <- barplot(sum*100*1000/sum.agg,
                ylab="Anteil [%]",
                sub=paste('Gesamtfläche =',round(sum.agg,1),"ha"),
                xlab=paste(T.CLASS,"(Prognose)"),
                las=2,
                ylim=c(0,100))
  text(x = xx, y = sum*100*1000/sum.agg, label = round(sum*100*1000/sum.agg,1), pos = 3, cex = 0.8, col = "red")
  dev.off()
  
  
 
  
  
  #export final prediction shape
  setwd(file.path(OUT.DIR))
  PN.class <- PN[c(length(PN),length(PN)-1)]
  PN.class$ID <- PN$ID
  write_sf(PN.class, paste(TRAIN.SHP,"_",POLYGON.SHP,"_",T.CLASS,"_MODEL-",M.TRAIN,"_part",PART*100,sep="",".shp"), delete_layer = TRUE)
  #return(list(Model.Acc = m.Fit, Class.Acc = acc.m))
}
