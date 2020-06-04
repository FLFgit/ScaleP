print("Function to [R]ecursive [F]eature [E]limination for the prediction of soil classes and numeric parameters")
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

#########################################################################################################
##Function
#########################################################################################################
fRFE <- function(RU.DIR,
                 RU.SHP,
                 SAMPLE.DIR,
                 SAMPLE.SHP,
                 OUT.DIR,
                 EPSG,
                 T.PM,
                 PM,
                 CLASS=TRUE,
                 UPTRAIN=TRUE,
                 PART){
  #------------------------------------------------------------------------------------------------------
  print("Import training data set and data partition")
  #------------------------------------------------------------------------------------------------------
  setwd(file.path(SAMPLE.DIR))
  T <- st_read(paste(SAMPLE.SHP,".shp",sep=""))
  head(T)
  #------------------------------------------------------------------------------------------------------
  print("Import reference unit polygon file")
  #------------------------------------------------------------------------------------------------------
  R <- st_read(paste(RU.DIR,RU.SHP,".shp",sep=""))
  #------------------------------------------------------------------------------------------------------
  print("Overlay polygon and traning data set")
  #------------------------------------------------------------------------------------------------------
  st_crs(T) = as.numeric(EPSG)
  st_crs(R) = as.numeric(EPSG)
  T <- st_intersection(T,R)
  st_geometry(T) <- NULL
  #------------------------------------------------------------------------------------------------------
  print("Extract predictors and target parameter (TPM), which sould be predicted") 
  #------------------------------------------------------------------------------------------------------
  T <- data.frame(TPM=T[[paste(T.PM)]], T[grepl(paste(PM,sep=""), names(T))])
  T <- na.omit(T)
  if(is.numeric(T$TPM)==TRUE){
  T <- T[which(T$TPM>=0),]
  }
  nrow(T)
  head(T)
  #------------------------------------------------------------------------------------------------------
  print("Normalization and scaling of predictor variables")
  #------------------------------------------------------------------------------------------------------
  #The predictors are centered and scaled:
  x <- T[grepl(paste(PM,sep=""), names(T))]
  y <- T$TPM
  
  normalization <- preProcess(x,
                              method=c("center", "scale"),
                              #na.remove=T, k=5,
                              verbose=F)
  T <- predict(normalization, x)
  T$TPM <- y
  #------------------------------------------------------------------------------------------------------
  print("Splitting data to training and test set")
  #------------------------------------------------------------------------------------------------------
  set.seed(123)
  indxTrain <- createDataPartition(y = T$TPM,p = PART,list = FALSE)
  T.train <- T[indxTrain,]
  T.test <- T[-indxTrain,]
  head(T.train)

  if(CLASS==TRUE){
    #Plot class proportions of training and test data set
    setwd(file.path(OUT.DIR))
    pdf(paste(SAMPLE.SHP,"_",T.PM,"RFE-BP",sep="",".pdf"), height=7,width=6)
    par(mfrow = c(2, 1))
    #barplot (percentage)
    n.tab <- table(T.train$TPM)
    xx <- barplot(n.tab/sum(n.tab)*100,
                  ylab="Anteil [%]",
                  sub=paste('Gesamtanzahl =',sum(n.tab)),
                  xlab=paste(T.PM),
                  las=2,
                  ylim=c(0,100),
                  main="Training")
    text(x = xx, y = n.tab/sum(n.tab)*100, label = round(n.tab/sum(n.tab)*100,1), pos = 3, srt= 90, offset = 1, cex = 0.8, col = "red")
    n.tab <- table(T.test$TPM)
    xx <- barplot(n.tab/sum(n.tab)*100,
                  ylab="Anteil [%]",
                  sub=paste('Gesamtanzahl =',sum(n.tab)),
                  xlab=paste(T.PM),
                  las=2,
                  ylim=c(0,100),
                  main="Test")
    text(x = xx, y = n.tab/sum(n.tab)*100, label = round(n.tab/sum(n.tab)*100,1), pos = 3, srt= 90, offset = 1, cex = 0.8, col = "red")
    dev.off()
  
    #UpTRAIN
    if(UPTRAIN==TRUE){
    up_train <- upSample(x = T.train,
                         y = T.train$TPM)
    T.train <- up_train[c(1:(length(up_train)-1))]
    }
  }
  
  if(CLASS==FALSE){
    #Plot target parameter distribution of training and test data set
    setwd(file.path(OUT.DIR))
    pdf(paste(SAMPLE.SHP,"_",T.PM,"RFE-DP",sep="",".pdf"), height=4.5,width=6)
    par(mfrow=c(1,2))
    #plot 1
    plot(density(T.train$TPM),
         main="Dichtefunktionen",
         ylim=range(density(T.train$TPM)[2],
                    density(T.test$TPM)[2]),
         lwd=2,
         cex.lab=1.2,
         xlab=paste(T.PM),
         ylab="")
    lines(density(T.test$TPM),
          col="red",
          lwd=2)
    #plot 2
    plot(ecdf(T.train$TPM),
         do.points=TRUE,col="black",
         main="ECDF plot",
         xlab=paste(T.PM),
         ylab=paste("f(",T.PM,")",sep=""),
         cex.lab=1.2)
    plot(ecdf(T.test$TPM),
         do.points=TRUE,col="red",add=TRUE,lwd=1.5)
    legend("bottomright",
           title="KS-Test",
           c(paste("D =",round(ks.test(T.train$TPM,T.test$TPM)$statistic,2)),
             paste("p = ",round(ks.test(T.train$TPM,T.test$TPM)$p.value,2))),
           bty="n",
           cex=1)
    dev.off()
  }
  #------------------------------------------------------------------------------------------------------
  print("Feature selection")
  #------------------------------------------------------------------------------------------------------
  #Control settings 
  set.seed(10)
  ctrl <- rfeControl(functions = rfFuncs,
                     #method = "repeatedcv",
                     method = "cv",
                     repeats = 5,
                     verbose = FALSE)
  subsets <- 1:length(x)
  
  #Start parallelization
  cl <- makePSOCKcluster(5)
  registerDoParallel(cl)
  #Start Recursive Feature Elimination (RFE) process
  rfProfile <- rfe(T.train[,-length(T.train)], T.train[,length(T.train)],
                        sizes = subsets,
                        rfeControl = ctrl,
                        newdata = T.test)
  stopCluster(cl)
  
  #Report
  setwd(file.path(OUT.DIR))
  sink(paste(SAMPLE.SHP,"_",T.PM,"_RFE-BV",c(".txt"),sep=""))
  print(rfProfile$optVariables)
  sink()
  
  if(CLASS==TRUE){
  write.csv2(data.frame(rfProfile$fit$confusion),file = paste(SAMPLE.SHP,"_",T.PM,"_RFE-CV",c(".csv"),sep=""))
  }
  
  sink(paste(SAMPLE.SHP,"_",T.PM,"_RFE-OA",c(".txt"),sep=""))
  print(rfProfile$fit)
  sink()

  write.csv2(data.frame(rfProfile$fit$importance),file = paste(SAMPLE.SHP,"_",T.PM,"_RFE-IMP",c(".csv"),sep=""))
  write.csv2(data.frame(rfProfile$results),file = paste(SAMPLE.SHP,"_",T.PM,"_RFE-ACC",c(".csv"),sep=""))
  
  
  pdf(paste(SAMPLE.SHP,"_",T.PM,"_RFE-ACC",c(".pdf"),sep=""),height=4,width=6)
  print(plot(rfProfile, type=c("g", "o")))
  dev.off()
}
