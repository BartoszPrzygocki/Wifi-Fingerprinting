library(caret)
library(dplyr)
library(ggplot2)
library(doParallel)
registerDoParallel(cores=2)
library(readr)
library(tidyverse)
library(plotly)
library(esquisse)
library(caTools)
library(kknn)
library(randomForest)

#Importing Data 


WiFi_Validation<-read.csv("/Users/BP/Desktop/Ubiqium/Data Analytics/3.Deep Analytics and Visualization/3/UJIndoorLoc/validationData.csv")
Wifi_Training<-read.csv("/Users/BP/Desktop/Ubiqium/Data Analytics/3.Deep Analytics and Visualization/3/UJIndoorLoc/trainingData.csv")


##############Dicriptive

anyNA(Wifi_Training)
anyNA(Wifi_Training)

head(Wifi_Training)
head(Val1)
tail(Tra1)
tail(Val1)
class(Val1)
labels(Tra1$USERID)



dim(Tra1)
nrow(Tra1)
class(Tra1)
head(Tra1)
str(Tra1)
summary(Tra1)
any(is.na(Tra1))

summary(Wifi_Training1$BUILDINGID)
str(Wifi_Training$BUILDINGID)
str(Wifi_Training$FLOOR)
summary(Wifi_Training$FLOOR)
summary(Wifi_Training$SPACEID)
summary(Wifi_Training$RELATIVEPOSITION)
str(Wifi_Training$RELATIVEPOSITION)
summary(Wifi_Training$USERID)
str(Wifi_Training$USERID)
summary(Wifi_Training$PHONEID)


## Histograms

hist(Wifi_Training$FLOOR)
hist(Wifi_Training$BUILDINGID)
hist(Wifi_Training$SPACEID)
hist(Wifi_Training$BUILDINGID)
hist(Wifi_Training$USERID)
hist(Wifi_Training$RELATIVEPOSITION)
hist(Wifi_Training$PHONEID)


## Frequency
table(Wifi_Training$FLOOR)
table(Wifi_Training$BUILDINGID)
table(Wifi_Training$SPACEID)
table(Wifi_Training$BUILDINGID)
table(Wifi_Training$USERID)
table(Wifi_Training$RELATIVEPOSITION)
table(Wifi_Training$PHONEID)


## Building count by floor
table(Wifi_Training$BUILDINGID,Wifi_Training$FLOOR)
table(Wifi_Training$USERID,Wifi_Training$BUILDINGID)
table(Wifi_Training$USERID,Wifi_Training$PHONEID)






#DATA PREPROCESSING#


Validation<-WiFi_Validation
Training<-Wifi_Training



Training[,c(523:528)] <- Training[,c(523:528)] %>% mutate_if(is.integer, as.factor) # chnage types 
Validation[,c(523:528)] <- Validation[,c(523:528)] %>% mutate_if(is.integer, as.factor)



Training[Training == 100]<- -104  #change val 100 to -104 
Validation[Validation == 100]<- -104





#Test and Traing Distrinb
ggplot() +
  geom_point(data = Wifi_Training , aes(x = LONGITUDE, y = LATITUDE, colour = "Training Set")) +
  geom_point(data = WiFi_Validation , aes(x = LONGITUDE, y = LATITUDE, colour = "Validation Set")) +
  ggtitle("Log In Locations") 


###########Nero Zero Var / Columns and rows 


#nearZeroVar on the other hand removes columns with unique values, but also columns 
#that have very few unique values relative to the total number of observations, 
#and where the ratio of the most common value to next most common value is large (i.e. highly overdispersed). 
#Now consider the same data, but if we set the ratio of the most common value
#to next most common low enough and the cutoff for the percentage of unique values out of total number
#of sample high enough then those columns will also be selected:
#By default, a predictor is classified as near-zero variance if the 
#percentage of unique values in the samples is less than  and when the frequency 
#ratio mentioned above is greater than 19 (95/5``). 
#These default values can be changed by setting the arguments uniqueCut and freqCut.
#nearZeroVar( mdat , freqCut = 4 , uniqueCut = 40 ) 




anyDuplicated(Validation)
anyDuplicated(Training)
TrainingForGRAPHS<-TrainingForGRAPHS[!duplicated(TrainingForGRAPHS),]
Training<-Training[!duplicated(Training),]


waps_only_Validation<-Validation[,c(1:520)]
waps_only_Trainig<-Training[,c(1:520)]


which(apply(Training[1:520], 2, var)==0)
removeVarTrain<- apply(waps_only_Trainig,2, function(x) var(x)==0)
Training<-Training[,-c(which(removeVarTrain==TRUE))]
which(apply(Training[1:465], 2, var)==0)
ncol(Training)



which(apply(Validation[1:520], 2, var)==0)
removeVarValid<- apply(waps_only_Validation,2, function(x) var(x)==0)
Validation<-Validation[,-c(which(removeVarValid==TRUE))]
which(apply(Validation[1:367], 2, var)==0)
ncol(Validation)


cols_to_keep <- intersect(colnames(Training),colnames(Validation))
ncol(Training)
Training<- Training[,cols_to_keep, drop=FALSE]
ncol(Training)
ncol(Validation)
Validation<- Validation[,cols_to_keep, drop=FALSE]

tail(Training)


THIS<-(Training[,c(1:312)])


 which(apply(Training[1:312], 1, var)==0)
 removeVarTrainrow<- apply(THIS,1, function(x) var(x)==0)
 Training<-Training[-c(which(removeVarTrainrow==TRUE)),]
 which(apply(Training[1:312], 1, var)==0)


which(apply(Validation[1:312], 1, var)==0)




# Count Values != +100/-105
#  MaxTraining$count <- apply(Training[,1:465], 1, function(x) length(which(x!=-104))) # count how many are not -105
#  MaxTraining$max <- apply(Training[,1:465], 1, max) # max WAP
#  Training$maxNames <- apply(Training[,1:465],1,function(x) names(Training[,1:465])[which(x==max(x))]) # returns the max WAP name(s)
#  Training$maxVal <- apply(Training[1:465],1,function(x) names(which.max(x))) # returns only one highest WAP w. name
# 
#  The Same for Validation
#  Validation$count <- apply(Validation[,1:367], 1, function(x) length(which(x!=-104)))
#  Validation$max <- apply(Validation[,1:367], 1, max)
#  Validation$maxNMes <- apply(Validation[,1:367],1,function(x) names(Validation[,1:367])[which(x==max(x))])
#  Validation$maxVal <- apply(Validation[1:367],1,function(x) names(which.max(x)))
# 
#   Best Signal 
#  Training<- subset(Training, Training$max!=0 & Training$count!=0)
# 
#  Training<-subset(Training,Training$max<=-40 & Training$max>=-80)



tail(Training)
max(Training[1:465])
min(Training[1:465])

tail(Training)

dim(Training)
str(Training)
tail(Training)



Training$TIMESTAMP <-NULL
Training$RELATIVEPOSITION<-NULL
Training$SPACEID<-NULL
Training$USERID<-NULL
Training$PHONEID<-NULL
Training$TIMESTAMP <-NULL
Training$RELATIVEPOSITION<-NULL
Training$SPACEID<-NULL
Training$USERID<-NULL
Training$PHONEID<-NULL
#Training$count<-NULL
# # Training$max<-NULL
# # Training$maxNames<-NULL
# # Training$maxVal<-NULL
Validation$TIMESTAMP <-NULL
Validation$RELATIVEPOSITION<-NULL
Validation$SPACEID<-NULL
Validation$USERID<-NULL
Validation$PHONEID<-NULL
# # Validation$count<-NULL
# # Validation$max<-NULL
# # Validation$maxNMes<-NULL
# # Validation$maxVal<-NULL




#PCA's ###################################################################




PCA_WPAS_Validation<-Validation[1:312]
PCA_WPAS_Training<-Training[1:312]
tail(PCA_WPAS_Training[311:312])
tail(PCA_WPAS_Validation[311:312])






which(apply(PCA_WPAS_Training, 1, var)==0)
which(apply(PCA_WPAS_Training, 2, var)==0)
str(PCA_WPAS_Training)


pcaT <- prcomp(PCA_WPAS_Training, scale= T,center = T)


plot(pcaT,type="l",npcs = 100,
     main = "Principal Components") # this is a scree plot 
abline(v=80, col="lightcoral", lwd=10)


summary(pcaT) # Look at Porportion Variance lets choose 80 PC's 

print(pcaT)




PCA_WPAS_Training <- as.matrix(PCA_WPAS_Training)
PCA_WPAS_Validation <- as.matrix(PCA_WPAS_Validation)




rotation <- pcaT$rotation
PCA_train <-PCA_WPAS_Training %*% rotation  #<--- appling rotaion from PCA to a data set 
PCA_valid <- PCA_WPAS_Validation %*% rotation




Training_PCA<- as.data.frame(cbind(PCA_train[,1:80],Training[,313:321]))
Validation_PCA<-as.data.frame(cbind(PCA_valid[,1:80],Validation[,313:321]))


Training_PCA$SPACEID<-NULL
Training_PCA$RELATIVEPOSITION<-NULL
Training_PCA$USERID<-NULL
Training_PCA$PHONEID<-NULL
Training_PCA$TIMESTAMP<-NULL

Validation_PCA$SPACEID<-NULL
Validation_PCA$RELATIVEPOSITION<-NULL
Validation_PCA$USERID<-NULL
Validation_PCA$PHONEID<-NULL
Validation_PCA$TIMESTAMP<-NULL

str(pcaT$rotation)
str(pcaT$x)

ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3, allowParallel = TRUE)

head(Training_PCA)

####################PCA MODELS#######################################################





ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3, allowParallel = TRUE)

set.seed(333)

#KNN PCA#
KNN.BULDING_PCA <- train.kknn(BUILDINGID~. -LATITUDE -LONGITUDE -FLOOR,
                          data = Training_PCA,trControl = ctrl,
                          kmax = 9)

KNN.BULDING.Predict_PCA<- predict(KNN.BULDING_PCA,Validation_PCA)

confusionMatrix(KNN.BULDING.Predict_PCA, Validation_PCA$BUILDINGID)



save(KNN.BULDING.Predict, file = "KNN.BULDING.Predict.rda")

# Confusion Matrix and Statistics
# 
# Reference
# Prediction   0   1   2
# 0 535   0   0
# 1   1 307   0
# 2   0   0 268
# 
# Overall Statistics
# 
# Accuracy : 0.9991    
# 95% CI : (0.995, 1)
# No Information Rate : 0.4824    
# P-Value [Acc > NIR] : < 2.2e-16 
# 
# Kappa : 0.9986    
# Mcnemar's Test P-Value : NA        



LONGValidaction_PCA<-Validation_PCA
LONGValidaction_PCA$BUILDINGID<-KNN.BULDING.Predict_PCA

KNN.LONGITUDE_PCA <-train.kknn(LONGITUDE~.-BUILDINGID -LONGITUDE -FLOOR,
                           data = Training_PCA,trControl = ctrl,
                           kmax = 9)

KKN.LONGITUDE.Predic_PCA <- predict(KNN.LONGITUDE_PCA,Validation_PCA)


postResample(KKN.LONGITUDE.Predic_PCA, Validation_PCA$LONGITUDE)

# RMSE  Rsquared       MAE 
# 9.9842130 0.9931499 5.9950699 


#  Random forest PCA


RFBuldingPCA <- randomForest(BUILDINGID~. -LATITUDE -LONGITUDE -FLOOR,   data=Training_PCA)
print(RFBuldingPCA ) # view results 
importance(RFBuldingPCA ) # importance of each predictor
rfPredictBPCA <- predict(RFBuldingPCA, newdata = Validation_PCA)

confusionMatrix(rfPredictBPCA, Validation_PCA$BUILDINGID)
save(rfPredictBPCA, file = "rfPredictBPCA.rda")
# 
# Overall Statistics
# 
# Accuracy : 1          
# 95% CI : (0.9967, 1)
# No Information Rate : 0.4824     
# P-Value [Acc > NIR] : < 2.2e-16  
# 
# Kappa : 1          
      


RFFLOORValidaction_PCA<-Validation_PCA
RFFLOORValidaction_PCA$BUILDINGID<-rfPredictBPCA




RFFloorPCA <- randomForest(FLOOR~. -LATITUDE -LONGITUDE -BUILDINGID,   data=Training_PCA)
print(RFFloorPCA) # view results 
importance(RFFloorPCA ) # importance of each predictor
rfPredictFPCA <- predict(RFFloorPCA, newdata = RFFLOORValidaction_PCA)

confusionMatrix(rfPredictFPCA, RFFLOORValidaction_PCA$FLOOR)
save(rfPredictB, file = "rfPredictB.rda")

# Accuracy : 0.9145          
# 95% CI : (0.8965, 0.9303)
# No Information Rate : 0.4158          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.8801          
# Mcnemar's Test P-Value : NA          





RFLONGValidaction_PCA<-Validation_PCA
RFLONGValidaction_PCA$BUILDINGID<-rfPredictBPCA

RFLongPCA <- randomForest(LONGITUDE~.-BUILDINGID -LONGITUDE -FLOOR,   data=Training_PCA)
print(RFLongPCA ) # view results
importance(RFLongPCA) # importance of each predictor
rfPredictLongPCA <- predict(RFLongPCA, newdata =RFLONGValidaction_PCA)

postResample(rfPredictLongPCA, RFLONGValidaction_PCA$LONGITUDE)


save(rfPredictF, file = "rfPredictF.rda")


# 8.0231193 0.9956746 4.8136738 


RFFLONGFValidaction_PCA<-RFFLOORValidaction_PCA
RFFLONGFValidaction_PCA$BUILDINGID<-rfPredictBPCA

RFFLongPCA <- randomForest(LONGITUDE~.-BUILDINGID -LONGITUDE -FLOOR,   data=Training_PCA)
print(RFFLongPCA) # view results
importance(RFFLongPCA) # importance of each predictor
rfFPredictLongPCA <- predict(RFFLongPCA, newdata =RFFLONGFValidaction_PCA)

postResample(rfFPredictLongPCA , RFFLONGFValidaction_PCA$LONGITUDE)

# RMSE  : 7.991426
# Rsquared :  0.995723   
# MAE : 4.794817



LATRFFValidation<-RFFLONGFValidaction_PCA
LATRFFValidation$LONGITUDE<-rfFPredictLongPCA

RFFLAT <- randomForest(LATITUDE~. -LONGITUDE -BUILDINGID -FLOOR,   data=Training_PCA)
print(RFFLAT  ) # view results 
importance(RFFLAT ) # importance of each predictor
rfPredictLATF <- predict(RFFLAT , newdata = LATRFFValidation)
postResample(rfPredictLATF, LATRFFValidation$LATITUDE)

# RMSE Rsquared      MAE 
# 7.757762 0.987949 5.021140 





###############Normal MODEL###########


#Bulding 

ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3, allowParallel = TRUE)



set.seed(333)
KNN.BULDING <- train.kknn(BUILDINGID~. -LATITUDE -LONGITUDE -FLOOR,
                        data = Training,trControl = ctrl,
                        kmax = 9)

KNN.BULDING.Predict<- predict(KNN.BULDING,Validation)

confusionMatrix(KNN.BULDING.Predict, Validation$BUILDINGID)



save(KNN.BULDING.Predict, file = "KNN.BULDING.Predict.rda")

# 
# Confusion Matrix and Statistics
# 
# Reference
# Prediction   0   1   2
# 0 529   0   0
# 1   3 300   1
# 2   4   7 267
# 
# Overall Statistics
# 
# Accuracy : 0.9865          
# 95% CI : (0.9778, 0.9924)
# No Information Rate : 0.4824          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.9787          
# Mcnemar's Test P-Value : 0.009308        
# 
# Statistics by Class:
# 
# Class: 0 Class: 1 Class: 2
# Sensitivity            0.9869   0.9772   0.9963
# Specificity            1.0000   0.9950   0.9870
# Pos Pred Value         1.0000   0.9868   0.9604
# Neg Pred Value         0.9880   0.9913   0.9988
# Prevalence             0.4824   0.2763   0.2412
# Detection Rate         0.4761   0.2700   0.2403
# Detection Prevalence   0.4761   0.2736   0.2502
# Balanced Accuracy      0.9935   0.9861   0.9916
# > LONGValidaction<-Validation


#LONGITUDE plus Bulding 

LONGValidaction<-Validation
LONGValidaction$BUILDINGID<-KNN.BULDING.Predict

KNN.LONGITUDE <-train.kknn(LONGITUDE~.-BUILDINGID -LONGITUDE -FLOOR,
                           data = Training,trControl = ctrl,
                           kmax = 9)

KKN.LONGITUDE.Predic <- predict(KNN.LONGITUDE,Validation)


postResample(KKN.LONGITUDE.Predic, Validation$LONGITUDE)




building1<- Training%>%filter(BUILDINGID=="0")
building2<- Training%>%filter(BUILDINGID=="1")
building3<- Training%>%filter(BUILDINGID=="2")
valbul1<-Validation%>%filter(BUILDINGID=="0")
valbul2<-Validation%>%filter(BUILDINGID=="1")
valbul3<-Validation%>%filter(BUILDINGID=="2")
                             



KNN.BULDING1 <- train.kknn(BUILDINGID~. -LATITUDE -LONGITUDE -FLOOR,
                          data = building1,trControl = ctrl,
                          kmax = 9)

KNN.BULDING1.Predict<- predict(KNN.BULDING1,valbul1)

confusionMatrix(KNN.BULDING1.Predict, valbul1$BUILDINGID)

KNN.BULDING2 <- train.kknn(BUILDINGID~. -LATITUDE -LONGITUDE -FLOOR,
                           data = building2,trControl = ctrl,
                           kmax = 9)

KNN.BULDING2.Predict<- predict(KNN.BULDING2,valbul2)

confusionMatrix(KNN.BULDING2.Predict, valbul2$BUILDINGID)


KNN.BULDING3 <- train.kknn(BUILDINGID~. -LATITUDE -LONGITUDE -FLOOR,
                           data = building3,trControl = ctrl,
                           kmax = 9)

KNN.BULDING3.Predict<- predict(KNN.BULDING3,valbul3)

confusionMatrix(KNN.BULDING3.Predict, valbul3$BUILDINGID)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~   Random Forest   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  


#  Bulding


RFBulding <- randomForest(BUILDINGID~. -LATITUDE -LONGITUDE -FLOOR,   data=Training)
print(RFBulding ) # view results 
importance(RFBulding ) # importance of each predictor
rfPredictB <- predict(RFBulding, newdata = Validation)

confusionMatrix(rfPredictB, Validation$BUILDINGID)
save(rfPredictB, file = "rfPredictB.rda")


#Long 





LONGRFValidation<-Validation
LONGRFValidation$BUILDINGID<-rfPredictB

RFLONG <- randomForest(LONGITUDE~. -LATITUDE -BUILDINGID -FLOOR,   data=Training)
print(RFLONG ) # view results 
importance(RFLONG) # importance of each predictor
rfPredictLONG <- predict(RFLONG, newdata = Validation)
postResample(rfPredictLONG , LONGRFValidation$LONGITUDE)


# RMSE  Rsquared       MAE 
# 11.661602  0.991044  7.202388 


LATRFValidation<-LONGRFValidation
LATRFValidation$LONGITUDE<-rfPredictLONG

RFLAT <- randomForest(LATITUDE~. -LONGITUDE -BUILDINGID -FLOOR,   data=Training)
print(RFLAT  ) # view results 
importance(RFLAT ) # importance of each predictor
rfPredictLAT <- predict(RFLAT , newdata = Validation)
postResample(rfPredictLAT, LATRFValidation$LATITUDE)





#KNN to predict the building 
set.seed(333)
KK_BU_ACP <- train.kknn(BUILDINGID~.,
                        data = Training,trControl = ctrl,
                        kmax = 9)

KKN_BU_predic <- predict(KKN_BU,Testing.Data.Model)

confusionMatrix(KKN_BU_predic_ACP, Testing.Data.Model$BUILDINGID)

save(KKN_BU_predic, file = "KK_BU_predic.rda")
# #Confusion Matrix and Statistics
# 
# Reference
# Prediction   0   1   2
# 0 532   0   0
# 1   1 303   0
# 2   3   4 268
# 
# Overall Statistics
# 
# Accuracy : 0.9928          
# 95% CI : (0.9859, 0.9969)
# No Information Rate : 0.4824          
# P-Value [Acc > NIR] : < 2e-16         
# 
# Kappa : 0.9886          
# Mcnemar's Test P-Value : 0.04601         
# 
# Statistics by Class:
# 
# Class: 0 Class: 1 Class: 2
# Sensitivity            0.9925   0.9870   1.0000
# Specificity            1.0000   0.9988   0.9917
# Pos Pred Value         1.0000   0.9967   0.9745
# Neg Pred Value         0.9931   0.9950   1.0000
# Prevalence             0.4824   0.2763   0.2412
# Detection Rate         0.4788   0.2727   0.2412
# Detection Prevalence   0.4788   0.2736   0.2475
# Balanced Accuracy      0.9963   0.9929   0.9958


FloorValidaction<-Testing.Data.Model
FloorValidaction$BUILDINGID<-KK_BU_predic_ACP

KNN.Floor <-train.kknn(FLOOR~.,
                       data = Training,trControl = ctrl,
                       kmax = 9)

KKN.Floor.Predic_ACP <- predict(KNN.Floor,FloorValidaction)

confusionMatrix(KKN.Floor.Predic_ACP, FloorValidaction$FLOOR)


save(KKN.Floor.Predic_ACP, file = "KKN.Floor.Predic_ACP.rda")
# 
# Confusion Matrix and Statistics
# 
# Reference
# Prediction   0   1   2   3   4
# 0 116  19   5   1   0
# 1  10 368  16   3   2
# 2   5  60 254  25   0
# 3   1  15  31 141   9
# 4   0   0   0   2  28
# 
# Overall Statistics
# 
# Accuracy : 0.8164          
# 95% CI : (0.7923, 0.8387)
# No Information Rate : 0.4158          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.7456          
# Mcnemar's Test P-Value : NA              
# 
# Statistics by Class:
# 
# Class: 0 Class: 1 Class: 2 Class: 3 Class: 4
# Sensitivity            0.8788   0.7965   0.8301   0.8198   0.7179
# Specificity            0.9745   0.9522   0.8882   0.9404   0.9981
# Pos Pred Value         0.8227   0.9223   0.7384   0.7157   0.9333
# Neg Pred Value         0.9835   0.8680   0.9322   0.9661   0.9898
# Prevalence             0.1188   0.4158   0.2754   0.1548   0.0351
# Detection Rate         0.1044   0.3312   0.2286   0.1269   0.0252
# Detection Prevalence   0.1269   0.3591   0.3096   0.1773   0.0270
# Balanced Accuracy      0.9266   0.8744   0.8591   0.8801   0.8580


LONGValidaction<-FloorValidaction
LONGValidaction$FLOOR<-KKN.Floor.Predic_ACP

KNN.LONGITUDE <-train.kknn(LONGITUDE~.,
                           data = Training,trControl = ctrl,
                           kmax = 9)

KKN.LONGITUDE.Predic <- predict(KNN.LONGITUDE2,LONGValidaction)


postResample(KKN.LONGITUDE.Predic2, LONGValidaction$LONGITUDE)

save(KKN.LONGITUDE.Predic, file = "KKN.LONGITUDE.Predic.rda")


# RMSE   Rsquared        MAE 
# 20.3658231  0.9717875  8.1718242 

#with bulidng only 

LONGValidaction<-Testing.Data.Model
tail(Testing.Data.Model)  

LONGValidaction$BUILDINGID<-KK_BU_predic_ACP

KNN.LONGITUDE <-train.kknn(LONGITUDE~.,
                           data = Training,trControl = ctrl,
                           kmax = 9)

KKN.LONGITUDE.Predic <- predict(KNN.LONGITUDE,LONGValidaction)


postResample(KKN.LONGITUDE.Predic, LONGValidaction$LONGITUDE)

save(KKN.LONGITUDE.Predic, file = "KKN.LONGITUDE.Predic.rda")
load("KKN.LONGITUDE.Predic.rda")




# RMSE   Rsquared        MAE 
# 20.3658231  0.9717875  8.1718242 

#with bulding only 

LONGValidaction<-FloorValidaction
LONGValidaction$FLOOR<-KKN.Floor.Predic_ACP

KNN.LONGITUDE <-train.kknn(LONGITUDE~.,
                           data = Training,trControl = ctrl,
                           kmax = 9)

KKN.LONGITUDE.Predic <- predict(KNN.LONGITUDE2,LONGValidaction)


postResample(KKN.LONGITUDE.Predic2, LONGValidaction$LONGITUDE)

save(KKN.LONGITUDE.Predic, file = "KKN.LONGITUDE.Predic.rda")
load("KKN.LONGITUDE.Predic.rda")




LATITUDEValidaction<-LONGValidaction
LATITUDEValidaction$LONGITUDE<-KKN.LONGITUDE.Predic

KNN.LATITUDE <-train.kknn(LATITUDE~.,
                          data = Training,trControl = ctrl,
                          kmax = 9)

KKN.LATITUDE.Predic <- predict(KNN.LATITUDE,LATITUDEValidaction)


postResample(KKN.LATITUDE.Predic, LATITUDEValidaction$LATITUDE)


save(KKN.LATITUDE.Predic, file = "KKN.LATITUDE.Predic.rda")
load("KKN.LATITUDE.Predic.rda")
# 
# RMSE   Rsquared        MAE 
# 15.9412291  0.9490353  7.6161806 


#             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~   Random Forest   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

set.seed(123)
ctrl <- trainControl(method="cv", number = 10) 

#  Random forest


RFBulding <- randomForest(BUILDINGID ~.,   data=Training)
print(RFBulding ) # view results 
importance(RFBulding ) # importance of each predictor
rfPredictB <- predict(RFBulding, newdata = Testing.Data.Model)

confusionMatrix(rfPredictB, Testing.Data.Model$BUILDINGID)
save(rfPredictB, file = "rfPredictB.rda")
# 
# Confusion Matrix and Statistics
# 
# Reference
# Prediction   0   1   2
# 0 536   0   0
# 1   0 307   0
# 2   0   0 268
# 
# Overall Statistics
# 
# Accuracy : 1          
# 95% CI : (0.9967, 1)
# No Information Rate : 0.4824     
# P-Value [Acc > NIR] : < 2.2e-16  
# 
# Kappa : 1          
# Mcnemar's Test P-Value : NA         
# 
# Statistics by Class:
# 
# Class: 0 Class: 1 Class: 2
# Sensitivity            1.0000   1.0000   1.0000
# Specificity            1.0000   1.0000   1.0000
# Pos Pred Value         1.0000   1.0000   1.0000
# Neg Pred Value         1.0000   1.0000   1.0000
# Prevalence             0.4824   0.2763   0.2412
# Detection Rate         0.4824   0.2763   0.2412
# Detection Prevalence   0.4824   0.2763   0.2412
# Balanced Accuracy      1.0000   1.0000   1.0000



FloorRFValidaction<-Testing.Data.Model
FloorRFValidaction$BUILDINGID<-rfPredictB

RFFloor <- randomForest(FLOOR ~.,   data=Training)
print(RFFloor ) # view results
importance(RFFloor) # importance of each predictor
rfPredictF <- predict(RFFloor, newdata = Testing.Data.Model)

confusionMatrix(rfPredictF, Testing.Data.Model$FLOOR)


save(rfPredictF, file = "rfPredictF.rda")
# 
# Confusion Matrix and Statistics
# 
# Reference
# Prediction   0   1   2   3   4
# 0 115   3   0   0   2
# 1  10 413   6   0   0
# 2   6  37 293   5   0
# 3   1   9   7 166   8
# 4   0   0   0   1  29
# 
# Overall Statistics
# 
# Accuracy : 0.9145
# 95% CI : (0.8965, 0.9303)
# No Information Rate : 0.4158
# P-Value [Acc > NIR] : < 2.2e-16
# 
# Kappa : 0.8803
# Mcnemar's Test P-Value : NA
# 
# Statistics by Class:
# 
# Class: 0 Class: 1 Class: 2 Class: 3 Class: 4
# Sensitivity            0.8712   0.8939   0.9575   0.9651   0.7436
# Specificity            0.9949   0.9753   0.9404   0.9734   0.9991
# Pos Pred Value         0.9583   0.9627   0.8592   0.8691   0.9667
# Neg Pred Value         0.9828   0.9282   0.9831   0.9935   0.9907
# Prevalence             0.1188   0.4158   0.2754   0.1548   0.0351
# Detection Rate         0.1035   0.3717   0.2637   0.1494   0.0261
# Detection Prevalence   0.1080   0.3861   0.3069   0.1719   0.0270
# Balanced Accuracy      0.9331   0.9346   0.9489   0.9692   0.8713


LONGRFValidaction<-FloorRFValidaction
LONGRFValidaction$FLOOR<-rfPredictF

RFLONG <- randomForest(LONGITUDE ~.,   data=Training)
print(RFLONG ) # view results 
importance(RFLONG) # importance of each predictor
rfPredictLONG <- predict(RFLONG, newdata = Testing.Data.Model)
postResample(rfPredictLONG, LATITUDEValidaction$LONGITUDE)


save(rfPredictLONG , file = "rfPredictLONG.rda")

# RMSE  Rsquared       MAE 
# 19.771091  0.973000  8.404952 





LATITUDEValidaction<-LONGRFValidaction
LATITUDEValidaction$FLOOR<-rfPredictLONG

RFLATITUDE<- randomForest(LATITUDE ~.,   data=Training)
print(RFLATITUDE) # view results 
importance(RFLATITUDE) # importance of each predictor
rfPredictRFLATITUDE <- predict(RFLATITUDE, newdata = Testing.Data.Model)
postResample(rfPredictRFLATITUDE, LATITUDEValidaction$LATITUDE)

save(rfPredictRFLATITUDE , file = "rfPredictRFLATITUDE.rda")

#Errors################################################################################ 

LAT1<-as.data.frame(c(rfPredictLATF))
LONG1<-as.data.frame(c(rfFPredictLongPCA))
VLAT<-as.data.frame(c(Validation_PCA$LATITUDE))
VLONG<-as.data.frame(c(Validation_PCA$LONGITUDE))
LONGLAT_PREDICTIONS <- as.data.frame(c(LAT1,LONG1))
Validation_LOGLAT <-as.data.frame(c(VLAT,VLONG))



# change column names
colnames(LONGLAT_PREDICTIONS)[2] <- 'LONGITUDE'
colnames(LONGLAT_PREDICTIONS)[1] <- 'LATITUDE'
colnames(Validation_LOGLAT)[2] <- 'LONGITUDE'
colnames(Validation_LOGLAT)[1] <- 'LATITUDE'




# Plot real and predicted results


#Randome Forest PCA 


# Training and Validation log in locations
ggplot() +
  geom_point(data = LONGLAT_PREDICTIONS , aes(x = LONGITUDE, y = LATITUDE, colour = "Predictions")) +
  geom_point(data = Validation_LOGLAT , aes(x = LONGITUDE, y = LATITUDE, colour = "Validation")) +
  ggtitle("Prediction Vs Validation") 



# Distribution of distance error (in meters)
Error = sqrt((LONGLAT_PREDICTIONS$LONGITUDE - Validation_LOGLAT$LONGITUDE)^2 +(LONGLAT_PREDICTIONS$LATITUDE - Validation_LOGLAT$LATITUDE)^2)
hist(Error, freq = T, xlab = " Absolute error (m)", col = "lightcyan", main = "Error distance in meters")


mean(Error) #<---- Mean of all the predctions 

avrageErrorLONG<-(LONGLAT_PREDICTIONS$LONGITUDE - Validation_LOGLAT$LONGITUDE)
avrageErrorLAT<-(LONGLAT_PREDICTIONS$LATITUDE - Validation_LOGLAT$LATITUDE)

mean(avrageErrorLAT)
mean(avrageErrorLONG)






#KNN Normal Errors 

KNNErrors <-sqrt((KKN.LONGITUDE.Predic - Validation$LONGITUDE)^2 +(KKN.LATITUDE.Predic - Validation_LOGLAT$LATITUDE)^2)
hist(KNNErrors, freq = T, xlab = " Absolute error (m)", col = "lightcyan", main = "Error distance in meters")




KNNLAT1<-as.data.frame(c(KKN.LATITUDE.Predic))
KNNLONG1<-as.data.frame(c(KKN.LONGITUDE.Predic))
KNNLOGLAT <- as.data.frame(c(KNNLAT1,KNNLONG1))
colnames(KNNLOGLAT)[2] <- 'LONGITUDE'
colnames(KNNLOGLAT)[1] <- 'LATITUDE'

avrageErrorKNNLONG<-(KKN.LONGITUDE.Predic - Validation$LONGITUDE)
avrageErrorKNNLAT<-(KKN.LATITUDE.Predic - Validation$LATITUDE)
mean(avrageErrorKNNLAT)
mean(avrageErrorKNNLONG)



#KNN VS RANDOME FOREST 

ggplot() +
  geom_point(data = KNNLOGLAT, aes(x = LONGITUDE, y = LATITUDE, colour = "KNN MODEL"))+
  geom_point(data = LONGLAT_PREDICTIONS , aes(x = LONGITUDE, y = LATITUDE, colour = "Randome Forest")) +
  ggtitle("Predictions KNN vs Randome Forest") 







