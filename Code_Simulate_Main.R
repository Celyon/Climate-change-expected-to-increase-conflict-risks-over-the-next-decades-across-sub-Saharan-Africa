##Step1##
##train BRT models##
##Run Time: approximately 1 hours##
rm(list=ls());
library(caret);
library(ggplot2);
library(nnet);
library(e1071);
library(ROCR)
library(RColorBrewer)
library(MLmetrics) #PRAUC
require(ggthemes)
require(coin)
require(plotrix)
library(raster)
library(dismo)
library(gbm)


timestart<-Sys.time();

filename=paste("F:\\Dataset_Simulate_Main.csv",sep='')
data <- read.csv(filename,header=T,encoding="utf-8")

data <- subset(data, select = -UCDP)
data <- subset(data, select = -Violent)
# data <- subset(data, select = -Demonstractions)


data <- data[,1:13]
summary(data)
names(data) <- c('X',	'Y',	'Year',	'Exclusion','GDP','land_cover',	'Liberal_Democracy_Index', 'Accessibility_to_cities', 
                 'Precipitation_Anomaly','Mean_Precipitation','Temperature_Anomaly','Mean_Temperature',	'Conflict')


temp_data <- data[,4]
data$Excluded <- as.factor(temp_data)



copy_Alldata <- data[,1:3]

temp_Alldata <- data[,4:13]

Alldata <- temp_Alldata



Accuracy_data <- c()

i = 1
for(i in 1 : 50){
  set.seed(i)
  
  Conflict_data_all<- subset(Alldata,Conflict == 1)
  noConflict_data_all<- subset(Alldata,Conflict != 1)
  
  rows_Conflict <- nrow(Conflict_data_all)
  rows_noConflict <- nrow(noConflict_data_all)
  proportion <- rows_Conflict/rows_noConflict
  
  samples_noConflict_size <- round(rows_noConflict*proportion);
  selectIndex <- sample(rows_noConflict, size=samples_noConflict_size);
  samples_noConflict_data <- noConflict_data_all[selectIndex, ]
  
  Conflict_data=rbind(Conflict_data_all,samples_noConflict_data)
  summary(Conflict_data)
  
  train.flag <- createDataPartition(y=Conflict_data$Conflict,p=1,list=FALSE)
  training <- Conflict_data[train.flag,]
  
  BRT_model <- gbm.step(data=training, gbm.x = 1:9, gbm.y = 10, tree.complexity = 4,learning.rate = 0.01, bag.fraction = 0.75, step.size = 10, cv.folds = 10, max.trees = 10000)
  
  pred <- predict.gbm(BRT_model, training, n.trees=BRT_model$n.trees, "response")
  pred <- data.frame(pred)
  names(pred) <- c("risk")
  
  
  figure_name=paste( "F:\\Demonstractions_Simulate_",i,".jpeg",sep="")
  jpeg(filename =figure_name,units="in",res = 400,height=8,width=8)
  par(mfrow=c(3,3))
  
  ggplot(pred, aes(x=risk)) + geom_bar(stat="bin")
  gbm.plot(BRT_model, n.plots=9, write.title = TRUE, plot.layout = c(3, 3))
  dev.off()
  
  
  ###PR-AUC
  PR_AUC<-PRAUC(y_pred = pred, y_true = training$Conflict)
  ###F1 score
  F1_pred <- ifelse(pred < 0.5, 0, 1)
  F1_score1 <- F1_Score(y_pred = F1_pred, y_true =training$Conflict, positive = "1")
  
  temp_auc_training <- BRT_model$cv.statistics$discrimination.mean
  
  temp_accuracy_data <- c()
  temp_accuracy_data <- cbind(temp_auc_training,PR_AUC,F1_score1)
  Accuracy_data <- rbind(Accuracy_data,temp_accuracy_data)
  
  modelname<-paste("F:\\Demonstractions_Simulate_",i,".Rdata",sep="")
  
  save(BRT_model, file = modelname)
  
  pred <-  predict.gbm(BRT_model, Alldata, n.trees=BRT_model$n.trees, "response")
  
  copy_Alldata$Risk <- pred
  simulate_filename <- paste("F:\\Demonstractions_Simulate_",i,".csv",sep="")
  write.csv(copy_Alldata, file=simulate_filename,row.names=F)
}
filename<-paste("F:\\Demonstractions_Simulate_risk_accuracy.csv",sep="")
write.csv(Accuracy_data, file=filename)

timeend<-Sys.time()
runningtime<-timeend-timestart
print(runningtime)


