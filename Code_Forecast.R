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

filename=paste("F:\\Dataset_Forecast.csv",sep='')
data <- read.csv(filename,header=T,encoding="utf-8")
#GDP
#data <- subset(data, select = -SSP245_GDP)
data <- subset(data, select = -SSP370_GDP)
data <- subset(data, select = -SSP585_GDP)

#data <- subset(data, select = -Land_SSP245)
data <- subset(data, select = -Land_SSP370)
data <- subset(data, select = -Land_SSP585)


#data <- subset(data, select = -B_SSP245_SPI)
#data <- subset(data, select = -B_SSP245_STI)
data <- subset(data, select = -B_SSP370_SPI)
data <- subset(data, select = -B_SSP370_STI)
data <- subset(data, select = -B_SSP585_SPI)
data <- subset(data, select = -B_SSP585_STI)
data <- subset(data, select = -C_SSP245_SPI)
data <- subset(data, select = -C_SSP245_STI)
data <- subset(data, select = -C_SSP370_SPI)
data <- subset(data, select = -C_SSP370_STI)
data <- subset(data, select = -C_SSP585_SPI)
data <- subset(data, select = -C_SSP585_STI)
data <- subset(data, select = -I_SSP245_SPI)
data <- subset(data, select = -I_SSP245_STI)
data <- subset(data, select = -I_SSP370_SPI)
data <- subset(data, select = -I_SSP370_STI)
data <- subset(data, select = -I_SSP585_SPI)
data <- subset(data, select = -I_SSP585_STI)


data <- data[,1:12]
summary(data)
names(data) <- c('X','Y','Year','Exclusion','GDP','land_cover','Liberal_Democracy_Index','Accessibility_to_cities','Precipitation_Anomaly','Mean_Precipitation','Temperature_Anomaly','Mean_Temperature')
temp_data <- data[,4]
data$Excluded <- as.factor(temp_data)

copy_Alldata <- data[,1:3]
temp_Alldata <- data[,4:12]
Alldata <- temp_Alldata
Modelorder=1
for (Modelorder in 1:50) {
  modelname <-
    paste("F:\\Demonstractions_Forecast_",
          Modelorder,".Rdata", sep = "")
  print(Modelorder)
  load(modelname)
  pred <- predict.gbm(BRT_model, temp_Alldata, n.trees=BRT_model$n.trees, "response")
  copy_Alldata$Risk <- pred
  simulate_filename <- paste("F:\\D_B_245_",Modelorder,".csv",sep="")
  write.csv(copy_Alldata, file=simulate_filename,row.names=F)
}
timeend<-Sys.time()
runningtime<-timeend-timestart
print(runningtime)