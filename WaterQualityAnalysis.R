getwd()
library(readxl)
WaterQualityDataSet<- read_excel("WaterQualityDataSet(2006-2016).xlsx")
#dimensions of data
names(WaterQualityDataSet)
dim(WaterQualityDataSet)
str(WaterQualityDataSet)
#cheking for missing value
#Converting Empty value to NA
WaterQualityDataSet[WaterQualityDataSet==""]=NA
WaterQualityDataSet[WaterQualityDataSet=="-"]=NA
#MIssing values in each parameter
sapply(WaterQualityDataSet,function(x) sum(is.na(x)))
library(DataExplorer)
plot_missing(WaterQualityDataSet)
#Missing value treatment
#Removing variables S.NO
WaterQualityDataSet=WaterQualityDataSet[,-c(1)]
WaterQualityDataSet=na.omit(WaterQualityDataSet)
str(WaterQualityDataSet)

#converting categorical variable to factor
WaterQualityDataSet$WATERTYPE=factor(WaterQualityDataSet$WATERTYPE,levels=c("GROUNDWATER","RIVERWATER"))

#Data Abstract
str(WaterQualityDataSet)
names(WaterQualityDataSet)
library(psych)
#Outlier determination and treatment
boxplot((WaterQualityDataSet[,4:10]))

x<- WaterQualityDataSet$`TOTALCOLIFORM(MPN/100ml)`
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
WaterQualityDataSet$`TOTALCOLIFORM(MPN/100ml)`=x 

Y<- WaterQualityDataSet$`FECALCOLIFORM(MPN/100ml)`
qnt <- quantile(Y, probs=c(.25, .75), na.rm = T)
caps <- quantile(Y, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
Y[Y < (qnt[1] - H)] <- caps[1]
Y[Y > (qnt[2] + H)] <- caps[2]
WaterQualityDataSet$`FECALCOLIFORM(MPN/100ml)`=Y

boxplot((WaterQualityDataSet[,4:10]))

#CalculateWQI
# Normalized data
#Normalise Temp
normalizeTemp=function(x){
  if ((x<= 15) & (x >= 0)) {
    x=100
  } else if ((( x <= 20) & (x >= 15)) | ((x<= 0) &(x >= -5))) {
    x=80
  } else if ((( x <= 25) & (x >= 20)) | ((x <= -5) &(x >=-10))) {
    x=60
  } else if (((x <= 35) & (x >= 25))| ((x <= -10) &(x >= -15))) {
    x=40
  } else{
    x=0
  }
}
NormalisedTemp=Vectorize(normalizeTemp)
WaterQualityDataSet$nTemp=NormalisedTemp(WaterQualityDataSet$`TEMPERATURE(c)`)


#Normalze PH
normalizePH=function(x){
  if ((x<= 8.5) & (x >= 6.5)) {
  x=100
} else if ((( x <= 6.5) & (x >= 6)) | ((x<= 9) &(x >= 8.5))) {
  x=80
} else if ((( x <= 6) & (x >= 4)) | ((x <= 10) &(x >=9))) {
   x=60
} else if (((x <= 4) & (x >= 2))| ((x <= 12) &(x >= 10))) {
    x=40
} else{
      x=0
}
}
NormalisedPH=Vectorize(normalizePH)
WaterQualityDataSet$npH=NormalisedPH(WaterQualityDataSet$pH)

#Normalze Conductivity
normalizeCon=function(x){
  if ((x<= 75) & (x >= 0)) {
    x=100
  } else if ((( x <= 150) & (x >= 75))) {
    x=80
  } else if ((( x <= 225) & (x >= 150))) {
    x=60
  } else if (((x <= 300) & (x >= 225))) {
    x=40
  } else{
    x=0
  }
}
NormalisedCon=Vectorize(normalizeCon)
WaterQualityDataSet$nCon=NormalisedCon(WaterQualityDataSet$`CONDUCTIVITY(µmhos/cm)`)



#Normalze BOD
normalizeBod=function(x){
  if ((x<= 3) & (x >= 0)) {
    x=100
  } else if ((( x <= 6) & (x >= 3))) {
    x=80
  } else if ((( x <= 80) & (x >= 6))) {
    x=60
  } else if (((x <= 125) & (x >= 80))) {
    x=40
  } else{
    x=0
  }
}
NormalisedBod=Vectorize(normalizeBod)
WaterQualityDataSet$nBod=NormalisedBod(WaterQualityDataSet$`B.O.D(mg/l)`)


#Normalze Nitrate
normalizeNit=function(x){
  if ((x<= 20) & (x >= 0)) {
    x=100
  } else if ((( x <= 50) & (x >= 20))) {
    x=80
  } else if ((( x <= 100) & (x >= 50))) {
    x=60
  } else if (((x <= 200) & (x >= 100))) {
    x=40
  } else{
    x=0
  }
}
NormalisedNit=Vectorize(normalizeNit)
WaterQualityDataSet$nNit=NormalisedNit(WaterQualityDataSet$NITRATE)


#Normalze Coliform
normalizeColi=function(x){
  if ((x<= 5) & (x >= 0)) {
    x=100
  } else if ((( x <= 50) & (x >= 5))) {
    x=80
  } else if ((( x <= 500) & (x >= 50))) {
    x=60
  } else if (((x <= 10000) & (x >= 500))) {
    x=40
  } else{
    x=0
  }
}
NormalisedColi=Vectorize(normalizeColi)
WaterQualityDataSet$nColi=NormalisedColi(WaterQualityDataSet$`TOTALCOLIFORM(MPN/100ml)`)


#Calculate WQI
WaterQualityDataSet$WTemp=WaterQualityDataSet$nTemp *0.17
WaterQualityDataSet$Wph=WaterQualityDataSet$npH * 0.17
WaterQualityDataSet$WBod=WaterQualityDataSet$nBod* 0.13
WaterQualityDataSet$WCon=WaterQualityDataSet$nCon* 0.086
WaterQualityDataSet$WNit=WaterQualityDataSet$nNit * 0.22
WaterQualityDataSet$WColi=WaterQualityDataSet$nColi* 0.22
WaterQualityDataSet$WQI=WaterQualityDataSet$WTemp+WaterQualityDataSet$Wph+WaterQualityDataSet$WCon+WaterQualityDataSet$WBod+WaterQualityDataSet$WNit+WaterQualityDataSet$WColi
write.csv(WaterQualityDataSet,file="WaterQualityDataSetProcessed.csv")

#Histogram
hist(WaterQualityDataSet$`TEMPERATURE(c)`)
hist(WaterQualityDataSet$pH)
hist(WaterQualityDataSet$`CONDUCTIVITY(µmhos/cm)`)
hist(WaterQualityDataSet$`B.O.D(mg/l)`)
hist(WaterQualityDataSet$NITRATE)
hist(WaterQualityDataSet$`FECALCOLIFORM(MPN/100ml)`)
hist(WaterQualityDataSet$`TOTALCOLIFORM(MPN/100ml)`)

#Variable importance
library(randomForest)
classifier = randomForest(x = WaterQualityDataSet[,c(4,5,6,7,8,10)],
                          y = WaterQualityDataSet$WQI,
                          ntree = 30)

varImpPlot(classifier)

#Correlation
library(psych)
library(corrplot)
library(stats)
corrmat1=cor(WaterQualityDataSet[,4:10],use="pairwise.complete.obs")
corrplot(corrmat1)
#Due to High correlation 
#Remove FeacalColiform
corrmat2=cor(WaterQualityDataSet[,4:8,10],use="pairwise.complete.obs")
corrplot(corrmat2)


#Splitting Dataset to test and train Dataset
# Splitting the dataset into the Training set and Test set
library(caTools)
set.seed(123)
row.number <- sample(1:nrow(WaterQualityDataSet), 0.7*nrow(WaterQualityDataSet))
traindata = WaterQualityDataSet[row.number,]
testdata = WaterQualityDataSet[-row.number,]
dim(traindata)
dim(testdata)

#Explore data
library(ggplot2)
ggplot(traindata, aes(WQI)) + geom_density(fill="blue")
ggplot(traindata, aes(log(WQI))) + geom_density(fill="blue")
ggplot(traindata, aes(sqrt(WQI))) + geom_density(fill="blue")


#Linear Regression
linearModel=lm(WQI ~.,
               data = traindata[,c(4:8,10,24)])
summary(linearModel)
par(mfrow=c(2,2))
plot(linearModel)

#Performance on test data
pred=predict(linearModel, newdata = testdata[,c(4:8,10,24)])
testdata$predictWQI=pred
pred1=predict(linearModel, newdata = traindata[,c(4:8,10,24)])
traindata$predictWQI=pred1
write.csv(traindata,file="TrainDataSet.csv")
write.csv(testdata,file="TestDataSet.csv")



#RMSE
library(Metrics)
rmse(testdata$WQI,pred)


# If you want, say, MAE, you can do the following:

# Function for Mean Absolute Error
mae <- function(error) { mean(abs(error)) }
mae(linearModel$residuals)

#Visualrepresentation of Actual and predicted Data
par(mfrow=c(1,1))
plot(testdata$WQI,col="Blue")
lines(testdata$WQI,col="Red")
plot(pred1,col="Red")
lines(pred1,col="Blue")
lines(testdata$WQI,col="Red")


#MultipleLinearRegression
MulLinmodel=lm(WQI~`TEMPERATURE(c)`+pH+`CONDUCTIVITY(µmhos/cm)`+`B.O.D(mg/l)`+NITRATE+`TOTALCOLIFORM(MPN/100ml)`+I(`TEMPERATURE(c)`^2) +I(pH^2)+I(`CONDUCTIVITY(µmhos/cm)`^2) +I(`B.O.D(mg/l)`^2)+I(NITRATE^2)+I(`TOTALCOLIFORM(MPN/100ml)`^2) ,data =traindata[,c(4:8,10,24)])
summary(MulLinmodel)

#Removing conductivity square
MulLinmodel2=lm(WQI~`TEMPERATURE(c)`+pH+`CONDUCTIVITY(µmhos/cm)`+`B.O.D(mg/l)`+NITRATE+`TOTALCOLIFORM(MPN/100ml)`+I(`TEMPERATURE(c)`^2) +I(pH^2)+I(`B.O.D(mg/l)`^2)+I(NITRATE^2)+I(`TOTALCOLIFORM(MPN/100ml)`^2) ,data =traindata[,c(4:8,10,24)])
summary(MulLinmodel2)

#Performance on test data
pred2=predict(MulLinmodel2, newdata = testdata[,c(4:8,10,24)])

#RMSE                                                     
library(Metrics)
rmse(testdata$WQI,pred2)

# If you want, say, MAE, you can do the following:

# Function for Mean Absolute Error
mae <- function(error) { mean(abs(error)) }
mae(MulLinmodel2$residuals)

#Visualrepresentation of Actual and predicted Data
par(mfrow=c(1,1))
plot(testdata$WQI,col="Blue")
lines(testdata$WQI,col="Red")
plot(pred2,col="Red")
lines(pred2,col="Blue")
lines(testdata$WQI,col="Red")



#Timeseries
installed.packages("timeSeries")
installed.packages("forecast")
installed.packages("MLmetrics")
library(forecast)
library(quantmod)
library(tseries)
library(forecast)
library(timeSeries)
library(xts)
library(MLmetrics)
library(dplyr)
WaterQualityTS=WaterQualityDataSet[,c(11,24)] %>% group_by(YEAR)
WaterTS=WaterQualityTS %>% summarise(
  WQI = mean(WQI)
)

WaterQualityTS=WaterTS[,-1]
WQITS=ts(WaterQualityTS,start=c(2006,1),end=c(2016,12),frequency=12)

# amount of NA
sapply(WQITS,function(x) sum(is.na(x)))

plot(WQITS)
WQIdecomp <- decompose(WQITS)
plot(WQIdecomp)
seasonplot(WQITS)
monthplot(WQITS)

#Test for stationary series

library(tseries)
adf.test(WQITS)

#Deseasonalise 
WQIDes <- stl(WQITS, s.window="p")
WQIDes$time.series
season=WQIDes$time.series[,1]
trend=WQIDes$time.series[,2]
rem=WQIDes$time.series[,3]
WQIDeSeason=trend+rem
ts.plot(WQIDeSeason, WQITS, col=c("red", "blue"), main="Comparison of WQI and WQI")


#Arima
DataTrain<- window(WQITS, start=c(2006,1), end=c(2012,12))
DataTest<- window(WQITS, start=c(2013,1), end=c(2016,12))


#AutoArima
Model=auto.arima((DataTrain),seasonal = TRUE)
plot(Model$residuals)
plot(Model$x,col="blue")
lines(Model$fitted,col="red",main="Actual vs forecasted WQI")
MAPE(Model$fitted ,Model$x)
acf(Model$residuals)
pacf(Model$residuals)

#forecast
ForecastTest=forecast(Model,h=121)
forcastval=cbind(DataTest,ForecastTest$mean)
write.csv(forcastval,file="Timeseries.csv")
ts.plot(forcastval[,1],forcastval[,2],col=c("blue","red"),xlab="Month",ylab="WQI",main="Original vs Forecasted")
MAPE(DataTest,ForecastTest$mean)
RMSE(DataTest,ForecastTest$mean)



