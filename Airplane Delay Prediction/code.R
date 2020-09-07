setwd("C:/Users/ramte/Desktop/ADM/AirPlane delay Prediction")
data1 <- read.csv("flights_2017.csv", stringsAsFactors = T)
data2 <- read.csv("flights_2018.csv", stringsAsFactors = T)


data3 <- rbind(data1, data2) 
write.csv(file="data3.csv", data3, row.names = F)
data3 <- read.csv("data3.csv",stringsAsFactors = T)
View(data3)



data3[data3 ==" "] <- NA
delete_data3 <- data3[complete.cases(data3), ]
summary(delete_data3)

data3 <- delete_data3 
na.zero <- function(x) {
  x[is.na(x)] <- 0
  return(x)
}

View(data3)

########################################################## KNN ###################################################################

data3$DEP_DELAY_CAT[data3$DEP_DELAY < 0] <- "EARLY"
data3$DEP_DELAY_CAT[data3$DEP_DELAY == 0] <- "ONTIME"
data3$DEP_DELAY_CAT[data3$DEP_DELAY >0 & data3$DEP_DELAY <= 15] <- "LATE"
data3$DEP_DELAY_CAT[data3$DEP_DELAY >15] <- "VERY_LATE"

data3$ARR_DELAY_CAT[data3$ARR_DELAY < 0] <- "EARLY"
data3$ARR_DELAY_CAT[data3$ARR_DELAY == 0] <- "ONTIME"
data3$ARR_DELAY_CAT[data3$ARR_DELAY > 0 & data3$ARR_DELAY <= 15] <- "LATE"
data3$ARR_DELAY_CAT[data3$ARR_DELAY >15] <- "VERY_LATE"

View(data3)

attach(data3)
data3_new <- data.frame(DAY_OF_MONTH,DAY_OF_WEEK,DEP_TIME,CRS_DEP_TIME,ARR_TIME,CRS_ARR_TIME,UNIQUE_CARRIER,ORIGIN,DEST,DISTANCE,DIVERTED,CANCELLED,CARRIER_DELAY,WEATHER_DELAY,NAS_DELAY,SECURITY_DELAY,LATE_AIRCRAFT_DELAY,ARR_DELAY,DEP_DELAY,"ARR_DELAY_CAT"=data3$ARR_DELAY_CAT,"DEP_DELAY_CAT"=data3$DEP_DELAY_CAT,TAXI_IN,TAXI_OUT,WHEELS_OFF,WHEELS_ON)
#detach (odataset)
#attach(data3_new)
#we are filtering out the flights which are cancelled
data3_new<-data3_new[data3_new[,"CANCELLED"]==0,]
#we are filtering out the flights which are Diverted
data3_new<-data3_new[data3_new[,"DIVERTED"]==0,]
#we are filtering out the flights which are latedeparture
data3_new<-data3_new[data3_new[,"DEP_DELAY"]<=5,]

#*****we are taking 3 highest freq unique carrier
#data3_new<-data3_new[data3_new$DEST =="ATL" | data3_new$DEST =="ORD" | data3_new$DEST =="LAX" | data3_new$DEST =="SFO", ] 
#*****we are taking 3 highest freq ORIGIN
data3_new<-data3_new[data3_new$ORIGIN =="ATL" | data3_new$ORIGIN =="ORD" | data3_new$ORIGIN =="LAX" | data3_new$ORIGIN =="DFW", ] 
View(data3_new)
#removing arrival time NA
#data3_new<-data3_new[complete.cases(data3_new[,6]),]
attach(data3_new)
library(plyr)
data<-join(data3_new,count(data3_new,'DEST'))
View(data)
summary(table(data$freq))
attach(data3_new)
#*******categoraizing dest type according to the num of flights
data3_new$Dest_Type[data$freq > 1000 & data$freq <= 2000] <- "Med Busy"
data3_new$Dest_Type[data$freq >0 & data$freq <= 1000] <- "Less Busy"
data3_new$Dest_Type[data$freq >2000] <- "High Busy"
View(data3_new)
count(data3_new$Dest_Type)
#detach(odataset)
#************normalization  & removal of na values functions ***********
na.zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}
mmnorm <-function(x) {z<-((x-min(x))/(max(x)-min(x)));return(z) }
data<-data3_new
data_new<-cbind(#Month=mmnorm(data$Month),
  TAXI_IN=mmnorm(data$TAXI_IN),
  TAXI_OUT=mmnorm(data$TAXI_OUT),
  WHEELS_OFF=mmnorm(data$WHEELS_OFF),
  WHEELS_ON=mmnorm(data$WHEELS_ON),
  DAY_OF_MONTH=mmnorm(data$DAY_OF_MONTH),
  WEATHER_DELAY=mmnorm(data$WEATHER_DELAY),
  NAS_DELAY=mmnorm(data$NAS_DELAY),
  SECURITY_DELAY=mmnorm(data$SECURITY_DELAY),
  LATE_AIRCRAFT_DELAY=mmnorm(data$LATE_AIRCRAFT_DELAY),
  DAY_OF_WEEK=mmnorm(data$DAY_OF_WEEK),
  CRS_DEP_TIME=mmnorm(data$CRS_DEP_TIME),
  CRS_ARR_TIME=mmnorm(data$CRS_ARR_TIME) ,
  UNIQUE_CARRIER=mmnorm(as.numeric(factor(data$UNIQUE_CARRIER))),
  ORIGIN=mmnorm(as.numeric(factor(data$ORIGIN))),
  Dest_Type=mmnorm(as.numeric(factor(data$Dest_Type))),
  ARR_DELAY_CAT=as.numeric(data$ARR_DELAY_CAT)
)
#*****taking 5000 entries*******
View(data_new)
# FOr ANN
data_new<-as.data.frame (data_new)
data_new<-na.omit(data_new)
factor(data_new$ARR_DELAY_CAT)
is.data.frame(data_new)
idx1<-seq(1:10000)
data_new<-data_new[idx1,]
idx<-sample(nrow(data_new),as.integer(.70*nrow(data_new)))
training<-data_new[idx,]
test<-data_new[-idx,]
is.data.frame(training)
if(!require(neuralnet)) {
  install.packages("neuralnet"); require(neuralnet)}


net.ArrDelay <- neuralnet(ARR_DELAY_CAT~DAY_OF_WEEK+CRS_DEP_TIME+CRS_ARR_TIME+
                            UNIQUE_CARRIER+ORIGIN+Dest_Type+SECURITY_DELAY+WEATHER_DELAY+NAS_DELAY,
                          training,hidden=5, threshold=0.01,stepmax=1e6)

summary(net.ArrDelay)
plot(net.ArrDelay)








#****sampling data**************
idx<-sample(nrow(data_new),as.integer(.70*nrow(data_new)))
#****training  & test dataset***********
training<-data_new[idx,]
test<-data_new[-idx,]
View(training)
names(training)
library(class)
####to find proper k value::
#running knn 50 time for itterative  k   starting from k=1 to k=20 
# here which k's average error rate is minimumm,that k is best.
for (j in 30:40){
  counter<- 0
  total<-0
  for (i in 1:50) {
    newpredict<-knn(training[,-16],test[,-16],training[,16],k <- j)
    newresults<-cbind(test,as.character(newpredict) )
    wrong<-newresults[,16]!=newresults[,17]
    rate<-sum(wrong)/length(wrong)
    rates<-rbind(rate,rate)
    total<-total+rate
    counter<-counter+1
  }
  print(j)
  avg=total/counter
  print(avg)
} 
plot(newpredict)

newpredict<-knn(training[,-16],test[,-16],training[,16],k=31)
newresults<-cbind(test,as.character(newpredict) )
head(newresults)
table(newresults[,16],newresults[,17])


############################################################ C50 Decision Tree ############################################################# 

data3_new <- data
library(plyr)
View(data3_new)
#defining the function for  normalization
mmnorm<-function(x)
{
  z<-((x-min(x))/(max(x)-min(x)))
  return(z)
}

#defining the na.zero function
na.zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}

View(data3_new)

data3_new<-sample(data)
data3_new$CARRIER_DELAY[is.na(data3_new$CARRIER_DELAY)] <- 0
data3_new$WEATHER_DELAY[is.na(data3_new$WEATHER_DELAY)] <- 0
data3_new$NAS_DELAY[is.na(data3_new$NAS_DELAY)] <- 0
data3_new$SECURITY_DELAY[is.na(data3_new$SECURITY_DELAY)] <- 0
data3_new$LATE_AIRCRAFT_DELAY[is.na(data3_new$LATE_AIRCRAFT_DELAY)] <- 0
attach(data3_new)
#******categorizing Day Of month
data3_new$DayofMonth_cat[DAY_OF_MONTH > 15] <- "SECOND_HALF"
data3_new$DayofMonth_cat[DAY_OF_MONTH <= 15] <- "FIRST_HALF"
#******categorizing Day of week
data3_new$week_cat[DAY_OF_WEEK == 1 | DAY_OF_WEEK ==7] <- "WEEKEND"
data3_new$week_cat[DAY_OF_WEEK < 7 & DAY_OF_WEEK > 1] <- "WEEKDAY"
#******categorizing Arrival Delay
data3_new$ARR_DELAY_CAT[ARR_DELAY < 0] <- "EARLY"
data3_new$ARR_DELAY_CAT[ARR_DELAY > 0 & ARR_DELAY <= 15] <- "LATE"
data3_new$ARR_DELAY_CAT[ARR_DELAY == 0] <- "ONTIME"
data3_new$ARR_DELAY_CAT[ARR_DELAY > 15] <- "VERY_LATE"
#******categorizing Distance
data3_new$Distance_cat[DISTANCE > 0 & DISTANCE <= 1000] <- "SHORT_DISTANCE"
data3_new$Distance_cat[DISTANCE > 1000 & DISTANCE <=  2000] <- "MID_DISTANCE"
data3_new$Distance_cat[DISTANCE > 2000] <- "LONG_DISTANCE"
data3_new<-join(data3_new,count(data3_new,'DEST'))
#******categorizing Destination
View(data3_new)
data3_new <- data3_new[,-29]
data3_new$Dest_Type[data$freq > 1000 & data$freq <= 2000] <- "Med Busy"
data3_new$Dest_Type[data$freq >0 & data$freq <= 1000] <- "Less Busy"
data3_new$Dest_Type[data$freq >2000] <- "High Busy"
#******categorizing Arrival Delay value
data3_new$ArrDelay_value[data3_new$ARR_DELAY_CAT == "EARLY"] <- 1
data3_new$ArrDelay_value[data3_new$ARR_DELAY_CAT == "ONTIME"] <- 2
data3_new$ArrDelay_value[data3_new$ARR_DELAY_CAT == "LATE"] <- 3
data3_new$ArrDelay_value[data3_new$ARR_DELAY_CAT == "VERY_LATE"] <- 4

data <-data3_new
data_new<-cbind(#Month=mmnorm(data$Month),
  TAXI_IN=data$TAXI_IN,
  TAXI_OUT=data$TAXI_OUT,
  WHEELS_OFF=data$WHEELS_OFF,
  WHEELS_ON=data$WHEELS_ON,
  DAY_OF_MONTH=data$DAY_OF_MONTH,
  WEATHER_DELAY=data$WEATHER_DELAY,
  NAS_DELAY=data$NAS_DELAY,
  SECURITY_DELAY=data$SECURITY_DELAY,
  LATE_AIRCRAFT_DELAY=data$LATE_AIRCRAFT_DELAY,
  DAY_OF_WEEK=data$DAY_OF_WEEK,
  CRS_DEP_TIME=data$CRS_DEP_TIME,
  CRS_ARR_TIME=data$CRS_ARR_TIME,
  UNIQUE_CARRIER=data$UNIQUE_CARRIER,
  ORIGIN=data$ORIGIN,
  Dest_Type=data$Dest_Type,
  ARR_DELAY_CAT=as.numeric(factor(data$ARR_DELAY_CAT))
)
View(data_new)



detach (data3_new)
attach(data_new)
#install.packages("C50")
require(C50)
set.seed(9850)
idx<-sample(nrow(data_new),as.integer(.70*nrow(data_new)))
#generating training dataset
training<-data_new[idx,]
nrow(training)
#generating test dataset
test<-data_new[-idx,]
#applying C5.0
m1 <- C5.0(training[,-16],factor(training[,16]))
#gives summary of the tree
summary(m1)
#plotting the tree
plot(m1)


#to  check  the accuracy of model
result<-predict(m1,test,type="class")
rTable<-table(predict=result,test=test[,16])
accuracy=(sum(diag(rTable))/nrow(test))
accuracy

#################################################################### ANN #######################################################################

data3_new <- data
library(plyr)

mmnorm <-function(x) {z<-((x-min(x))/(max(x)-min(x)));return(z) }

na.zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}

data_new<-cbind(#Month=mmnorm(data$Month),
  TAXI_IN=data$TAXI_IN,
  TAXI_OUT=data$TAXI_OUT,
  WHEELS_OFF=data$WHEELS_OFF,
  WHEELS_ON=data$WHEELS_ON,
  DAY_OF_MONTH=data$DAY_OF_MONTH,
  WEATHER_DELAY=data$WEATHER_DELAY,
  NAS_DELAY=data$NAS_DELAY,
  SECURITY_DELAY=data$SECURITY_DELAY,
  LATE_AIRCRAFT_DELAY=data$LATE_AIRCRAFT_DELAY,
  DAY_OF_WEEK=data$DAY_OF_WEEK,
  CRS_DEP_TIME=data$CRS_DEP_TIME,
  CRS_ARR_TIME=data$CRS_ARR_TIME,
  ARR_DELAY_CAT=as.numeric(factor(data$ARR_DELAY_CAT))
)


idx<-sample(nrow(data_new),as.integer(.70*nrow(data_new)))
training<-data_new[idx,]
test<-data_new[-idx,]
is.data.frame(training)
if(!require(neuralnet)) {
  install.packages("neuralnet"); require(neuralnet)}
rate<-0
wrong<-0
net.ArrDelay <- neuralnet(ARR_DELAY_CAT~TAXI_IN+TAXI_OUT+WHEELS_OFF+WHEELS_ON+DAY_OF_MONTH+DAY_OF_WEEK+WEATHER_DELAY+NAS_DELAY+SECURITY_DELAY+LATE_AIRCRAFT_DELAY+CRS_ARR_TIME+CRS_DEP_TIME,
                          training,hidden=8, threshold=0.01,stepmax=1e6)
#to plot the neuralnet
plot(net.ArrDelay)
net.result1 <- compute(net.ArrDelay, subset(test, select=-ArrDelay_cat))
fit <- round(net.result1$net.result, digits = 0)
results <- cbind(test$ArrDelay_cat, fit)
wrong <- results[,1]!=results[,2]
rate <- sum(wrong)/length(wrong)
rate




