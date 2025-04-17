library(data.table)
library(forecast)
library(base)
install.packages(data.table) #install the required packages
install.packages(forecast)
install.packages("base64")


data=fread("C:/Users/LENOVO/Desktop/trafik.csv",header=TRUE) #read the data 
str(data)


acf(data$traffic_volume,lag.max=100)#there is an increase at lag 24 which is a sign for seasonality of hours

data=as.data.table(data) #convert data to data table
is.data.table(data)
data[,date:=as.POSIXct(data$date_time, format = '%Y-%m-%d ')] #seperate date from datetime
str(data)

hour = format(as.POSIXct(data$date_time,format="%Y-%m-%d %H:%M:%S"),"%H") #seperate hour from datetime
data[,hour:=(hour)]
str(data)
acf(data$traffic_volume,lag.max = 100) 

test_start<-as.Date('2018-01-01') #starting of test period
test_end<-max(data$date)
numbertest<-7949

test_data<-data[date>"2017-12-31"] #seperation of test and train data
train=data[date<"2018-01-01"]

acf(train$traffic_volume,lag.max=100)
plot(train$traffic_volume)

fit1<-lm(train$traffic_volume~temp+date,train) # trials of creating an lm model 
summary(fit1)
fit<-lm(train$traffic_volume~temp+hour+date,train)
summary(fit)
fit2<-lm(train$traffic_volume~temp+hour+date,train)
summary(fit2)
levels(train$holiday) 
levels(train$weather_main)
levels(train$weather_description)
train$holiday <- as.factor(train$holiday) #converting holiday,weather_main,weather_description to factors
train$weather_main<-as.factor(train$weather_main)
train$weather_description<-as.factor(train$weather_description)
str(train)
fit3<-lm(traffic_volume~temp+hour+date+holiday+weather_main+weather_description,train) #first lm model 
summary(fit3)



  
train[,c('Year','Month','Day'):=tstrsplit(date,'-')]
str(train)
##data[, c('Year','Month','Day'):=lapply(.SD, function(x) as.numeric(x)), .SDcols = c('Year','Month','Day')]
str(data)
data_daily=train[,.(traffic_volume=mean(traffic_volume)),by=.(Day,Month,Year)] #creating a daily data
acf(data_daily$traffic_volume,lag.max = 100) # observing an increase in lag 7 which is sign of daily seasonality
train[,weekday:=weekdays(date)]

fit33<-lm(traffic_volume~temp+hour+date+holiday+weather_main+weather_description+weekday,train)
# Second lm model similar to first lm model but weekday is also included
summary(fit33)

#Modeling trend by using linear regression approach
mtrend=train[,list(date_time,traffic_volume,hour)]
mtrend[,index:=1:.N]  #creating an index
str(mtrend)
ltrend<-lm(traffic_volume~index,mtrend) #modeling trend according to index
summary(ltrend) #not a good r-squared but the positive intercept refers to increasing trend
ltrend_comp<-ltrend$fitted
mtrend[,lr_trend:=ltrend_comp]
matplot(mtrend[,list(traffic_volume,lr_trend)],type='l')

#Modeling trend by using moving average approach
window_size=12
mtrend[,ma_trend:=frollmean(traffic_volume,(2*window_size)+1,align='center')]
matplot(mtrend[,list(traffic_volume,ma_trend)],type='l')

matplot(mtrend[,list(traffic_volume,lr_trend,ma_trend)],type='l') #linear modeling trend has a better visualization as trend


train[,trend:=mtrend[,lr_trend]] # adding trend component to trend data 

mtrend[,detrendl:=traffic_volume-lr_trend] #removing trend because it can affects seasonality
mtrend[,detrendma:=traffic_volume-ma_trend]

#modeling seasonlity by using moving average approach
mtrend[,mahour_effect:=mean(detrendma,na.rm=TRUE),by=list(hour)] #creating hour effect
plot(mtrend$mahour_effect[1:120],type='l')
mtrend[,weekday:=train$weekday]
mtrend[,day_effect:=mean(detrendma,na.rm=TRUE),by=list(weekday)] #creating day effect
plot(mtrend$day_effect[1:200],type='l')
mtrend[,indexses:=(index%%168)]
mtrend[,dayhour_effect:=mean(detrendma,na.rm=TRUE),by=list(indexses)] #creating day time effect
plot(mtrend$dayhour_effect[1:120],type='l')
mtrend[,holiday:=train$holiday] #creating holiday effect 
mtrend[,holiday_effect:=mean(detrendma,na.rm=TRUE),by=list(holiday)]
plot(mtrend$dayhour_effect[1:120],type='l')

train[,hour_effect:=mtrend$mahour_effect] #Adding all seasonality related variables to train data 
train[,day_effect:=mtrend$day_effect]
train[,dayhour_effect:=mtrend$dayhour_effect]
train[,holiday_effect:=mtrend$holiday_effect]
# the third lm model
lmson<-lm(traffic_volume~-1+temp+weather_main+weather_description+hour_effect+day_effect+dayhour_effect+holiday_effect+trend,train)
summary(lmson) #this model has a good r-squared


#Applying similar procedures and adding similar variables to test data
test_data[,c('Year','Month','Day'):=tstrsplit(date,'-')]
str(test_data)
str(data)
data_daily2=test_data[,.(traffic_volume=mean(traffic_volume)),by=.(Day,Month,Year)]
acf(data_daily$traffic_volume,lag.max = 100)
test_data[,weekday:=weekdays(date)]


#Modeling trend with linear approach by using test data
mtrend2=test_data[,list(date_time,traffic_volume,hour)]
mtrend2[,index:=1:.N]
str(mtrend2)
ltrend2<-lm(traffic_volume~index,mtrend2)
summary(ltrend2)
ltrend_comp2<-ltrend2$fitted
mtrend2[,lr_trend2:=ltrend_comp2]
matplot(mtrend2[,list(traffic_volume,lr_trend2)],type='l')


#Modeling trend with moving average approach by using test data
window_size=12
mtrend2[,ma_trend2:=frollmean(traffic_volume,(2*window_size)+1,align='center')]
matplot(mtrend2[,list(traffic_volume,ma_trend2)],type='l')

matplot(mtrend2[,list(traffic_volume,lr_trend2,ma_trend2)],type='l')

test_data[,trend:=mtrend2[,lr_trend2]]
mtrend2[,detrendl2:=traffic_volume-lr_trend2]
mtrend2[,detrendma2:=traffic_volume-ma_trend2]

#Modeling seasonality with moving average method by using test data
mtrend2[,mahour_effect2:=mean(detrendma2,na.rm=TRUE),by=list(hour)]
plot(mtrend2$mahour_effect2[1:120],type='l')
mtrend2[,weekday:=test_data$weekday]
mtrend2[,day_effect:=mean(detrendma2,na.rm=TRUE),by=list(weekday)]
plot(mtrend2$day_effect[1:200],type='l')
mtrend2[,indexses:=(index%%168)]
mtrend2[,dayhour_effect:=mean(detrendma2,na.rm=TRUE),by=list(indexses)]
plot(mtrend2$dayhour_effect[1:120],type='l')
mtrend2[,holiday:=test_data$holiday]
mtrend2[,holiday_effect:=mean(detrendma2,na.rm=TRUE),by=list(holiday)]
plot(mtrend$dayhour_effect[1:120],type='l')
#adding seasonality related variables to test data
test_data[,hour_effect:=mtrend2$mahour_effect2]
test_data[,day_effect:=mtrend2$day_effect]
test_data[,dayhour_effect:=mtrend2$dayhour_effect]
test_data[,holiday_effect:=mtrend2$holiday_effect]




#Making predictions of test data by using these three model

p<-predict(fit3,test_data)
r<-predict(lmson,test_data)
s<-predict(fit33,test_data)
p[1:24]
r[1:24]
test_data[,fit3pred:=p]
test_data[,lmsonpred:=r]
test_data[,fit33pred:=s]
ts.plot(test_data$traffic_volume) #visualization of predictions and real values
lines(test_data$fit3pred,col="blue")
lines(test_data$lmsonpred,col="brown")
lines(test_data$fit33pred,col="orange")
#calculating mse,mad,mape
pred_table=test_data[,.(date_time,traffic_volume,fit3pred,lmsonpred,fit33pred)]
melted=melt(pred_table,id.vars=c(1,2),na.rm=TRUE)
result=melted[ ,list(se=(value-traffic_volume)^2,mse=(value-traffic_volume)^2,
                            ape=abs(value-traffic_volume)/traffic_volume,traffic_volume),by=list(date_time,variable)]
result=result[,list(mse=mean(se,na.rm=TRUE),mad=mean(mse,na.rm=TRUE),mape=mean(ape,na.rm=TRUE)),by=list(variable)]
result



  #Arima
  
train_ts<-ts(train[,c("traffic_volume")],frequency=24) #creating an ts object
arima_model<-auto.arima(train_ts) #modeling this time series object by using arima (first arima model)
summary(arima_model) 
checkresiduals(arima_model)
arimamodel_fitted<-train_ts-residuals(arima_model)
ts.plot(train_ts)
points(arimamodel_fitted,col="blue")
prediction<-forecast(arima_model,h=24)
plot(prediction)

ara<-test_data[1:24,]
ara<-as.data.table(ara)
ara[,arima_prediction:=as.vector(prediction$mean)]
ts.plot(ara$traffic_volume)
lines(ara$arima_prediction,col="blue")
ts2<-ts(train[,c("traffic_volume")])


matris3<-as.data.table(train[,c('temp','day_effect','holiday_effect')]) #creating a matrix for regressors  
nrow(matris3)
z_mat1<-as.matrix(matris3)
z_mat<- matrix(unlist(z_mat1), nrow =40255 )

matris2<-matrix(test_data[,c('temp','day_effect','holiday_effect')]) #creating a matrix for regressors of test data
str(matris)
z_mat2 <- matrix(unlist(matris2), nrow = 7949)



reg_arima<-auto.arima(train_ts,xreg=as.matrix(z_mat)) # Arima model with regressors(second arima model)
summary(reg_arima)
checkresiduals(reg_arima)
arimareg_fitted<-train_ts-residuals(reg_arima)
ts.plot(train_ts)
points(arimareg_fitted,type="l")

regpredicted<-forecast(reg_arima,h=24,xreg=as.matrix(z_mat2))
plot(regpredicted)
test_data[,predictreg:=as.vector(regpredicted$mean)]
ts.plot(test_data$traffic_volume)
lines(test_data$predictreg,col="red")

k=forecast(arima_model,h=7949) #forecasting with first arima model
l=forecast(reg_arima,h=7949,xreg=as.matrix(z_mat2)) #forecasting with second arima model

test_data[,arimap:=as.vector(k$mean)]
test_data[,regarimap:=as.vector(l$mean)]
ts.plot(test_data$traffic_volume)
lines(test_data$arimap,col="red")
lines(test_data$regarimap,col="green")
 #calculatin mse,mad,mape for these two arima models 
pred2=test_data[,.(date_time,traffic_volume,arimap,regarimap)]
melted2=melt(pred2,id.vars=c(1,2),na.rm=TRUE)
res2=melted2[,list(se=(value-traffic_volume)^2,
                            ad=abs(value-traffic_volume),
                            ape=abs(value-traffic_volume)/traffic_volume,traffic_volume),by=list(date_time,variable)]

res2=res2[,list(mse=mean(se,na.rm=TRUE),mad=mean(ad,na.rm=TRUE),mape=mean(ape,na.rm=TRUE)),by=list(variable)]
res2


##task 3
sontable<-data.table()
houref<-test_data[,"hour_effect"] #since hour effect repeats every 24 hour
phour<-houref[1:24]
sontable[ ,hour_effect:=phour]
dayef<-test_data[,"day_effect"] #day effects repeats every week and my prediction will be monday so I take the effect of monday
pday<-dayef[1:24]
sontable[ ,day_effect:=pday]
dayhouref<-test_data[,"dayhour_effect"] #day hour effects repeat every 168 hour 
dayhef<-dayhouref[1:24] 
sontable[ ,dayhour_effect:=dayhef]
holidayef<-test_data[,"holiday_effect"]
phol<-holidayef[2:25]
#since the date that will made prediction is not holiday effect equal to none holiday effect
sontable[ ,holiday_effect:=phol]
#naive forecast has been made for the temperature,weather_main,weather_description,trend
te<-test_data[,"temp"]
ptemp<-te[7926:7949] 
sontable[ ,temp:=ptemp]   #temperature of day before the forecast day 
wem<-test_data[,"weather_main"]
pwem<-wem[7926:7949]
sontable[ ,weather_main:=pwem]  #weather main of the day before the forecast day 
wed<-test_data[,"weather_description"]
pwed<-wed[7926:7949]
sontable[ ,weather_description:=pwed] #weather description of the day before the forecast day 
tred<-test_data[,"trend"]
ptrend<-tred[7926:7949]     
sontable[ ,trend:=ptrend] #trend of the day before forecast day 
ptr<-test_data[,"traffic_volume"]
ptraff<-ptr[7926:7949]
sontable[ ,traffic_volume:=ptraff]


phour2<-test_data[,"hour"] 
p32<-phour2[7926:7949]
sontable[ ,hour:=p32] #adding hour information
pholiday2<-test_data[,"holiday"]
h32<-pholiday2[7926:7949]
sontable[ ,holiday:=h32]  #adding holiday information
tar2<-as.Date("2018-10-01")
tar32<-rep(tar2,24)
sontable[ ,date:=tar32]
sontable[,weekday:="Pazartesi"] #adding weekday information

pa1<-predict(lmson,sontable) #prediction with the third lm model 
pa2<-predict(fit3,sontable) #prediction with the first lm model 
pa3<-predict(fit33,sontable) #prediction with the second lm model 

ts3<-ts(sontable[,"traffic_volume"])


pp4<-forecast(arima_model,h=24) 
pa4<-as.vector(pp4$mean) #prediction with the first arima model 

matris3<-matrix(sontable[,c('temp','day_effect','holiday_effect')]) #creating a matrix for regressors of forecast data
str(matris3)
matris3<as.numeric(matris3)
z_mat3 <- matrix(unlist(matris3), nrow = 24)
 
pp5<-forecast(reg_arima,h=24,xreg=as.matrix(z_mat3)) 
pa5<-as.vector(pp5$mean) #prediction with the second arima model 


myfor<-pa1+pa2+pa3+pa4+pa5
my_forecasts<-myfor/5 #average of predictions of these 5 model 
 my_forecasts #my forecasts 




  


