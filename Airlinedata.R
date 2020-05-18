library(readr)
Airline = read.csv(choose.files())
windows()
plot(Airline$Passengers,type = "c")
# creating dummmy variables
pass<-data.frame(outer(rep(month.abb,length = 96),month.abb,"==") + 0)
View(pass)
colnames(pass)<-month.abb
View(pass)
Airdata<-cbind(Airline,pass)
View(Airdata)

colnames(Airdata)[2]<-"Passanger"
colnames(Airdata)
Airdata["t"]<- 1:96
View(Airdata)
Airdata["logpassanger"]<-log(Airdata["Passanger"])
Airdata["t_square"]<-Airdata["t"]*Airdata["t"]
attach(Airdata)
View(Airdata)

###Data partation in train and test data
train<-Airdata[1:84,]
test<-Airdata[85:96,]

############ Linear model#######
linear_model<-lm(Passanger~t,data=train)
summary(linear_model)
## predict model
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test ))
View(linear_pred)
rmse_linear<-sqrt(mean((test$Passanger-linear_pred$fit)^2,na.rm =T))
rmse_linear

######### Exponential ######
expo_model1<-lm(logpassanger~t,data=train)
summary(expo_model1)
### predict exponential model
expo_pred<-data.frame(predict(expo_model1,interval = 'predict',newdata = test))
View(expo_pred)
rmse_expo<-sqrt(mean((test$Passanger-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo

############Quadratic########
Quad_model2<-lm(Passanger~t+t_square,data=train)
summary(Quad_model2)
## predict quadratic data
Quad_pred<-data.frame(predict(Quad_model2 ,interval='predict',newdata = test))
View(Quad_pred)
rmse_Quad<-sqrt(mean((test$Passanger-Quad_pred$fit)^2,na.rm=T))
rmse_Quad

######## additive seasonality
sea_add_model3<-lm(Passanger~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(sea_add_model3)
##Predict 
sea_add_pred<-data.frame(predict(sea_add_model3 ,newdata=test,interval='predict'))
View(sea_add_pred)
rmse_sea_add<-sqrt(mean((test$Passanger-sea_add_pred$fit)^2,na.rm=T))
rmse_sea_add

########################## additvie seasonality with linear #######
Add_sea_Linear_model4<-lm(Passanger~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Linear_model4)
######predict additive seasonality with linear
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model4 ,interval='predict',newdata = test))
rmse_Add_sea_Linear<-sqrt(mean((test$Passanger-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear

#########Additive seasonality with quadratic #####
Add_sea_Quad_model5<-lm(Passanger~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Quad_model5)
####### predict additive seasonality with quadratic
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model5,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Passanger-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad

######## multiplicative seasonality ######

multi_sea_model6<-lm(logpassanger~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(multi_sea_model6)
###### predict multiplicative seasonality
multi_sea_pred<-data.frame(predict(multi_sea_model6,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Passanger-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea


######## multiple seasonality linear tred ######

multi_add_sea_model7<-lm(logpassanger~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(multi_add_sea_model7)
###########prdict multiple seasonality linear tred
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model7 ,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Passanger-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea

###preparing table on model and its RMSE values ####

table_rmse<-data.frame('Model'=c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Linear" ,"rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),
                       'RMSE'=c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_Add_sea_Linear, rmse_multi_add_sea,rmse_multi_sea))
View(table_rmse)
colnames<-c("model","RMSE")
View(table_rmse)

######Additive seasonality with quadratic has least Rmse value 
new_model9 <- lm(Passanger~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=trakdata)
predict(new_model9,n.ahead=1)

### Getting residuals
resid <- residuals(new_model9)
resid[1:10]
windows()
hist(resid)
acf(resid,lag.max = 10)
#by principal of parcimony we will consider lag -1 as we have so many significant lags
#Building Autoregressive model on residual consider lag-1
k <- arima(resid, order =c(1,0,0))
windows()
acf(k$residual,lag.max = 5)
pred_res<- predict(arima(k$residuals,order=c(1,0,0)),n.ahead = 96)
str(pred_res)
pred_res$pred
acf(k$residuals)
write.csv(Airdata,file="Airdata.csv",col.names = F,row.names = F)


#predicting new data ######
install.packages('readxl')
library(readxl)
Airdata<read_excel(file.choose(),1) #Load predict_new.xlsx
View(Airdata)
pred_new<-data.frame(predict(new_model9,newdata=Airdata,interval = 'predict'))
View(pred_new)
pred_re<-pred_res$pred[1:12]
pred_new$fit <- pred_new$fit+pred_res$pred[1:12]
View(pred_new)
