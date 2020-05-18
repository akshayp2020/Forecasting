## platics sales data
Plastic = read.csv (choose.files())
plot(Plastic$Sales,type='l')

# So creating 12 dummy variables 
X <- data.frame(outer(rep(month.abb,length=60),month.abb,"==")+0)
View(X)
colnames(X)<-month.abb
View(X)
plasticdata <- cbind(Plastic,X)
View(plasticdata)

plasticdata["t"]<-1:60
View(plasticdata)
plasticdata["logsales"] <- log(plasticdata["Sales"])
plasticdata["tsquare"] <- plasticdata["t"]*plasticdata["t"]
attach(plasticdata)
View(plasticdata)

#sepret data in to Training And Testing
train<-plasticdata[1:48,]
test <- plasticdata[49:60,]

#####Linear model####
lin_model1 <- lm(Sales~t,data=train)
summary(lin_model1)
#Predict linear model
lin_pred<-data.frame(predict(lin_model1,interval = 'predict',newdata = test))
View(lin_pred)
rmse_lin<-sqrt(mean((test$Sales-lin_pred$fit)^2,na.rm=T))
rmse_lin 

########Exponential Model#########
expo_model2 <- lm(logsales~t,data = train)
summary(expo_model2)
#Predict exponantial model 
expo_pred <- data.frame(predict(expo_model2,interval = 'predict',newdata = test))
View(expo_pred)
rmse_expo<-sqrt(mean((test$Sales-exp(expo_pred$fit))^2,na.rm=T))
rmse_expo 

##########Quadrartic Model######
quad_model3 <- lm(Sales~t+tsquare,data = train)
summary(quad_model3)
#Predict quadratic model
quad_pred <- data.frame(predict(quad_model3,interval = 'predict',newdata = test))
View(quad_pred)
rmse_quad<-sqrt(mean((test$Sales-quad_pred$fit)^2,na.rm = T))
rmse_quad 

############Additive Seasonality########
sea_add_model4 <- lm(Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = train)
summary(sea_add_model4)
#Predict additive seasonality model 
sea_add_pred <- data.frame(predict(sea_add_model4,interval = 'predict',newdata = test))
View(sea_add_pred)
rmse_sea_add <- sqrt(mean((test$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add 


########Additive Seasonality with linear######
add_sea_lin_model5 <- lm(Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = train)
summary(add_sea_lin_model5)
#Predict additive seasonaltiy with linear
add_sea_lin_pred<- data.frame(predict(add_sea_lin_model5,interval = 'predict',newdata = test))
View(add_sea_lin_pred)
rmse_add_sea_lin<- sqrt(mean((test$Sales-add_sea_lin_pred$fit)^2,na.rm=T))                               
rmse_add_sea_lin    

#########Additive Seasonality with quadratic########
add_sea_quad_model6 <- lm(Sales~t+tsquare+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data=train)
summary(add_sea_quad_model6)

#Predict additive seasonaltiy with quadratic 
add_sea_quad_pred<-data.frame(predict(add_sea_quad_model6,interval = 'predict',newdata = test))
View(add_sea_quad_pred)
rmse_add_sea_quad<-sqrt(mean((test$Sales-add_sea_quad_pred$fit)^2,na.rm=T))
rmse_add_sea_quad


######################## Multiplicative Seasonality #########################

multi_sea_model7<-lm(logsales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = train)
summary(multi_sea_model7)
#Predict multipicative seasonaltiy model
multi_sea_pred<-data.frame(predict(multi_sea_model7,newdata=test,interval='predict'))
View(multi_sea_pred)
rmse_multi_sea<-sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea 

##################Multiplicative Seasonality with Linear Trend########
multi_sea_lin_model8 <- lm(logsales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = train)
summary(multi_sea_lin_model8)
#Predict multiplicative seasonality with linear trend 
multi_sea_lin_pred <- data.frame(predict(multi_sea_lin_model8,interval = 'predict',newdata = test))
View(multi_sea_lin_pred)
rmse_multi_sea_lin<-sqrt(mean((test$Sales-exp(multi_sea_lin_pred$fit))^2,na.rm = T))
rmse_multi_sea_lin

# Rmse value table
table_rmse <-data.frame(c("rmse_lin","rmse_expo","rmse_quad","rmse_sea_add","rmse_add_sea_lin","rmse_add_sea_quad","rmse_multi_sea","rmse_multi_sea_lin"),c(rmse_lin,rmse_expo,rmse_quad,rmse_sea_add,rmse_add_sea_lin,rmse_add_sea_quad,rmse_multi_sea,rmse_multi_sea_lin))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)


#Multiplicative Seasonality with Linear has less rmse 
#New Model
nmodel1 <- lm(logsales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = plasticdata)
nmodel_pred <-data.frame(predict(nmodel1,interval = 'predict',newdata = plasticdata))


nmodel_fin <- exp(nmodel$fitted.values)
View(nmodel_fin)

month <-as.data.frame(plasticdata$Month)

final<-as.data.frame(cbind(month,plasticdata$Sales,nmodel_fin))
colnames(final) <-c("Month","Sales","New_Pred_Value")

plot(final$Sales,main = "ActualGraph", xlab="Sales(Actual)", ylab="Months",
     col.axis="blue",type="o") 

plot(final$New_Pred_Value, main = "PredictedGraph", xlab="Sales(Predicted)", ylab="Months",
     col.axis="Green",type="s")
View(final)
