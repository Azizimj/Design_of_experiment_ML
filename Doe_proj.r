rm(list=ls())
graphics.off()
setwd("F:/term1/DoE/Proj")

###########
# reading and preparation of data
orginal=read.csv("energydata_complete.csv",as.is = T,header = T,stringsAsFactors = T)
#str(orginal$date)
orginal$date_day=as.Date(orginal$date,format = "%Y-%m-%d")
#summary(orginal$date_day)#orginal[["date_day"]][1]-orginal[["date_day"]][1]
orginal$time=format(as.POSIXct(strptime(orginal$date,"%Y-%m-%d %H:%M:%S",tz="GMT")) ,format = "%H:%M:%S")
#summary(orginal$time)#orginal[["time"]][1];orginal[["time"]][1]<orginal[["time"]][2]#orginal[["time"]][1]<as.Date("2016-01-18 12:20:00")
install.packages("lubridate")
library("lubridate")
orginal$NSM = period_to_seconds(hms(orginal[["time"]]))
orginal$WeekDay = as.POSIXlt(strptime(orginal$date_day,"%Y-%m-%d",tz="GMT"))$wday #week days 0-6 starting Sunday
orginal$WeekStatus[orginal$WeekDay == 0 | orginal$WeekDay == 6] = "weekend"
orginal$WeekStatus[orginal$WeekDay != 0 & orginal$WeekDay != 6] = "workday"
orginal$WeekStatus = as.factor(orginal$WeekStatus)

#############
# train and test sets
set.seed(1)
train_index <- sample(1:nrow(orginal),round(0.75*nrow(orginal)))
orginal_train=orginal[train_index,]
orginal_test=orginal[-train_index,]
#scaling the data
orginal1=orginal[,-which(names(orginal) %in% c("date","time","date_day","WeekDay","WeekStatus"))] # for scaling put aside factors(in addition, these factors are unimportant in lm)
orginal1_train = orginal1[train_index,]
orginal1_test = orginal1[-train_index,]
maxs <- apply(orginal1,2, max) 
mins <- apply(orginal1,2, min)
orginal1_scale = as.data.frame(scale(orginal1, center = mins, scale = maxs - mins))
orginal1_train_scale = orginal1_scale[train_index,]
orginal1_test_scale = orginal1_scale[-train_index,]

##Pair wise plots  # data visulization (not complete)
ggcorplot(orginal[,which(names(orginal) %in% c("Appliances","lights","T1","RH_1","T_out"))] 
            ,var_text_size = 5,cor_text_limits = c(5,10))
pairs(orginal[,which(names(orginal) %in% c("Appliances","lights","T1","RH_1","T_out"))] )
############
# Split plot
summary(orginal$RH_out)
summary(orginal$T_out)
summary(subset(orginal$date_day,orginal$T_out==6))
str(subset(orginal$date_day,orginal$T_out==5))

#########
# ANOVCOV
summary(orginal$lights)
#orginal1=orginal[,-which(names(orginal) %in% c("date","time","date_day"))]
#orginal1=orginal1[1:5000,]
lm1=lm(orginal1$Appliances~ .-orginal1$Windspeed-orginal1$RH_out-orginal1$Visibility
       -orginal1$T_out-orginal1$T_out -date-time-date_day , orginal1)
summary(lm1)

###########
## blocking
names(orginal)
summary(orginal$T1)
summary(orginal$RH_1)
a=subset(orginal, orginal$T1 >= 16 & orginal$T1<= 20 )
summary(a$RH_1)

#######################################
## Variable selection and feature filter

# Boruta
install.packages("Boruta")
library(Boruta)
set.seed(1)
boruta.train <- Boruta(Appliances~.-date-time-date_day,orginal_train,pValue= 0.01, doTrace = 2) #time consuming command
print(boruta.train)
boruta.train$finalDecision
#boruta.train$ImpHistory
#boruta.train$impSource
plot(boruta.train)

####################
# Linear Regression
#install.packages("leaps")
library(leaps)
sub=regsubsets(Appliances ~ .-date-time-date_day,orginal_train)
plot(sub,scale = "Cp",main = 'cp') 
plot(sub,scale = "adjr2",main = 'adjr2')
plot(sub,scale = "bic",main = "bic")

#stepwise
null=lm(Appliances~1, orginal)
full=lm(Appliances~.-date-time-date_day, orginal_train) 
#step_for = step(null, scope=list(lower=null, upper=full),direction="forward") 
#step_back = step(full, scope=list(lower=null, upper=full),data=d,direction="backward")
step_both = step(null, scope = list(upper=full), data=orginal_train, direction="both") 
#summary(step_for)#summary(step_back)
summary(step_both)
step_both$anova # result is saved

# CARET to find correlations and important ones
install.packages("mlbench")
install.packages("caret")
install.packages("recipes")
library(recipes)
library(mlbench)
library(caret)
correlationMatrix <- cor(orginal1[,2:29])
#highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)

model <- train(Appliances~., data=orginal1, method="", preProcess="scale", trControl=control)


mdl=lm(Appliances ~ NSM+lights+RH_out+RH_1+RH_7+RH_2+RH_8 ,orginal_train )
summary(mdl) # Most of them are important 
plot(mdl) # the residuals have non-normal behavior

mdl=lm(log(Appliances) ~ NSM+lights+RH_out+RH_1+RH_7+RH_2+RH_8 ,orginal_train )
summary(mdl) # less residuals so we can USE LOG (appliances)
plot(mdl)


############
# NN
install.packages("neuralnet")
library("neuralnet")
set.seed(1)
NN = neuralnet(Appliances ~ NSM+lights+RH_out+RH_1+RH_7+RH_2+RH_8
               ,orginal1_train_scale , hidden = c(3,3) , linear.output = T,stepmax = 10 ) #important features of regsubsets is used
summary(NN)
plot(NN)

predict_testNN = compute(NN, orginal1_test_scale)
predict_testNN = (predict_testNN$net.result * (max(orginal1$Appliances) - min(orginal1$Appliances))) + min(orginal1$Appliances)
#plot(datatest$rating, predict_testNN, col='blue', pch=16, ylab = "predicted rating NN", xlab = "real rating")
#abline(0,1)
# Calculate Root Mean Square Error (RMSE)
RMSE.NN = (sum((orginal1_test$Appliances - predict_testNN)^2) / nrow(orginal1_test)) ^ 0.5


#############
# SVM
install.packages("e1071")
library(e1071)
svm1=svm(Appliances ~NSM+lights+RH_out+RH_1+RH_7+RH_2+RH_8 ,orginal1_train_scale )
summary(svm1$residuals)
pred_svm1 <- predict(svm1, orginal1_test_scale)
pred_svm1 = (pred_svm1 * (max(orginal1$Appliances) - min(orginal1$Appliances))) + min(orginal1$Appliances)
RMSE.svm1 = (sum((orginal1_test$Appliances - pred_svm1)^2) / nrow(orginal1_test)) ^ 0.5
RMSE.svm1

# RF
#install.packages("randomForest")
require(randomForest)
RF = randomForest(Appliances ~ . , data = orginal1_train)
plot(RF)
# RMSE
pred_rf<-predict(rf,orginal1_test)
rf_test_err= with(orginal1_test, mean( (Appliances - pred_rf)^2)) 

# GBM
#install.packages("gbm")
#install.packages("cvAUC")
library(gbm)
library(cvAUC)
gbm_model <- gbm(Appliances ~ ., distribution = "bernoulli",data = orginal1_train ,n.trees = 300,interaction.depth = 5,shrinkage = 0.3,bag.fraction = 0.5,train.fraction = 1.0,n.cores = NULL)
print(model)
preds_gbm <- predict(gbm_model, newdata = orginal1_test, n.trees = 300)
labels <- orginal1_test[,"Appliances"]
# Compute AUC on the test set
cvAUC::AUC(predictions = preds, labels = labels)
#RMSE
gbm_test_err = with(orginal1_test, mean( (Appliances - pred_gbm)^2)) 


############################
## data leveling
orginal_leveled = orginal

summary(orginal_leveled$T1)
orginal_leveled$T1_l =""
orginal_leveled$T1_l[orginal_leveled$T1<=20]="low"
orginal_leveled$T1_l[orginal_leveled$T1>20 & orginal_leveled$T1<=23]="med"
orginal_leveled$T1_l[orginal_leveled$T1>23]="high"
orginal_leveled$T1_l = as.factor(orginal_leveled$T1_l)
summary(orginal_leveled$T1_l)

summary(orginal_leveled$RH_1)
orginal_leveled$RH_1_l =""
orginal_leveled$RH_1_l[orginal_leveled$RH_1<=40]="low"
orginal_leveled$RH_1_l[orginal_leveled$RH_1>40 & orginal_leveled$RH_1<=50]="med"
orginal_leveled$RH_1_l[orginal_leveled$RH_1>50]="high"
orginal_leveled$RH_1_l = as.factor(orginal_leveled$RH_1_l)
summary(orginal_leveled$RH_1_l)

#to do in this way
