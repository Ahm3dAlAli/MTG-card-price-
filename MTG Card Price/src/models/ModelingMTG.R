install.packages("data.table")
library(data.table)
library(readr)
library(caret)
library(Metrics)
library(glmnet)
library(plotmo)
library(lubridate)
############################################################################################################################

##########################
# Prep Data for Modeling #
##########################
train_data <- data.table(read_csv("project/volume/data/processed/Clean_train.csv"))
testdata<-data.table(read_csv("project/volume/data/processed/Clean_test.csv"))

##############################################################################################################################
test_id1<-testdata

unique(train_data$current_date)
unique(testdata$current_date)


train_1<-train_data[year(current_date)<2018]
test_1<-train_data[year(current_date)==2018]

train_2<-train_data[year(current_date)<2019]
test_2<-train_data[year(current_date)==2019]

train_3<-train_data[year(current_date)<2020]
test_3<-train_data[year(current_date)==2020]

#sapply(train_3,function(x) sum(is.na(x)))

# subset out only the columns to model

drops<-c("id","current_date","future_date","card_name","set","set_name","train")

traintes<-train_data[, !drops, with = FALSE]
testtes<-testdata[, !drops, with = FALSE]

train_1<-train_1[, !drops, with = FALSE]
test_1<-test_1[, !drops, with = FALSE]

train_2<-train_2[, !drops, with = FALSE]
test_2<-test_2[, !drops, with = FALSE]

train_3<-train_3[, !drops, with = FALSE]
test_3<-test_3[, !drops, with = FALSE]

#save the response var because dummyVars will remove
train_y<-traintes$future_price

train_1y<-train_1$future_price
train_2y<-train_2$future_price
train_3y<-train_3$future_price

test_1y<-test_1$future_price
test_2y<-test_2$future_price
test_3y<-test_3$future_price

traintes$future_price<-0

# work with dummies

dummies <- dummyVars(future_price ~ ., data = traintes)
traintes<-predict(dummies, newdata = traintes)
testtes<-predict(dummies, newdata = testtes)

train_1<-predict(dummies, newdata = train_1)
test_1<-predict(dummies, newdata = test_1)
train_2<-predict(dummies, newdata = train_2)
test_2<-predict(dummies, newdata = test_2)
train_3<-predict(dummies, newdata = train_3)
test_3<-predict(dummies, newdata = test_3)

traintes<-data.table(traintes)
testtes<-data.table(testtes)

train_1<-data.table(train_1)
test_1<-data.table(test_1)

train_2<-data.table(train_2)
test_2<-data.table(test_2)

train_3<-data.table(train_3)
test_3<-data.table(test_3)



########################
# Use cross validation #
########################




traintes<-as.matrix(traintes)
train_1<-as.matrix(train_1)
train_2<-as.matrix(train_2)
train_3<-as.matrix(train_3)

testtes<-as.matrix(testtes)
test_1<-as.matrix(test_1)
test_2<-as.matrix(test_2)
test_3<-as.matrix(test_3)

#note that your problem is a linear reg problem, this is a logistic regression problem
gl_model_1<-glmnet(train_1, train_1y, alpha = 1,family="gaussian")

error_DT<-NULL
for (i in 1:length(unclass(gl_model_1)$lambda)){
  model_lambda<-unclass(gl_model_1)$lambda[i]
  pred<-predict(gl_model_1,s=model_lambda, newx = test_1)
  error<-rmse(pred[,1],test_1y)
  new_row<-c(model_lambda,error)
  error_DT<-rbind(error_DT,new_row)
}

error_DT_1<-data.table(error_DT)
setnames(error_DT_1,c("V1","V2"),c("lambda","error"))

gl_model_2<-glmnet(train_2, train_2y, alpha = 1,family="gaussian")

error_DT<-NULL
for (i in 1:length(unclass(gl_model_2)$lambda)){
  model_lambda<-unclass(gl_model_2)$lambda[i]
  pred<-predict(gl_model_2,s=model_lambda, newx = test_2)
  error<-rmse(pred[,1],test_2y)
  new_row<-c(model_lambda,error)
  error_DT<-rbind(error_DT,new_row)
}


error_DT_2<-data.table(error_DT)
setnames(error_DT_2,c("V1","V2"),c("lambda","error"))

gl_model_3<-glmnet(train_3, train_3y, alpha = 1,family="gaussian")

error_DT<-NULL
for (i in 1:length(unclass(gl_model_3)$lambda)){
  model_lambda<-unclass(gl_model_3)$lambda[i]
  pred<-predict(gl_model_3,s=model_lambda, newx = test_3)
  error<-rmse(pred[,1],test_3y)
  new_row<-c(model_lambda,error)
  error_DT<-rbind(error_DT,new_row)
}

error_DT_3<-data.table(error_DT)
setnames(error_DT_3,c("V1","V2"),c("lambda","error"))

error_DT_1$current_date<-2018
error_DT_2$current_date<-2019
error_DT_3$current_date<-2020

error_DT_full<-rbind(error_DT_1,error_DT_2,error_DT_3)

ggplot(error_DT_full,aes(x=log(lambda),y=error,col=current_date))+geom_smooth()

lambda_1<-error_DT_full[error==min(error_DT_1$error)]$lambda
lambda_2<-error_DT_full[error==min(error_DT_2$error)]$lambda
lambda_3<-error_DT_full[error==min(error_DT_3$error)]$lambda
all_best_lambdas<-c(lambda_1,lambda_2,lambda_3)


bestlam<-0.01345827

####################################
# fit the model to all of the data #
####################################


#now fit the full model

#fit a logistic model
gl_model<-glmnet(traintes, train_y, alpha = 1,family="gaussian")

plot_glmnet(gl_model)

#save model
saveRDS(gl_model,"./project/volume/models/gl_modelahmed2.model")

testtes<-as.matrix(testtes)

#use the full model
pred<-predict(gl_model,s=bestlam, newx = testtes)

bestlam
predict(gl_model,s=bestlam, newx = testtes,type="coefficients")
gl_model

error_DT_full[error==min(error_DT_1$error)]
error_DT_full[error==min(error_DT_2$error)]
error_DT_full[error==min(error_DT_3$error)]

#########################
# make a submision file #
#########################


#our file needs to follow the example submission file format.
#we need the rows to be in the correct order

test_id1<-cbind(test_id,data.table(pred))
test_id1$future_price=NULL
setnames(test_id1,"1","future_price")

drops<-c("id","future_price")
submission<-test_id1
submission<-submission[, drops, with = FALSE]

write_csv(submission,"project/volume/data/processed/SubmissionPrices.csv")


