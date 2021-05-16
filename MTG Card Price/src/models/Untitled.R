install.packages("data.table")
library(data.table)
library(readr)
test_data1<- data.table(read_csv("project/volume/data/processed/clean_test_file.csv"))
train_data1 <- data.table(read_csv("project/volume/data/processed/clean_train_file.csv"))
################################################################################################
#model
train_data1$result<-as.factor(train_data1$result)
model<-glm(result~.,data=train_data1,family="binomial")
#prediction
Outcome<-predict(model,test_data1,type="response")
resultdata<-test_data1
resultdata$result=Outcome
resultdata$id=1:100000
resultdata[,c("Tail","Head")]=NULL

write_csv(data.frame(resultdata),"project/volume/data/processed/result_test_file.csv")
