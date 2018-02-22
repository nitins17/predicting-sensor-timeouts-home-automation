#using the data 
dd<- read.csv(file.choose())

#ggplot(dd,aes(log(dd$Duration)))+geom_histogram(binwidth = 20)
#dd<- subset(dd,select = -c(Location))

#dd$Start_time_mins<-scale(dd$Start_time_mins)

#options()
#validation of the data using kfold 
df<-dd[dd$Duration<30,]

plot(dd$Duration)
library(caret)
x<-dd[,-3]
y<-dd[,3]
#control <- trainControl(method = "repeatedcv" , number = 10, repeats = 5 ,search = "grid")
# Fit lm model using 10-fold CV: model
library(Matrix)
a<-model.matrix(~.,dd)
model <- train(
  Duration ~Location+Place+Start_time_mins+Day, dd,
  method = "rf",
  trControl = trainControl(
    method = "cv", number = 10,
    verboseIter = TRUE
  )
)
print(model)
df<-dd
#df<-dd[dd$Duration<60,]
#spliting the data into test and train 
require(caTools)
set.seed(101) 
sample = sample.split(df, SplitRatio = .75)
train = subset(df, sample == TRUE)
test  = subset(df, sample == FALSE)
#dd$Start_time_mins<-scale(dd$Start_time_mins)


#svm 
library(e1071)
model_svm <- svm(Duration~Location+Place+Start_time_mins+Day,train)

print(model_rpart)
summary(model_rpart)
#predicting the values for the test data set
test$pred_duration<-predict(model_svm,test)
#finding RMSE
a<-(test$Duration-test$pred_duration)^2
a<-sqrt(mean(a))
print(a)
           