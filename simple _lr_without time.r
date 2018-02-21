#using the data 
dd<- read.csv(file.choose())

#preping the data for modelling
str(dd)



#options()
#validation of the data using kfold 
library(caret)
x<-dd[,-4]
y<-dd[,4]
#control <- trainControl(method = "repeatedcv" , number = 10, repeats = 5 ,search = "grid")
# Fit lm model using 10-fold CV: model
library(Matrix)
#a<-model.matrix(~.,dd)
model <- train(
  Duration ~ ., dd,
  method = "lm",
  trControl = trainControl(
    method = "cv", number = 10,
    verboseIter = TRUE
  )
)

print(model)


#spliting the data into test and train 
require(caTools)
set.seed(101) 
sample = sample.split(dd, SplitRatio = .75)
train = subset(dd, sample == TRUE)
test  = subset(dd, sample == FALSE)

#linear regression model 
model_lr <- lm(Duration~.,train )
print(model_lr)
summary(model_lr)
#predicting the values for the test data set
test$pred_duration<-predict(model_lr,test)
#finding RMSE
a<-(test$Duration-test$pred_duration)^2
a<-sqrt(mean(a))
print(a)
