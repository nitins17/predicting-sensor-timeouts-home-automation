#using the data 
dd<- read.csv(file.choose())

#dd<- subset(dd,select = -c(Location))

#dd$Start_time_mins<-scale(dd$Start_time_mins)

#options()
#validation of the data using kfold 
dd<-dd[dd$Duration<60,]

library(caret)
x<-df[,-3]
y<-df[,3]
#control <- trainControl(method = "repeatedcv" , number = 10, repeats = 5 ,search = "grid")
# Fit lm model using 10-fold CV: model
library(Matrix)
a<-model.matrix(~.,dd)
model <- train(
  Duration ~Location+Place+Start_time_mins+Day, df,
  method = "rpart",
  trControl = trainControl(
    method = "cv", number = 10,
    verboseIter = TRUE
  )
)

  print(model)

df<-dd[dd$Duration<30,]
#spliting the data into test and train 
require(caTools)
set.seed(101) 
sample = sample.split(df, SplitRatio = .75)
train = subset(df, sample == TRUE)
test  = subset(df, sample == FALSE)
#dd$Start_time_mins<-scale(dd$Start_time_mins)

#regression tree
library(rpart)
library(rpart.plot)
model_rpart <- rpart(Duration~Location+Place+Start_time_mins+Day,train,
                     control = rpart.control(cp = 0.03979373 ))
#print(model_rpart)
#summary(model_rpart)
#predicting the values for the test data set
test$pred_duration<-predict(model_rpart,test)
#finding RMSE
a<-(test$Duration-test$pred_duration)^2
a<-sqrt(mean(a))
print(a)
