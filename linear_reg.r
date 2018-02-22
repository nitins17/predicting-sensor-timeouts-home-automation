#using the data 
dd<- read.csv(file.choose())

#dd<- subset(dd,select = -c(Location))

#dd$Start_time_mins<-scale(dd$Start_time_mins)

#options()
#validation of the data using kfold 
library(caret)
x<-dd[,-3]
y<-dd[,3]
#control <- trainControl(method = "repeatedcv" , number = 10, repeats = 5 ,search = "grid")
# Fit lm model using 10-fold CV: model
library(Matrix)
a<-model.matrix(~.,dd)
model <- train(
  Duration ~ ., a,
  method = "lm",
  trControl = trainControl(
    method = "cv", number = 10,
    verboseIter = TRUE
  )
)

print(model)

dd$Day<-as.numeric(dd$Day)
df<-dd[dd$Duration<30,]
#spliting the data into test and train 
require(caTools)
set.seed(101) 
sample = sample.split(df, SplitRatio = .75)
train = subset(df, sample == TRUE)
test  = subset(df, sample == FALSE)
#dd$Start_time_mins<-scale(dd$Start_time_mins)

#linear regression model 
model_lr <- lm(Duration~Location+Place+Start_time_mins+Day,train )
print(model_lr)
summary(model_lr)
#predicting the values for the test data set
test$pred_duration<-predict(model_lr,test)
#finding RMSE
a<-(test$Duration-test$pred_duration)^2
a<-sqrt(mean(a))
print(a)
write.csv(dd,"C:\\Users\\prasr\\Desktop\\final sem project\\UCI ADL Binary Dataset\\dd.csv",row.names = FALSE)
write.csv(df,"C:\\Users\\prasr\\Desktop\\final sem project\\UCI ADL Binary Dataset\\df.csv",row.names = FALSE)
write.csv(test,"C:\\Users\\prasr\\Desktop\\final sem project\\UCI ADL Binary Dataset\\test.csv",row.names = FALSE)
