#Importing the data set 
df<- read.csv(file.choose(),na.strings = "NA",header = T)

# joining start date and start time
df$Start <- paste(df$Start,df$Time_Start)
#joining end date and time
df$End<- paste(df$End,df$Time_End)
#dropping  start time and end time
df<- subset(df, select = -c(Time_End,Time_Start))

#finding the class of the time
class(df$Start)

#converting the start and end to type time
df$Start<-strptime(df$Start , format = "%m/%d/%Y %H:%M:%S")
df$End<-strptime(df$End , format = "%m/%d/%Y %H:%M:%S")
class(df$Start)

#Finding the duration from the start time and end time
df$Duration <- abs(difftime(df$Start,df$End , units = "mins" ))
df$Duration <- round(df$Duration, digits = 3)

#finding the day when the sensor was triggered
df$Day <- format(as.Date(df$Start,format="%Y-%m-%d"), "%a")

#finding the object for which the sensor was triggered
df$obj <- paste(df$Place,df$Location , sep = " ")

dd<- subset(df,select = -c(Start,End))
write.csv(dd,"C:\\Users\\prasr\\Desktop\\final sem project\\UCI ADL Binary Dataset\\data2.csv",row.names = FALSE)
