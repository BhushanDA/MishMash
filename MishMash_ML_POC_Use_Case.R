# MishMash
#Unilever Data Science POC Use Case

#importing dataset
library(readxl)
Train_original <- read_excel("Training-Data-Sets.xlsx")
Test_original <- read_excel("Test dataset v1.xlsx")
View(Train_original)

summary(Train_original)
boxplot(Test_original$EQ)
columns<-c(colnames(Train_original))
mean(Train_original$EQ)
mean(Test_original$EQ)

#checking missing data
is.na(Train_original)
sum(is.na(Train_original))

##########################################################################
#################Converting Traing set intoperiod form####################
##########################################################################

#since Training set is daywise and Test set is periodwise, so converting Traing set by period
#1 perod= 4 weeks=28days

a_1<-sapply(Train_original[1:28,c(2:39)], mean)

for (i in seq(1,11984,28)){
  a<-sapply(Train_original[i:(i+27),c(2:39)], mean)
  a<-as.data.frame(rbind(a_1[],a))
  a_1<-a
}

Train<-a[-1,]

library(writexl)
write_xlsx(Train,"Train.xlsx")


##########################################################################
##########################Selecting Drivers###########################
##########################################################################
Train<- read_excel("Train.xlsx") #Train is Training data after time series transformation
View(Train)
summary(Train)
attach(Train)
boxplot(EQ)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#Normalized all the columns except EQ
Train1<-normalize(Train[,2:38])
Train1 <- cbind(Train1,Train$EQ)
str(Train1)
colnames(Train1)[38] <- "EQ"

#Using Boruta for Driver selection
#install.packages("Boruta")
library(Boruta)
boruta_output <- Boruta(EQ ~ ., data=Train1, doTrace=0) 
names(boruta_output)

# Get significant variables including tentatives
boruta_signif <- getSelectedAttributes(boruta_output, withTentative = TRUE)
print(boruta_signif)  
# Get significant variables without tentatives
boruta_signif_wo <- getSelectedAttributes(boruta_output, withTentative = FALSE)
print(boruta_signif_wo)

# Plot variable importance
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")  

# Using random forest for variable selection
#install.packages('randomForest')
library('randomForest')
rfModel <-randomForest(EQ ~ ., data = Train1)

# Getting the list of important variables
d<-importance(rfModel)

Train_driver<-Train[c("EQ","Social_Search_Impressions", "Median_Rainfall","Inflation",
                      "pct_PromoMarketDollars_Category","EQ_Category","EQ_Subcategory",
                      "pct_PromoMarketDollars_Subcategory")]

Test_driver<-Test_original[c("EQ","Social_Search_Impressions", "Median_Rainfall","Inflation",
                             "pct_PromoMarketDollars_Category","EQ_Category","EQ_Subcategory",
                             "pct_PromoMarketDollars_Subcategory")]

Train_driver["Period"]<-1:428
Test_driver["Period"]<-429:467
Dataset<-rbind(Train_driver,Test_driver)

#writing dataset with selected driver
library(writexl)
getwd()
write_xlsx(Train_driver,"Train_driver.xlsx")
write_xlsx(Test_driver,"Test_driver.xlsx")
write_xlsx(Dataset,"Dataset.xlsx")



#########################################################################################################
#################Forecasting using Multivariate forecasting####################
########################################################################################################
Train_driver<- read_excel("Train_driver.xlsx")
Test_driver<- read_excel("Test_driver.xlsx")
Dataset<-read_excel("Dataset.xlsx")

#install.packages("vars")
library(forecast)
library(fpp)
library(smooth)
library(readxl)
library(vars)
library(tseries)

windows()
plot(Train_driver$EQ,type="o")
plot.ts(Train_driver)
plot(ts(Dataset$EQ,frequency = 13))

# Converting data into time series object
plot(ts(Train_driver$EQ,frequency = 13))
windows()
plot(ts(Train_driver[-9],frequency = 13))

# converting time series object
data<-ts(Dataset[-9],frequency = 13)

#lag selection
info.bv <- VARselect(data, lag.max = 14, type = "both")
info.bv$selection

model<- VAR(data[1:454,],p=5, type="both", ic="AIC") 
model
k<-predict(model, n.ahead=13)
model_mape<-MAPE(k$fcst$EQ[1:13],data[455:467, 1])*100     
model_mape

#install.packages('coefplot')
library(coefplot)
coefplot(model$varresult$EQ)
names(model$varresult)

#forecast for next 6 periods
sales_forecast<-predict(model, n.ahead=19)$fcst$EQ[14:19]
sales_forecast



############################### Second Hurdle#####################
#importing dataset
library(readxl)
TrainingData<-read_excel("TrainingData.xlsx")
TestData <- read_excel("TestData.xlsx")
View(TrainingData)

#checking missing data
is.na(TrainingData)
sum(is.na(TrainingData))
is.na(TestData)
sum(is.na(TestData))
colnames(TrainingData)[colSums(is.na(TrainingData)) > 0]

# Taking care of missing data
library(mlr)
#impute missing values by mean and mode
imp <- impute(TrainingData, classes = list(numeric = imputeMean()), dummy.classes = "integer", dummy.type = "numeric")
imp1 <- impute(TestData, classes = list(numeric = imputeMean()), dummy.classes = "integer", dummy.type = "numeric")


Train_data1<-imp$data
summary(Train_data1)
Test_data1<-imp1$data
summary(Test_data1)

#Selecting Drivers
library(Boruta)
boruta_output <- Boruta(EQ ~ ., data=TrainData1, doTrace=0) 
names(boruta_output)

# Get significant variables including tentatives
boruta_signif <- getSelectedAttributes(boruta_output, withTentative = TRUE)
print(boruta_signif)  
# Get significant variables without tentatives
boruta_signif_wo <- getSelectedAttributes(boruta_output, withTentative = FALSE)
print(boruta_signif_wo)

# Plot variable importance
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")  

#Creating Data through selected drivers
TrainData1<-Train_data1[c("EQ","Social_Search_Impressions","Inflation",
                          "pct_PromoMarketDollars_Category","EQ_Category","EQ_Subcategory",
                          "pct_PromoMarketDollars_Subcategory")]

TestData1<-Test_data1[c("EQ","Social_Search_Impressions", "Inflation",
                        "pct_PromoMarketDollars_Category","EQ_Category","EQ_Subcategory",
                        "pct_PromoMarketDollars_Subcategory")]


attach(TrainData1)


library(vars)
attach(TrainData1)
EQ=diff(EQ)
EQ_Category=diff(EQ_Category)
pct_PromoMarketDollars_Subcategory=diff(pct_PromoMarketDollars_Subcategory)
Inflation=diff(Inflation)
pct_PromoMarketDollars_Category=diff(pct_PromoMarketDollars_Category)
EQ_Subcategory=diff(EQ_Subcategory)
Social_Search_Impressions=diff(Social_Search_Impressions)
Median_Rainfall=diff(Median_Rainfall)

#lag selection
info.bv1 <- VARselect(rbind(TrainData1,TestData1), lag.max = 14, type = "both")
info.bv1$selection
data1=rbind(TrainData1,TestData1)
model2=VAR(data1[1:34,],p=4,type="both",ic="AIC")
model2
k1<-predict(model2, n.ahead=5)
model_mape<-MAPE(k1$fcst$EQ[1:5],data1[35:39, 1])*100     
model_mape  #227.97

#forecast for next 6 periods
sales_forecast_2<-predict(model2, n.ahead=11)$fcst$EQ[6:11]
sales_forecast_2
