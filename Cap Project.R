library(ggplot2)
library(caret)
library(GGally)
library(ggthemes)
library(broom)
library(dplyr)
library(bindrcpp)
library(caTools)
library(rattle)
library(RColorBrewer)
library(nnet)
dim(heartdata)
library(rpart.plot)
library(caret)

heartdata<-read.csv("C:/Users/shiva/OneDrive/Desktop/Capstone Project/heart.csv")
head(heartdata)
sum(is.na(heartdata))
colnames(heartdata)[1]<-"age"
str(heartdata)
summary(heartdata)

heartdata$sex<-as.factor(heartdata$sex)
heartdata$cp<-as.factor(heartdata$cp)
heartdata$fbs<-as.factor(heartdata$fbs)
heartdata$exang<-as.factor(heartdata$exang)
heartdata$restecg<-as.factor(heartdata$restecg)
heartdata$slope<-as.factor(heartdata$slope)
heartdata$thal<-as.factor(heartdata$thal)
heartdata$target<-as.factor(heartdata$target)
str(heartdata)
levels(heartdata$sex)[levels(heartdata$sex)==0] <- "Female"
levels(heartdata$sex)[levels(heartdata$sex)==1] <- "Male"
levels(heartdata$fbs)[levels(heartdata$fbs)==0] <- "Fasting Blood Sugar <= 120"
levels(heartdata$fbs)[levels(heartdata$fbs)==1] <- "Fasting Blood Sugar > 120"
levels(heartdata$thal)[levels(heartdata$thal)==0] <- "No Thalassemia"
levels(heartdata$thal)[levels(heartdata$thal)==1] <- "Normal Thalassemia"
levels(heratdata$thal)[levels(heratdata$thal)==2] <- "Fixed Defect Thalassemia"
levels(heartdata$thal)[levels(heartdata$thal)==3] <- "Reversible Defect Thalassemia"
levels(heartdata$target)[levels(heartdata$target)==0] <- "Healthy"
levels(heartdata$target)[levels(heartdata$target)==1] <- "Heart Disease"
levels(heartdata$exang)[levels(heartdata$exang)==1] <- "Exercise Induced Angina"
levels(heartdata$exang)[levels(heartdata$exang)==0] <- "No Exercise Induced Angina"
levels(heartdata$cp)[levels(heartdata$cp)==0] <- "Chest Pain Type 0"
levels(heartdata$cp)[levels(heartdata$cp)==1] <- "Chest Pain Type 1"
levels(heartdata$cp)[levels(heartdata$cp)==2] <- "Chest Pain Type 2"
levels(heartdata$cp)[levels(heartdata$cp)==3] <- "Chest Pain Type 3"
levels(heartdata$restecg)[levels(heartdata$restecg)==0] <- "Rest ECG 0"
levels(heartdata$restecg)[levels(heartdata$restecg)==1] <- "Rest ECG 1"
levels(heartdata$restecg)[levels(heartdata$restecg)==2] <- "Rest ECG 2"
levels(heartdata$slope)[levels(heartdata$slope)==0] <- "Peak Excercise ST Slope 0"
levels(heartdata$slope)[levels(heartdata$slope)==1] <- "Peak Excercise ST Slope 1"
levels(heartdata$slope)[levels(heartdata$slope)==2] <- "Peak Excercise ST Slope 2"
sum(is.na(heartdata))
summary(heartdata)
#Number of observations: Healthy and Heart Disease cases
myplot=ggplot(heartdata,aes(target, fill=target)) +
  geom_bar(stat="count") +
  theme_economist()  +
  scale_fill_manual(values=c("green","blue")) 
myplot + theme_gray(base_size = 14)
myplot + theme_bw()

#More Heart Disease patients seem to have between 200 and 250 mg/dl
myplot1=ggplot(heartdata,aes(chol, fill=target)) +
  geom_histogram(aes(y=..density..),breaks=seq(100, 600, by=25), color="grey") +
  geom_density(alpha=.1, fill="black")+
  facet_wrap(~target, ncol=1,scale="fixed") +
  theme_economist()  +
  scale_fill_manual(values=c("green","blue")) +
  xlab("Serum Cholestoral in mg/dl") +
  ylab("Density / Count") +
  ggtitle("Cholestoral Histogram")
myplot1 + theme_bw()


#Gender v/s target
myplot2=ggplot(heartdata,aes(target, fill=target)) +
  geom_bar(stat="count") +
  facet_wrap(~sex, ncol=2,scale="fixed") +
  theme_economist()  +
  scale_fill_manual(values=c("green","blue")) 
myplot2 + theme_bw()


#More Heart Disease patients have chest pain type 1 or 2
myplot3=ggplot(heartdata,aes(target, fill=target)) +
  geom_bar(stat="count") +
  facet_wrap(~cp, ncol=2,scale="fixed") +
  theme_economist()  +
  scale_fill_manual(values=c("green","blue")) 
myplot3 + theme_bw()

#Logistic Regression on old data
log<-glm(target~., data=heartdata, family=binomial)
summary(log)

# We can see that only a few of the paramenters significantly has an effect on Heart Disease we can exclude few
newheartdata<-heartdata[,c(2,3,9,10,12,14)]
summary(newheartdata)
#logistice regression on new data 
log1<-glm(target~., data=newheartdata, family=binomial)
summary(log1)
log.df<-tidy(log1)

data<-newheartdata
set.seed(1237)
train <- sample(nrow(data), .8*nrow(data), replace = FALSE)
TrainSet <- data[train,]
ValidSet <- data[-train,]
dim(TrainSet)
dim(ValidSet)
#Tuning parameters
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)


TrainSet$target<-make.names(TrainSet$target)
set.seed(142)
TrainSet$target<-as.factor(TrainSet$target)
# Logistic Regression with the train function in caret package
gbm<- caret::train(target ~ ., 
                          data = TrainSet ,
                          method = "glm", 
                          trControl = fitControl,
                          metric="ROC")

gbm
#Variable importance. We see that ST Depression is the most important variable followed by Chest Pain Type and No. of Vessels
varImp(gbm)
#  we predict on the Test Set.
pred <- predict(gbm,ValidSet)
levels(pred)[2] <- "Heart Disease"
t<-table(pred, ValidSet$target)
t.df<-as.data.frame(t)
res<-caret::confusionMatrix(t, positive="Heart Disease")
res
#Plotting the Confusion Matrix for Logistic Regression
logisticplot =ggplot(data = t.df, aes(x = Var2, y = pred, label=Freq)) +
  geom_tile(aes(fill = Freq)) +
  scale_fill_gradient(low="green", high="blue") +
  theme_economist() +
  xlab("Actual Heart Disease") +
  ylab("Predicted Heart Disease") +
  geom_text(size=8) +
  ggtitle("Logistic Regression")
logisticplot + theme_bw()

#Random Forest
Randomforest <- caret::train(target ~ ., 
                          data = TrainSet ,
                          method = "rf", 
                          trControl = fitControl,
                          metric="ROC")

Randomforest
#Variable importance of random forest. We see similar importance as logistic regression.
varImp(Randomforest)
#Predicting on the Test Set
pred <- predict(Randomforest,ValidSet)
levels(pred)[2] <- "Heart Disease"
t<-table(ValidSet$target, pred)
t.df<-as.data.frame(t)
res1<-caret::confusionMatrix(t, positive="Heart Disease")
res1
#confusion matrix
RFPLOT=ggplot(data = t.df, aes(x = Var1, y = pred, label=Freq)) +
  geom_tile(aes(fill = Freq)) +
  scale_fill_gradient(low="green", high="blue") +
  theme_economist() +
  xlab("Actual Heart Disease") +
  ylab("Predicted Heart Disease") +
  geom_text(size=8) +
  ggtitle("Random Forest")
RFPLOT + theme_bw()


#Decision Tree
gbmGrid <-  expand.grid(cp=c(0.01))
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)
newheartdata$target<-make.names(newheartdata$target)
system.time(decstree <- caret::train(target ~ ., 
                                      data = newheartdata,
                                      method = "rpart", 
                                      trControl = fitControl,
                                      metric="ROC",
                                      tuneGrid=gbmGrid))


decstree
#ploting decsion tree
rpart.plot(decstree$finalModel,   
           type=5,
           fallen.leaves = FALSE,
           box.palette = "GnBu",
           nn=TRUE)

#neuralnetwork
fitControl2 <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)
Neuralnetwork <- caret::train(target ~ ., 
                          data = TrainSet ,
                          method = "nnet", 
                          trControl = fitControl,
                          metric="ROC")
Neuralnetwork
#Predicting in the Test Set
pred <- predict(Neuralnetwork,ValidSet)
levels(pred)[2] <- "Heart Disease"
t<-table(ValidSet$target, pred)
t.df<-as.data.frame(t)
res3<-caret::confusionMatrix(t, positive="Heart Disease")
res3

#Confusion matrix
NeuralNetworkplot=ggplot(data = t.df, aes(x = Var1, y = pred, label=Freq)) +
  geom_tile(aes(fill = Freq)) +
  scale_fill_gradient(low="green", high="blue") +
  theme_economist() +
  xlab("Actual Heart Disease") +
  ylab("Predicted Heart Disease") +
  geom_text(size=8) +
  ggtitle("Neural Network")
NeuralNetworkplot + theme_bw()



