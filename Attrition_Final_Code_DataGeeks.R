rm(list=ls())

#install.packages('descr')
library(dplyr)
library(caTools)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(e1071)
library(class)
library(kknn)
library(corrplot)
library(magrittr)
library(knitr)
library(ggthemes)
library(dplyr)
library(forcats)
library(scales)
library("C50")
library(descr)
library(randomForest)
library(rattle)
##########################################################################
#####       Data Preprocessing
##########################################################################

### Loading CSV
attrition_raw<-read.csv("S://Spring 2020//CS 513//HW//Final//Data//attrition_data.csv", stringsAsFactors=TRUE)
View(attrition_raw)

### Data check 
str(attrition_raw)
summary(attrition_raw)

### Adding Attrition Column based on Employee Status
attrition_data <- mutate(attrition_raw, Attrition = ifelse(STATUS == "A", "No", "Yes"))
View(attrition_data)
head(attrition_data,3)
write.csv(attrition_data,"S://Spring 2020//CS 513//HW//Final//Data//Data//attrition_data_with_attrColumn.csv", row.names = FALSE)

### Explore structure
str(attrition_data) 

### Convert to Factor
attrition_data_factorized <- attrition_data %>%  mutate_if(is.character, as.factor) %>%  select(Attrition, everything())
View(attrition_data_factorized)
write.csv(attrition_data_factorized,"S://Spring 2020//CS 513//HW//Final//Data//Data//attrition_data_factorized.csv", row.names = FALSE)
glimpse(attrition_data_factorized)


### Removing insignificant varaibles
attrition_data$EMP_ID	<-NULL
attrition_data$HRLY_RATE	<-NULL
attrition_data$ETHNICITY	<-NULL
attrition_data$NUMBER_OF_TEAM_CHANGED	<-NULL
attrition_data$REFERRAL_SOURCE	<-NULL
attrition_data$HIRE_MONTH	<-NULL
attrition_data$REHIRE	<-NULL
attrition_data$TERMINATION_YEAR	<-NULL
attrition_data$STATUS	<-NULL
attrition_data$JOB_GROUP	<-NULL
attrition_data$PREVYR_1<-NULL	
attrition_data$PREVYR_2	<-NULL
attrition_data$PREVYR_3	<-NULL
attrition_data$PREVYR_4	<-NULL
attrition_data$PREVYR_5<-NULL
attrition_data$ANNUAL_RATE <-NULL
attrition_data$JOBCODE <-NULL

### Convert values to numeric

attrition_data$Attrition = factor(ifelse(attrition_data$Attrition == "Yes", 1, 0))
attrition_data$IS_FIRST_JOB = factor(ifelse(attrition_data$IS_FIRST_JOB == "Y", 1, 0))
attrition_data$DISABLED_EMP = factor(ifelse(attrition_data$DISABLED_EMP == "Y", 1, 0))
attrition_data$DISABLED_VET = factor(ifelse(attrition_data$DISABLED_VET == "Y", 1, 0))
attrition_data$EDUCATION_LEVEL = factor(attrition_data$EDUCATION_LEVEL,
                                        levels= c('LEVEL 1','LEVEL 2','LEVEL 3','LEVEL 4','LEVEL 5'),
                                        labels= c(1,2,3,4,5))
attrition_data$SEX = factor(ifelse(attrition_data$SEX == "M", 1, 0))
attrition_data$TRAVELLED_REQUIRED =factor( ifelse(attrition_data$TRAVELLED_REQUIRED == "Y", 1, 0))
attrition_data$MARITAL_STATUS= factor(attrition_data$MARITAL_STATUS,
                                      levels= c('Single','Married','Divorced'),
                                      labels= c(1,2,3))
attrition_data$AGE <- as.factor(ifelse(attrition_data$AGE<=24,"Young", ifelse(
                      attrition_data$AGE>=54,"Middle-Age","Adult"  )))
attrition_data$AGE= factor(attrition_data$AGE,
                           levels= c('Young','Middle-Age','Adult'),
                           labels= c(1,2,3))
attrition_data$PERFORMANCE_RATING= factor(attrition_data$PERFORMANCE_RATING)
attrition_data$JOB_SATISFACTION= factor(attrition_data$JOB_SATISFACTION)
attrition_data$SEX= factor(attrition_data$SEX)

##########################################################################
#####       Correlation between data
##########################################################################

# gender distribution
slices <- table(attrition_data$SEX)
lbls <- c("Male", "Female")
colr <- c("red", "blue")
pie(slices,labels=lbls, main="Pie Chart of Gender distribution", col = c("red", "blue"))

#bivariate analysis
corrplot(cor(sapply(attrition_data,as.integer)),method = "pie")

#Correlation between Marital status and Attrition
ggplot(attrition_data,aes(Attrition,MARITAL_STATUS,color=Attrition))+geom_boxplot()

#Correlation between monthly income and attrition
ggplot(attrition_data,aes(Attrition,ANNUAL_RATE,fill=Attrition))+geom_jitter()

#Correlation between Gender and attrition
ggplot() +
  geom_bar(data = attrition_data,
           aes(x = factor(attrition_data$SEX),
               fill = attrition_data$Attrition),
           position = "stack") + scale_x_discrete("Gender") + 
  scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) +
  scale_fill_manual(values=c("blue", "red"))

#Correlation between EDUCATION_LEVEL and attrition
ggplot() +
  geom_bar(data = attrition_data,
           aes(x = factor(attrition_data$EDUCATION_LEVEL),
               fill = attrition_data$Attrition),
           position = "stack") + scale_x_discrete("Education") + scale_y_continuous("Percent") + 
  guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))


#Correlation between Gender vs.Age vs. MaritalStatus vs.Attrition
ggplot(attrition_data, aes(SEX, AGE)) + 
  facet_grid(.~MARITAL_STATUS) + 
  geom_jitter(aes(color=Attrition), alpha = 0.4) +
  ggtitle("x=OverTime, y= Age, z = MaritalStatus , t = Attrition") +
  theme_light()

#Correlation between Population Pyramide
ggplot(attrition_data, aes(x= AGE, fill = SEX)) +
  geom_bar(data = subset(attrition_data, SEX == "0")) + 
  geom_bar(data = subset(attrition_data, SEX == "1"), mapping = aes(y = -..count..), position = "identity") +
  scale_y_continuous(labels = abs) + coord_flip()

##########################################################################
#####        Data Partition
##########################################################################

### Sample of 500 data is taken from original dataset 
attrition_sample<- sample(attrition_data[1:500,])

inTrain <- createDataPartition(attrition_sample$Attrition,p=0.75,list = FALSE)
train_data <- attrition_sample[inTrain,]
test_data <- attrition_sample[-inTrain,]

### original dataset 
inTrain1 <- createDataPartition(attrition_data$Attrition,p=0.75,list = FALSE)
training <- attrition_data[inTrain1,]
test <- attrition_data[-inTrain1,]

##########################################################################
#####       MOdels
##########################################################################

### Prediction Using Naive Bayes Algorithm ###
nBayes_all <- naiveBayes(Attrition~., data = training)
category_all<-predict(nBayes_all,test)
table(NBayes_all=category_all,Attrition=test$Attrition)

length(category_all)
length(test$Attrition)
#error rate
NB_wrong<-sum(category_all!=test$Attrition)
NB_error_rate<-NB_wrong/length(category_all)
#accuracy
accuracy <- 1- NB_error_rate
accuracy

### Prediction Using Dtree-CART
CART_Att<-rpart( Attrition~.,data=training)
rpart.plot(CART_Att)
CART_predict2<-predict(CART_Att,test,type="vector") 
CART_predict2

length(CART_predict2[,1])
table(Actual=test[,1],CART=CART_predict2)
CART_predict<-predict(CART_Att,test)
table(Actual=test[,1],CART=CART_predict)
str(CART_predict)
#error rate
e<- function(actual,predict) {mean(abs(actual - predict))}
errorrate<-e(test$Attrition,CART_predict2)
errorrate
#accuracy
accuracy <- 1-errorrate
accuracy

# plot 
prp(CART_Att)
fancyRpartPlot(CART_Att)

### Prediction Using C5.0 Algorithm ###

C50_class<-C5.0(formula=Attrition~.,data = training)
plot(C50_class)
category_all<-predict(C50_class,test,type="class")
table1<-table(Actual=category_all,C50=test$Attrition)

#calculating error rate
wrong<-sum(category_all!=test$Attrition )
c5_error_rate<-wrong/length(category_all)
c5_error_rate
#accuracy
confusionMatrix(table1)

### Prediction Using Random Forest Algorithm ###
fit <- randomForest( Attrition~., data=training, importance=TRUE, ntree=1000)
importance(fit)
varImpPlot(fit)
Prediction <- predict(fit, test)
table(actual=test[,1],Prediction)
#error rate
wrong<- (test[,11]!= Prediction )
errorRate<-sum(wrong)/length(wrong)
errorRate 
#accuracy
accuracy <- 1-errorRate
accuracy

### Prediction Using KKNN Algorithm ###
trctrl <- trainControl(method = 'repeatedcv', number = 10, repeats = 3)

set.seed(3033)
knn_fit <- train(Attrition ~. , data = training, method = 'knn',
                 trControl=trctrl, preProcess = c('center', 'scale'),
                 tuneLength = 10) 
knn_fit
test_pred <- predict(knn_fit, newdata =test)
table(test_pred, test$Attrition)
#accuracy
confusionMatrix(test_pred, test$Attrition)

### Prediction Using K means Algorithm ###

column<-c(2:26)
as.character(attrition_data_factorized)

is.finite(attrition_data_factorized)
kmeans<- kmeans(attrition_data_factorized[,-1],2,nstart = 10)
kmeans$cluster
table(kmeans$cluster,attrition_data_factorized[,2])

### Prediction Using SVM Algorithm ###

svmfit = svm(Attrition~ ., data = training, kernel = "linear", cost = 10, scale = FALSE)
print(svmfit)
plot(svmfit,training)

#accuracy
cm=table(unlist(test[,1]),unlist(svmfit))

### Prediction Using Logistic regression Algorithm ###
Logistic <- glm(Attrition ~., data = training, family = "binomial")
summary(Logistic) 

#confint function to obtain confidence intervals for the coefficient estimates. 
#Note that for logistic models, confidence intervals are based on the profiled log-likelihood function. 
#We can also get CIs based on just the standard errors by using the default method.
confint(Logistic) 
confint.default(Logistic)

### Prediction Using ANN Algorithm ###
index<-sort(sample(nrow( attrition_data_factorized),round(.30*nrow(attrition_data_factorized ))))
training2<- attrition_data_factorized[-index,]
test2<- attrition_data_factorized[index,]

column<-c(1,2,3,4,5,9,10,14,18,24,25,26,27,28)
net_Att  <- neuralnet(Attrition~.,data=training2[,column], hidden=5, threshold=0.01, stepmax = 1e6)
plot(net_Att)

net_results <-compute(net_Att, test2) 
class(net_results$net.result)
ANN=as.numeric(net_results$net.result)
pred<-ifelse(ANN<0.5,1,2)
View(pred)

table(Actual=test2$Attrition,pred)
length(test2$Attrition)
length(pred)

#error rate
wrong<- (round(net_results$net.result[,1])!=pred )
error_rate<-sum(wrong)/length(wrong)
error_rate
e<- function(actual,predict) {mean(abs(actual - predict))}
errorrate<-e(test2$Attrition,pred)
errorrate

#accuracy
accuracy <- 1-errorrate
accuracy

##### cross validation to check if its over fitting
train.control <- trainControl(method = "cv")
# Train the model
model <- train(Attrition ~, data = test, method = "knn",
               trControl = train.control)
# Summarize the results
print(model)
