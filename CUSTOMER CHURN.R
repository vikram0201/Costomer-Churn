library(plyr)
library(corrplot)
library(ggplot2)
library(gridExtra)
install.packages("ggthemes")
library(ggthemes)
library(caret)
library(MASS)
library(randomForest)
install.packages("party")
library(party)

churn <- WA_Fn.UseC_.Telco.Customer.Churn
str(churn)

#missing values
sapply(churn, function(x) sum(is.na(x)))

#data wrangling
#We will change "No internet service" to "No" for six columns, they are: "OnlineSecurity", "OnlineBackup", "DeviceProtection", "TechSupport", "streamingTV", "streamingMovies".

cols_recode1 <- c(10:15)

is.atomic(cols_recode1)

for(i in 1:ncol(churn[,cols_recode1])) {
  churn[,cols_recode1][,i] <- as.factor(mapvalues
                                        (churn[,cols_recode1][,i], from =c("No internet service"),to=c("No")))
}
churn$MultipleLines <- as.factor(mapvalues(churn$MultipleLines, 
                                           from=c("No internet service"),
                                           to=c("No")))
churn$OnlineSecurity <- as.factor(mapvalues(churn$OnlineSecurity, 
                                           from=c("No internet service"),
                                           to=c("No")))
churn$OnlineBackup <- as.factor(mapvalues(churn$OnlineBackup, 
                                            from=c("No internet service"),
                                            to=c("No")))
churn$DeviceProtection <- as.factor(mapvalues(churn$DeviceProtection, 
                                          from=c("No internet service"),
                                          to=c("No")))
churn$TechSupport <- as.factor(mapvalues(churn$TechSupport, 
                                              from=c("No internet service"),
                                              to=c("No")))
churn$StreamingMovies <- as.factor(mapvalues(churn$StreamingMovies, 
                                         from=c("No internet service"),
                                         to=c("No")))
min(churn$tenure)
max(churn$tenure)
#Since the minimum tenure is 1 month and maximum tenure is 72 months, we can group them into five tenure groups: "0-12 Month", "12-24 Month", "24-48 Months", "48-60 Month", "> 60 Month"
group_tenure <- function(tenure){
  if (tenure >= 0 & tenure <= 12){
    return('0-12 Month')
  }else if(tenure > 12 & tenure <= 24){
    return('12-24 Month')
  }else if (tenure > 24 & tenure <= 48){
    return('24-48 Month')
  }else if (tenure > 48 & tenure <=60){
    return('48-60 Month')
  }else if (tenure > 60){
    return('> 60 Month')
  }
}
churn$tenure_group <- sapply(churn$tenure,group_tenure)
churn$tenure_group <- as.factor(churn$tenure_group)


#Change the values in column "SeniorCitizen" from 0 or 1 to "No" or "Yes".

churn$SeniorCitizen <- as.factor(mapvalues(churn$SeniorCitizen,
                                           from=c("0","1"),
                                           to=c("No", "Yes")))
#Remove the columns we do not need for the analysis.

churn$customerID <- NULL
churn$tenure <- NULL
#Correlation between numeric variables

numeric.var <- sapply(churn, is.numeric)
corr.matrix <- cor(churn[,numeric.var])
corrplot(corr.matrix, main="\n\nCorrelation Plot for Numerical Variables", method="number")

#The Monthly Charges and Total Charges are correlated. So one of them will be removed from the model. We remove Total Charges.

churn$TotalCharges <- NULL
#Bar plots of categorical variables
p1 <- ggplot(churn, aes(x=gender)) + ggtitle("Gender") + xlab("Gender") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") +coord_flip() + theme_minimal()
p2 <- ggplot(churn, aes(x=SeniorCitizen)) + ggtitle("Senior Citizen") + xlab("Senior Citizen") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p3 <- ggplot(churn, aes(x=Partner)) + ggtitle("Partner") + xlab("Partner") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p4 <- ggplot(churn, aes(x=Dependents)) + ggtitle("Dependents") + xlab("Dependents") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p1, p2, p3, p4, ncol=2,top="Churn distribution")
#
p5 <- ggplot(churn, aes(x=PhoneService)) + ggtitle("Phone Service") + xlab("Phone Service") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p6 <- ggplot(churn, aes(x=MultipleLines)) + ggtitle("Multiple Lines") + xlab("Multiple Lines") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p7 <- ggplot(churn, aes(x=InternetService)) + ggtitle("Internet Service") + xlab("Internet Service") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p8 <- ggplot(churn, aes(x=OnlineSecurity)) + ggtitle("Online Security") + xlab("Online Security") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p5, p6, p7, p8, ncol=2,top="Churn distribution")

p9 <- ggplot(churn, aes(x=OnlineBackup)) + ggtitle("Online Backup") + xlab("Online Backup") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p10 <- ggplot(churn, aes(x=DeviceProtection)) + ggtitle("Device Protection") + xlab("Device Protection") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p11 <- ggplot(churn, aes(x=TechSupport)) + ggtitle("Tech Support") + xlab("Tech Support") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p12 <- ggplot(churn, aes(x=StreamingTV)) + ggtitle("Streaming TV") + xlab("Streaming TV") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p9, p10, p11, p12, ncol=2,top="Churn distribution")

p13 <- ggplot(churn, aes(x=StreamingMovies)) + ggtitle("Streaming Movies") + xlab("Streaming Movies") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p14 <- ggplot(churn, aes(x=Contract)) + ggtitle("Contract") + xlab("Contract") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p15 <- ggplot(churn, aes(x=PaperlessBilling)) + ggtitle("Paperless Billing") + xlab("Paperless Billing") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p16 <- ggplot(churn, aes(x=PaymentMethod)) + ggtitle("Payment Method") + xlab("Payment Method") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p17 <- ggplot(churn, aes(x=tenure_group)) + ggtitle("Tenure Group") + xlab("Tenure Group") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p13, p14, p15, p16, p17, ncol=2,top="Churn distribution")


#Logistic Regression
#First, we split the data into training and testing sets:
  
intrain<- createDataPartition(churn$Churn,p=0.7,list=FALSE)
set.seed(2017)
training<- churn[intrain,]
testing<- churn[-intrain,]
#Confirm the splitting is correct:
  
  dim(training); dim(testing)
attach(churn)
  trainLRmodel<- glm(Churn ~ gender + SeniorCitizen + Partner + Dependents + tenure_group + 
                       PhoneService + MultipleLines + InternetService + OnlineSecurity + 
                       OnlineBackup + DeviceProtection + TechSupport + StreamingTV + 
                       StreamingMovies + Contract + PaperlessBilling + PaymentMethod + 
                       MonthlyCharges, data= training, family=binomial)
  
  trainLRmodel1<- glm(Churn~SeniorCitizen+tenure_group+PhoneService+MultipleLines+OnlineBackup+TechSupport+Contract+PaperlessBilling++PaymentMethod+MonthlyCharges,data = training,family = "binomial")
  
  
  summary(trainLRmodel1)
      
  library(ROCR)
  library(pROC)
  train_pred <- predict.glm(trainLRmodel1, newdata=training,type="response")
  ROCpred <- prediction(train_pred, training$Churn)
  ROCperf <- performance(ROCpred,'tpr','fpr')
  plot(ROCperf,main = "LOGIT ROC Curve")  
  auc <- performance(ROCpred, measure="auc")
  auc <- auc@y.values[[1]]
  auc
  area <- format(round(auc, 4), nsmall = 4)
  text(x=0.8, y=0.1, labels = paste("AUC =", area))
    
  
  #SVM
  library(e1071)
  trainSVMmodel1 <- svm(Churn~SeniorCitizen+tenure_group+PhoneService+MultipleLines+OnlineBackup+TechSupport+Contract+PaperlessBilling++PaymentMethod+MonthlyCharges,training)
  TrainSVMpred <- predict(trainSVMmodel1,training)
  ROCRpredSVM <- prediction(as.numeric(TrainSVMpred),as.numeric(training$Churn))
  ROCRperfSVM <- performance(ROCRpredSVM,'tpr','fpr')
  plot(ROCRperfSVM,main = "SVM ROC Curve")
  aucSVM <- performance(ROCRpredSVM,measure="auc")
  aucSVM <- aucSVM@y.values[[1]]
  aucSVM
  area <- format(round(aucSVM, 4), nsmall = 4)
  text(x=0.8, y=0.1, labels = paste("AUC =", area))

  #random forest
  library(randomForest)
  class(Churn)
  trainRFmodel1 <- randomForest(Churn~SeniorCitizen+tenure_group+PhoneService+MultipleLines+OnlineBackup+TechSupport+Contract+PaperlessBilling++PaymentMethod+MonthlyCharges, 
                                data = training, importance= TRUE, ntree=1000)
  summary(trainRFmodel1)  
  predictions <- as.vector(trainRFmodel1$votes[,2])
  TrainpredictRF <- predictions
  ROCRpredwholeRandom <- prediction(TrainpredictRF,training$Churn)
  ROCRperfwholeRandom <- performance(ROCRpredwholeRandom,'tpr','fpr')
  plot(ROCRperfwholeRandom,main = "Random forest ROC Curve")  
  aucwholeRandom <- performance(ROCRpredwholeRandom,measure='auc')
  aucwholeRandom <- aucwholeRandom@y.values[[1]]
  aucwholeRandom  
  area <- format(round(aucwholeRandom, 4), nsmall = 4)
  text(x=0.8, y=0.1, labels = paste("AUC =", area))

  #decision tree
  library(rpart)
  tree <- rpart(churn$Churn~., churn)
  plot(tree)
  text(tree, pretty=0)
  printcp(tree)
  plotcp(tree)
  ptree<- prune(tree,cp= tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"])
  plot(ptree, uniform=TRUE, main="Pruned Classification Tree")
  text(tree, pretty=0)
  
  your_threshold <- 0.5
  pred_tree <- predict(ptree, testing,type = "vector")>= your_threshold
  pred_class <- ifelse(pred_tree==TRUE,1,0)
  print("Confusion Matrix for Decision Tree"); table(Predicted = pred_class, Actual = testing$Churn)
  p1 <- predict(ptree, training,type = "vector")>= your_threshold
  tab1 <- table(Predicted = p1, Actual = training$Churn)
  tab2 <- table(Predicted = pred_tree, Actual = testing$Churn)
  
  print(paste('Decision Tree Accuracy on train',sum(diag(tab1))/sum(tab1)))
  
  
  print(paste('Decision Tree Accuracy on test',sum(diag(tab2))/sum(tab2)))
  
  
#Cradient boosting tree
install.packages("gbm")
    library(gbm)
trainGBMmodel1 <- gbm(Churn~SeniorCitizen+tenure_group+PhoneService+MultipleLines+OnlineBackup+TechSupport+Contract+PaperlessBilling++PaymentMethod+MonthlyCharges, data = training,
                      distribution= "gaussian",n.trees=1000,shrinkage = 0.01,interaction.depth=4)
summary(trainGBMmodel1)
# the most important factor to predict churn is "Contract" followed by "tenure". 
TrainpredGBM <- predict(trainGBMmodel1,training,n.trees=1000)
ROCRpredGBM <- prediction(TrainpredGBM, training$Churn)
ROCRperfGBM <- performance(ROCRpredGBM,'tpr','fpr')
plot(ROCRperfGBM,main = "GBM ROC Curve")
aucGBM <- performance(ROCRpredGBM,measure='auc')
aucGBM <- aucGBM@y.values[[1]]
aucGBM
area <- format(round(aucGBM, 4), nsmall = 4)
text(x=0.8, y=0.1, labels = paste("AUC =", area))
#performance comparison
m <- matrix(c(auc,aucSVM,aucwholeRandom,aucGBM),nrow=4,ncol=1)
colnames(m) <- c("AUC Value")
rownames(m) <- c("Logistic Regression","SVM","Random Forest","Gradient Boosting")
m
plot(ROCperf, col="red",colorsize = TRUE, text.adj = c(-.2,1.7), main="AUC Curves - 4 Models ")
plot(ROCRperfwholeRandom,add=TRUE,col="green", colorsize = TRUE, text.adj = c(-.2,1.7))
plot(ROCRperfSVM,add=TRUE,col="blue", colorsize = TRUE, text.adj = c(-.2,1.7))
plot(ROCRperfGBM,add=TRUE,col="black", colorsize = TRUE, text.adj = c(-.2,1.7))
labels <- c("GLM: AUC=83.83%","Random Forest: AUC=81.11%","SVM: AUC=67.16%", "Gradient Boosting: AUC=85.61%")
legend("bottom",xpd=TRUE,inset=c(0,0),labels,bty="n",pch=1,col=c("red","green","blue","black"))

#According to the AUC curves, the method that gives us the most accurate model is gradient boosting with AUC value of 85.61%.
#Performance analysis for the test set using the best model.
# Let apply GBM to the test set.

testGBMmodel1 <- gbm(Churn~SeniorCitizen+tenure_group+PhoneService+MultipleLines+OnlineBackup+TechSupport+Contract+PaperlessBilling++PaymentMethod+MonthlyCharges, data = testing,
                     distribution= "gaussian",n.trees=1000,shrinkage = 0.01,interaction.depth=4)
summary(testGBMmodel1)
TestsetpredGBM <- predict(testGBMmodel1,testing,n.trees=1000)
ROCRpredTestGBM <- prediction(TestsetpredGBM,testing$Churn)
ROCRperfTestGBM <- performance(ROCRpredTestGBM,'tpr','fpr')
plot(ROCRperfTestGBM,main = "GBM ROC Curve ON TEST SET")
aucTestGBM <- performance(ROCRpredTestGBM,measure="auc")
aucTestGBM <- aucTestGBM@y.values[[1]]
aucTestGBM
aucGBM
area <- format(round(aucTestGBM, 4), nsmall = 4)
text(x=0.8, y=0.1, labels = paste("AUC =", area))
#conclusion
a <- matrix(c("The Best Model","Average Model","Average Model","Underperforming"),nrow=4,ncol=1)
colnames(a) <- c("General Performance (accuracy) of the algorithms")
rownames(a) <- c("Gradient Boosting","Logistic Regression","Random Forest","Support Vector Machine")
a

