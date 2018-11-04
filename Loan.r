#########################################################################################
#LOAD AND CLEAN THE DATA
#########################################################################################
#To manipulate data
if (!require("tidyverse")) install.packages("tidyverse")
library("tidyverse")
data <- read.csv('C:/Users/Robot/Desktop/LoanData.csv',header=TRUE, sep=",",skip=2)

data <- data[,-1]

#Let's check if there is any NA
sapply(data, function(x) sum(is.na(x)))

#Lets have a look
summary(data)

#Get number of unique values in each column
unique <- sapply(data, function(x) { length(unique(x[!is.na(x)])) })

#Subset to == 1
one_val <- names(unique[unique <= 1])

#Drop columns from data
data <- data[, -which(names(data) %in% one_val)]

#Lets see the loan default rate
if (!require("janitor")) install.packages("janitor")
library("janitor")
prop = tabyl(data$Default_On_Payment)
prop

#To get the correlation between variables, the variables must be numeric
#First I make a copy
dataCopyNum <- data

#Delete de UQ CustomerID
dataCopyNum <- dataCopyNum[,-1]

#Save the numeric variables except our label
numeric_val <- unlist(lapply(dataCopyNum[,-21], is.numeric)) 
table(numeric_val)

# The conversion
dataCopyNum[sapply(dataCopyNum, is.factor)] <- lapply(dataCopyNum[sapply(dataCopyNum, is.factor)], 
                                            as.numeric)

if (!require("caret")) install.packages("caret")
library("caret")
set.seed(7)
trainId <- createDataPartition(dataCopyNum$Default_On_Payment, 
                               p=0.7, list=FALSE,times=1)
#I only save the training because I dont want to plot or analyse the test set.
db_trainNum <- dataCopyNum[trainId,]

#Lets plot the correlation matrix.
if (!require("corrplot")) install.packages("corrplot")
library("corrplot")
res <- cor(db_trainNum)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

#I make another copy to model the data
dataCopy <- data

#Delete de UQ CustomerID
dataCopy <- dataCopy[,-1]

db_train <- dataCopy[trainId,]
db_test <- dataCopy[-trainId,]

############################################################################################
#PLOTS
############################################################################################
library("ggplot2")
db_train$Default_On_Payment = ifelse(db_train$Default_On_Payment == 0,'No','Yes')
db_trainNum$Default_On_Payment = ifelse(db_trainNum$Default_On_Payment == 0,'No','Yes')
# Interleaved histograms
ggplot(db_trainNum, aes(x=Duration_in_Months, fill=Default_On_Payment)) +
  geom_histogram(binwidth=1, position="dodge") + ggtitle('Histogram of Duration in Months')

ggplot(db_trainNum, aes(x=Status_Checking_Acc, fill=Default_On_Payment)) +
  geom_histogram(binwidth=0.5, position="dodge") + ggtitle('Histogram of Status Checking Acc ')

ggplot(db_trainNum, aes(x=Credit_History, fill=Default_On_Payment)) +
  geom_histogram(binwidth=0.5, position="dodge") + ggtitle('Histogram of Credit History')

# Density plots with semi-transparent fill
ggplot(db_trainNum, aes(x=Duration_in_Months, fill=Default_On_Payment)) + geom_density(alpha=.3) + 
  ggtitle('Density of Duration in_Months')

ggplot(db_trainNum, aes(x=Status_Checking_Acc, fill=Default_On_Payment)) + geom_density(alpha=.3) +
  ggtitle('Density of Status Checking Acc')

ggplot(db_trainNum, aes(x=Credit_History, fill=Default_On_Payment)) + geom_density(alpha=.3) +
  ggtitle('Density of Credit History')

#I need to convert as factor the label.
db_train$Default_On_Payment <- as.factor(db_train$Default_On_Payment)
#################################################################################################
#MODELS
#################################################################################################

#There are some numerical columns in data with diferent scale, so we have to normalize
normalize <- function(x) {
  result <- (x - min(x, na.rm = TRUE)
  ) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  return(result)
}
norm.train <- lapply(db_train[,numeric_val],normalize)
norm.train = do.call(cbind, norm.train) %>%
  as.data.frame()

db_train <- db_train[,!numeric_val]
db_train <- cbind(db_train,norm.train)

#Decision tree
if (!require("rpart")) install.packages("rpart")
library("rpart")
if (!require("rpart.plot")) install.packages("rpart.plot")
library("rpart.plot")
cntrl = list(maxdepth = 3)

tree = rpart(Default_On_Payment ~., data = db_train, method="class", control = cntrl)
rpart.plot(tree)

#Random forest
if (!require("randomForest")) install.packages("randomForest")
library("randomForest")
ctrl = trainControl(classProbs = TRUE, summaryFunction = twoClassSummary)

#In random forest I want to use the variables that tree uses
model.rf = train(Default_On_Payment ~., data = db_train %>% select(Status_Checking_Acc,
                                                                   Credit_History,
                                                                   Duration_in_Months,
                                                                   Purposre_Credit_Taken,
                                                                   Default_On_Payment),
                 method = "rf",
                 ntree = 10,
                 tuneLength = 5,
                 metric = "ROC",
                 trControl = ctrl )

#Let's see the results
model.rf

#Generalized linear model
logReg = step(glm(Default_On_Payment ~.,data = db_train, 
                      family=binomial(link='logit')), 
                  direction="both")

summary(logReg)

###############################################################################
#Prepare the test with the same operations I did in train

norm.test <- lapply(db_test[,numeric_val],normalize)
norm.test = do.call(cbind, norm.test) %>%
  as.data.frame()

db_test <- db_test[,!numeric_val]
db_test <- cbind(db_test,norm.test)
db_test$Default_On_Payment = ifelse(db_test$Default_On_Payment == 0,'No','Yes')
db_test$Default_On_Payment <- as.factor(db_test$Default_On_Payment)
##############################################################################
#Predictions
if (!require("ROSE")) install.packages("ROSE")
library("ROSE")
pred_tree = predict(tree, db_test, type = c("prob"))[,2]
pred_rf = predict(object=model.rf, db_test %>% select(Status_Checking_Acc,
                                                     Credit_History,
                                                     Duration_in_Months,
                                                     Purposre_Credit_Taken,
                                                     Default_On_Payment), type='prob')[,2]
pred_logistic = predict(logReg, db_test, type="response")

roc_tree = roc.curve(response = db_test$Default_On_Payment, pred_tree, 
                     col = "#0d84da")
roc_rf = roc.curve(response = db_test$Default_On_Payment, pred_rf, 
                   col = "#ef0a30", add.roc=TRUE)
roc_logistic = roc.curve(response = db_test$Default_On_Payment, pred_logistic, 
                         col = "#45a163", add.roc=TRUE)

legend("bottomright", legend=c("Decision Tree", "Random Forest", 
                               "Logistic Regression"), 
       lwd = 2, col = c("#0d84da", "#ef0a30", "#45a163"))

###############################################################################
#Lets see the variable importance in our best model
plot(varImp(model.rf), main="Variable importance in our Random Forest")

