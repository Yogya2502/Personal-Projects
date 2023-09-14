library(ggplot2)
library(data.table)

library(rpart)

install.packages(rpart.plot)
library(rpart.plot)
library(caTools)

#uploading data set
loan.dt <- fread("C:/Users/Yogya Hridey Sareen/Desktop/CBA/CBA Question Paper/homeloan2.csv", na.strings = c("NA", "na", "n/a", "N/A", ""))

summary(loan.dt)


#-----------------------------------Q: 1----------------------------------------------------
#changing variables with distinct values to factors
loan.dt$Gender <- factor(loan.dt$Gender)
loan.dt$Married <- factor(loan.dt$Married)
loan.dt$Dependents <- factor(loan.dt$Dependents)
loan.dt$Education <- factor(loan.dt$Education)
loan.dt$Self_Employed <- factor(loan.dt$Self_Employed)
loan.dt$Loan_Status <- factor(loan.dt$Loan_Status)
loan.dt$Credit_Score <- factor(loan.dt$Credit_Score)
loan.dt$Property_Area <- factor(loan.dt$Property_Area)

#changing coapplicants income to an integer
loan.dt$CoapplicantIncome <- strtoi(loan.dt$CoapplicantIncome)

sum(is.na(loan.dt))
which(is.na(loan.dt$Loan_Amount_Term))

summary(loan.dt$Gender)
summary(loan.dt$Married)
summary(loan.dt$Self_Employed)

ggplot(data = loan.dt, aes(x = Gender, y = Loan_Status)) + geom_jitter() + labs(title = "Distribution of loan approval status across gender")

#checking for missing values in data set
summary(loan.dt)

#deleting the missing values
loan2.dt <- na.omit(loan.dt)
summary(loan2.dt)

#deleting Loan ID as it is not required for analysis
loan2.dt$Loan_ID <- NULL


#-----------------------------------Q: 2------------------------------------------------------------

ggplot(data = loan.dt, aes(x = Loan_ID, y = ApplicantIncome)) + geom_col() + labs(title = "Distribution of Applicant Income", )

ggplot(data = loan2.dt, aes(x = Credit_Score, y = Loan_Status)) + geom_jitter() + labs(title = "Loan Approval on the Basis of Credit Score")


#------------------------------------Q: 3-----------------------------------------------------

#----------------------------USING LOGISTICAL REGRESSION--------------------------------------
#splitting the data set into train and test set
set.seed(2)
train <- sample.split(Y = loan2.dt$Loan_Status, SplitRatio = 0.7)
trainset <- subset(loan2.dt, train == T)
testset <- subset(loan2.dt, train == F)

#training the model
training <- glm(Loan_Status ~., family = binomial, data = trainset)
options(scipen = 10)
summary(training)

#predicting using the model
predict <- predict(training, newdata = testset, type = "response")
summary(predict)

#setting the threshold
threshold <- 0.5
Model_prediction <- ifelse(predict >= threshold, "Y", "N")

#checking the output
Model_prediction

confusion_matrix <- table(Data_Set = testset$Loan_Status, Model_prediction)
print(confusion_matrix)
#--------------------------------USING CART---------------------------------------------------

#make a balanced CART tree
cart.bal <- rpart(Loan_Status ~ ., data = trainset, method = 'class', control = rpart.control(minsplit = 2, cp = 0))

printcp(cart.bal)

plotcp(cart.bal)

cp.bal <- sqrt(0.0288462*0.3365385)

#pruning the tree

cart.opt <- prune(cart.bal, cp = cp.bal)

cart.predict <- predict(cart.opt, newdata = testset, type = "class")

cart.cm <- table(Testset.Actual = testset$Loan_Status, cart.predict)

cart.cm

round(prop.table(cart.cm), 3)

#checking the accuracy of the tree
mean(cart.predict == testset$Loan_Status)

cart.opt$variable.importance




