library(ROSE)
library(caret)
library(tidyverse)
library(smotefamily)
library(mice)
library(pROC)
library(boot)
library(geometry)
library(ggplot2)
#library(glmnet)
#library(InformationValue)

# Removing all previous information
rm(list=ls())

#Reading data
df <- read_csv("stroke-data.csv", col_types = "cfdfffffddcf", na = c("N/A")) # At first "Unknown" in smoking is kept to plot for the exploratory analysis

# After factoring stroke and gender and removing a few extreme points : violin plots
df$stroke <- as.factor(df$stroke)
df$gender <- as.factor(df$gender)

# Remove only "other" gender
df <- df[if_else(df$gender=="Other",FALSE,TRUE),]
df$gender <- factor(if_else(df$gender=="Male",1,0))

# Removing bmis > 60
df_trim <- df %>% filter(bmi <= 60 | is.na(bmi))

pl<- ggplot(df, aes(x=stroke,y=age, fill=stroke))+geom_violin(trim=FALSE)+geom_boxplot(width=0.1, fill="white")+labs(x="Stroke",y="Age (in Years)")+scale_fill_brewer(palette="Blues")+theme_classic()
pl

pl1<- ggplot(df, aes(x=gender,y=bmi, fill=gender))+geom_violin(trim=FALSE)+geom_boxplot(width=0.1, fill="white")+labs(x="Gender",y="BMI (in kg/m^2)")+scale_fill_brewer(palette="Blues")+theme_classic()
pl1

pl2<- ggplot(df, aes(x=gender,avg_glucose_level, fill=gender))+geom_violin(trim=FALSE)+geom_boxplot(width=0.1, fill="white")+labs(x="Gender",y="Average Glucose Level (in mg/dl)")+scale_fill_brewer(palette="Blues")+theme_classic()
pl2

# pie chart
pie(table(df$smoking_status), main="Smoking Habits", clockwise = TRUE)

# Now again reading data, taking unknown as n/a
df <- read_csv("stroke-data.csv", col_types = "cfdfffffddcf", na = c("N/A","Unknown"))

# Data cleaning : 
# removing ID
df$id <- NULL

# Making ever-married 0 and 1, and factoring 
df$ever_married <- factor(if_else(df$ever_married == "Yes", 1, 0))

# Making Residence type 0 and 1, and factoring
df$Residence_type <- factor(if_else(df$Residence_type == "Urban", 1, 0))

# Removing only "other" gender
df <- df[if_else(df$gender=="Other",FALSE,TRUE),]
df$gender <- factor(if_else(df$gender=="Male",1,0))

# Removing bmis > 60 as extreme points
df_trim <- df %>% filter(bmi <= 60 | is.na(bmi))

# Removing unknown smoking statuses of children
df_trim$smoking_status <- ifelse((df_trim$work_type == "children" & is.na(df_trim$smoking_status)), 
                                 "never smoked", as.character(df_trim$smoking_status))
df_trim$smoking_status <- as.factor(df_trim$smoking_status)
df_trim$smoking_status <- fct_relevel(df_trim$smoking_status, c("never smoked", "formerly smoked", "smokes"))

df_trim %>% filter(work_type == "children") %>% 
  group_by(smoking_status) %>% 
  summarise(N = n(), 
            avg.age = mean(age), 
            max.age = max(age), 
            min.age = min(age))

# Imputations in bmi and unknown smoking statuses using "mice" library
imp_mice <- mice(df_trim)
df_imp <- complete(imp_mice)

# Get a summary of our cleaned data
summary(df_imp)

# Dummy variables needed for work type and smoking status
df_imp$form_smokes <- ifelse(df_imp$smoking_status=="formerly smoked",1,0)
df_imp$smokes <- ifelse(df_imp$smoking_status=="smokes",1,0) # both 0 implies never smokes

df_imp$work.private <- ifelse(df_imp$work_type=="Private",1,0)
df_imp$work.self_emp <- ifelse(df_imp$work_type=="Self-employed",1,0)
df_imp$work.govt_job <- ifelse(df_imp$work_type=="Govt_job",1,0)
df_imp$work.children <- ifelse(df_imp$work_type=="children",1,0) # similarly all 0 means never worked

# Summary with the dummy variables
summary(df_imp)

#making factors numeric for fitting models
df_imp$gender <- as.numeric(df_imp$gender)-1
df_imp$hypertension <- as.numeric(df_imp$hypertension)-1
df_imp$heart_disease <- ifelse(as.numeric(df_imp$heart_disease)==1,1,0)
df_imp$ever_married <- as.numeric(df_imp$ever_married)-1
df_imp$Residence_type <- as.numeric(df_imp$Residence_type)-1
df_imp$stroke <- ifelse(as.numeric(df_imp$stroke)==1,1,0)

str(df_imp) # Easy to know the data types of covariates

# Now separating stroke positive and stroke negative
stroke <- df_imp[df_imp$stroke==1,]
non_stroke <- df_imp[df_imp$stroke==0,]

# Sampling from both of them separately
samples <- sample(c(TRUE, FALSE), nrow(stroke), replace=TRUE, prob=c(0.6,0.4))
samplen <- sample(c(TRUE,FALSE), nrow(non_stroke), replace = TRUE, prob=c(0.6,0.4))

# Merging them to get training and testing data sets
train <- rbind(stroke[samples,],non_stroke[samplen,])
test <- rbind(stroke[!samples,],non_stroke[!samplen,])

summary(train$stroke)# To check percentage of +ve stroke data points
summary(test$stroke)

# Using SMOTE to over-sample stroke positives

train_new <- (SMOTE(train[-6][-9],train$stroke)$data)[-15] # Removing smoking status and work type as they are not needed now
summary(train_new$stroke)

#First Logistic model
model <- glm(stroke~gender+age+hypertension+heart_disease+ever_married+work.private+work.self_emp+work.govt_job+Residence_type+avg_glucose_level+bmi+form_smokes+smokes,family = "binomial"(link="logit"),data = train_new)
summary(model)

#plot(model)
#coef(model)

# Predicting from the model
predicted <- predict(model, test, type="response")
#predicted

# Using InformationValue library to get the optimal cutoff
#optimal <- optimalCutoff(test$stroke, predicted,returnDiagnostics = TRUE)

# Setting a cutoff value to get confidence matrix
pred <- ifelse(predicted>0.,1,0)
c<-confusionMatrix(as.factor(pred),as.factor(test$stroke),mode="everything",positive = '1')
c

# Have to repeat last cutoff value selection for different values for the optimal cutoff part : 
pred <- ifelse(predicted>0.9,1,0)
c<-confusionMatrix(as.factor(pred),as.factor(test$stroke),mode="everything",positive = '1')
c

pred <- ifelse(predicted>0.12,1,0)
c<-confusionMatrix(as.factor(pred),as.factor(test$stroke),mode="everything",positive = '1')
c

pred <- ifelse(predicted>0.22,1,0)
c<-confusionMatrix(as.factor(pred),as.factor(test$stroke),mode="everything",positive = '1')
c

# want to predict all the stroke cases, what should be the cutoff : getting sorted probabilities for +ve troke cases in test data
cutoff_choice.sensitivity <- sort(predicted[ifelse(test$stroke==1,TRUE,FALSE)]) # using that to get the 0.12 and 0.22 cutoff 

# Cutoff vs accuracy, specificity and sensitivity pots, ROC plot and AUC value
cutoff_choice.plots <- function(){
  cutoffs<-seq(from=0, to=1,by=0.01)
  accuracy <- c()
  sensi <- c()
  speci <- c()
  
  for (c in cutoffs){
    pr <- ifelse(predicted>c,1,0)
    conf <- confusionMatrix(as.factor(pr),as.factor(test$stroke),positive='1')$table
    accuracy <- c(accuracy,(conf[1,1]+conf[2,2])/(conf[1,1]+conf[2,1]+conf[1,2]+conf[2,2]))
    sensi <- c(sensi,sensitivity(as.factor(test$stroke),as.factor(pr)))
    speci <- c(speci,specificity(as.factor(test$stroke),as.factor(pr)))
  }
  # ROC
  roc_score = roc(test$stroke,predicted)
  print(roc_score$auc)
  par(mfrow=c(2,2))
  plot(accuracy~cutoffs,type="l",col = "red",xlab="Cut-offs",ylab = "Accuracy")
  plot(sensi~cutoffs,type="l",col = "blue",xlab="Cut-offs",ylab = "Sensitivity")
  plot(speci~cutoffs, type="l",col = "green",xlab="Cut-offs",ylab = "Specificity")
  plot(roc_score)
  
  # Best cutoff from ROC curve:
  #sensi[is.na(sensi)]<-0
  #speci[is.na(speci)]<-0
  #d <- as.vector(diag(sensi) %*% diag(speci))
  #plot(d~cutoffs,type="l",col = "red",xlab="Cut-offs",ylab = "Metric : GM(Sensitivity,Specificity)")
  #print(cutoffs[which.max(d)/101])
}
cutoff_choice.plots()

# k-fold cross validation error
# k=10
#err.model.1 <- cv.glm(train_new, model, K=10)$delta[1]



# Stepwise backward variable selection based on AIC
#model.back <- step(model)
#summary(model.back)
#model.back$deviance
#AIC(model.back)
#BIC(model.back)
#err.model.back <- cv.glm(train_new, model.back, K=10)$delta[1]

# Second Model!! Removing gender, ever-married annd bmi as they were least significants 
model1 <- glm(stroke~age+hypertension+heart_disease+work.private+work.self_emp+work.govt_job+Residence_type+avg_glucose_level+form_smokes+smokes,family = "binomial"(link="logit"),data = train_new)
summary(model1)
#plot(model)
#coef(model1)

# Predicting from the model
predicted1 <- predict(model1, test, type="response")

#Using InformationValue library to get optimal utoff
#optimal1 <- optimalCutoff(test$stroke, predicted1,optimiseFor = "Both",returnDiagnostics = TRUE)

# setting cutoff value to 0.5 and getting confdence matrix
pred1 <- ifelse(predicted1>0.5,1,0)
c1<-confusionMatrix(as.factor(pred1),as.factor(test$stroke),mode="everything",positive = '1')
c1

# Cutoff plots and sensitivity sorting as previous case
cutoff_choice1.sensitivity <- sort(predicted1[ifelse(test$stroke==1,TRUE,FALSE)])
#plot(cutoff_choice1.sensitivity,xlab="Index for positive Stroke patient",ylab="Corresponding Sensitivity, in a asc order")
min(cutoff_choice1.sensitivity) # to set a cutoff st all +ve stroke cases are predicted positive

#plots
cutoff_choice.plots <- function(){
  cutoffs<-seq(from=0.01, to=0.99,by=0.01)
  acc <- c()
  sensi <- c()
  speci <- c()
  
  for (c in cutoffs){
    pr <- ifelse(predicted1>c,1,0)
    print(pr)
    #conf <- confusionMatrix(as.factor(pr),as.factor(test$stroke),positive='1')$table
    acc <- c(acc,sum(ifelse(test$stroke==pr,1,0)/length(test$stroke)))
    sensi <- c(sensi,sensitivity(as.factor(test$stroke),as.factor(pr)))
    speci <- c(speci,specificity(as.factor(test$stroke),as.factor(pr)))
  }
  # ROC
  roc_score = roc(test$stroke,predicted1)
  print(roc_score$auc)
  par(mfrow=c(2,2))
  plot(acc~cutoffs,type="l",col = "red",xlab="Cut-offs",ylab = "Accuracy")
  plot(sensi~cutoffs,type="l",col = "blue",xlab="Cut-offs",ylab = "Sensitivity")
  plot(speci~cutoffs, type="l",col = "green",xlab="Cut-offs",ylab = "Specificity")
  plot(roc_score)
  
  # Best cutoff from ROC curve:
  #sensi[is.na(sensi)]<-0
  #speci[is.na(speci)]<-0
  #d <- as.vector(diag(sensi) %*% diag(speci))
  #plot(d~cutoffs,type="l",col = "red",xlab="Cut-offs",ylab = "Metric : GM(Sensitivity,Specificity)")
  #print(cutoffs[which.max(d)/101])
}
cutoff_choice.plots()

# 10 fold cross validation
#err.model1.1 <- cv.glm(train_new, model1, K=10)$delta[1]

# Stepwise backward variable selection
#model1.back <- step(model1)
#summary(model1.back)
#model1.back$deviance
#AIC(model1.back)
#BIC(model1.back)
# CV
#err.model1.back <- cv.glm(train_new, model1.back, K=10)$delta[1]



# Testing for Probit regression
modelp <- glm(stroke~gender+age+hypertension+heart_disease+ever_married+work.private+work.self_emp+work.govt_job+Residence_type+avg_glucose_level+bmi+form_smokes+smokes,family = "binomial"(link="probit"),data = train_new)
summary(modelp)
#plot(model)
#coef(model)

# Predicting using the model
predictedp <- predict(modelp, test, type="response")

# using InformationValue to get optimal cutoff
# optimalp <- optimalCutoff(as.factor(test$stroke), as.factor(predictedp))[1]

# setting cutoff value as 0.5 and getting confusion matrix
predp <- ifelse(predictedp>0.5,1,0)
confusionMatrix(as.factor(test$stroke), as.factor(predp))

# want to predict all the stroke cases, what should be the cutoff, and plots
cutoff_choice.sensitivityp <- sort(predictedp[ifelse(test$stroke==1,TRUE,FALSE)])
cutoff_choice.accuracyp <- function(){
  cutoffs<-seq(from=0, to=1,by=0.01)
  accuracy <- c()
  
  for (c in cutoffs){
    pr <- ifelse(predictedp>c,1,0)
    conf <- confusionMatrix(test$stroke,pr)
    accuracy <- c(accuracy,(conf[1,1]+conf[2,2])/(conf[1,1]+conf[2,1]+conf[1,2]+conf[2,2]))
  }
  plot(accuracy~cutoffs,type="l")
}
cutoff_choice.accuracyp()


# ROC
roc_scorep = roc(test$stroke,predictedp)
plot(roc_scorep)
roc_scorep$auc



