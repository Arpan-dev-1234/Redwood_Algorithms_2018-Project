library(readxl) #to read excel files
require(randomForest)
library(caret)
library(tidyverse)
library(ggraph)
library(corrplot)
library(caret) #cross validation
FE-LG_campaigns<-read.csv(file.choose())
setwd("D:/Insurance PL/new.csv")

#data preparation
PL <- read.csv("PLcampaigns2.csv", sheet = "PLcampaigns2")

sapply(PL_campaigns, function(x){sum(is.na(x))})

str(PL_campaigns)
PL_campaigns <- na.omit(PL_campaigns)

#converting to factors
#INS$submition_date <- as.numeric(INS$submition_date)

FE_LG$state_change <- as.factor(PL_campaigns$state_change)
FE_LG$state_change <- unclass(PL_campaigns$state_change)
FE_LG$state_change <- as.numeric(PL_campaigns$state_change)

FE_LG$name <- as.factor(PL_campaigns$name)
FE_LG$name <- unclass(PL_campaigns$name)
FE_LG$name <- as.numeric(PL_campaigns$name)

FE_LG$differ_time <- as.numeric(as.POSIXct(PL_campaigns$differ_time))
#INS$s_ip_address <- as.numeric(INS$s_ip_address)
FE_LG$Vietnam_Call_Time <- as.numeric(as.POSIXct(PL_campaigns$Vietnam_Call_Time))

FE_LG$age <- as.factor(PL_campaigns$age)
FE_LG$age <- unclass(PL_campaigns$age)
FE_LG$age <- as.numeric(PL_campaigns$age)

PL_campaigns$callback_state <- as.factor(PL_campaigns$callback_state)
PL_campaigns$callback_state <- unclass(PL_campaigns$callback_state)
PL_campaigns$callback_state <- as.numeric(PL_campaigns$callback_state)

PL_campaigns$channel <- as.factor(PL_campaigns$channel)
PL_campaigns$channel <- unclass(PL_campaigns$channel)
PL_campaigns$channel <- as.numeric(PL_campaigns$channel)

PL_campaigns$campaign_team <- as.factor(PL_campaigns$campaign_name)
PL_campaigns$campaign_team <- unclass(PL_campaigns$campaign_name)
PL_campaigns$campaign_team <- as.numeric(PL_campaigns$campaign_name)

PL_campaigns$weekday <- as.factor(PL_campaigns$weekday)
PL_campaigns$weekday <- unclass(PL_campaigns$weekday)
PL_campaigns$weekday <- as.numeric(PL_campaigns$weekday)

PL_campaigns$ip_address <- as.numeric(PL_campaigns$ip_address)
PL_campaigns$call_duration <- as.numeric(PL_campaigns$call_duration)

str(PL_campaigns)

write.csv(PL_campaigns,"D:/Insurance PL/new.csv")

#data after adding golden segment
FE_LG<-read.csv(file.choose())
sapply(FE_LG, function(x){sum(is.na(x))})
#INS <- subset(INS, select = -4)
str(FE_LG)
#PL_campaigns <- na.omit(INS)

FE_LG$state_change <- as.factor(FE_LG$state_change)
FE_LG$state_change <- unclass(FE_LG$state_change)
FE_LG$state_change <- as.numeric(FE_LG$state_change)

FE_LG$Golden_Segment <- as.factor(FE_LG$Golden_Segment)
FE_LG$Golden_Segment <- unclass(FE_LG$Golden_Segment)
FE_LG$Golden_Segment<- as.numeric(FE_LG$Golden_Segment)

FE_LG$write_date <- as.factor(FE_LG$write_date)
FE_LG$write_date <- unclass(FE_LG$write_date)
FE_LG$write_date<- as.numeric(FE_LG$write_date)

FE_LG$age <- as.factor(FE_LG$age)
FE_LG$age <- unclass(FE_LG$age)
FE_LG$age <- as.numeric(FE_LG$age)

FE_LG$campaign_team <- as.factor(FE_LG$campaign_team)
FE_LG$campaign_team <- unclass(FE_LG$campaign_team)
FE_LG$campaign_team <- as.numeric(FE_LG$campaign_team)

FE_LG$sale_team <- as.factor(FE_LG$sale_team)
FE_LG$sale_team <- unclass(FE_LG$sale_team)
FE_LG$sale_team <- as.numeric(FE_LG$sale_team)

FE_LG$gender <- as.factor(FE_LG$gender)
FE_LG$gender <- unclass(FE_LG$gender)
FE_LG$gender <- as.numeric(FE_LG$gender)

FE_LG$city_id <- as.factor(FE_LG$city_id)
FE_LG$city_id <- unclass(FE_LG$city_id)
FE_LG$city_id <- as.numeric(FE_LG$city_id)

FE_LG$s_call_time <- as.factor(FE_LGs$s_call_time)
FE_LG$s_call_time <- unclass(FE_LG$s_call_time)
FE_LG$s_call_time <- as.numeric(FE_LG$s_call_time)


FE_LG$Vietnam_call_time <- as.factor(FE_LG$Vietnam_call_time)
FE_LG$Vietnam_call_time <- unclass(FE_LG$Vietnam_call_time)
FE_LG$Vietnam_call_time <- as.numeri(FE_LG$Vietnam_call_time)

FE_LG$subdisposition_status <- as.factor(FE_LG$subdisposition_status)
FE_LG$subdisposition_status <- unclass(FE_LG$subdisposition_status)
FE_LG$subdisposition_status <- as.numeric(FE_LG$subdisposition_status)

FE_LG$campaign_name <- as.factor(FE_LG$subdisposition_status)
FE_LG$campaign_team <- unclass(FE_LG$subdisposition_status)
FE_LG$campaign_team <- as.numeric(FE_LG$subdisposition_status)

FE_LG$subdisposition_name_sale <- as.factor(FE_LG$subdisposition_status)
FE_LG$subdisposition_name_sale <- unclass(FE_LG$subdisposition_status)
FE_LG$subdisposition_name_sale <- as.numeric(FE_LG$subdisposition_status)



FE_LG$Vietnam_call_Time <- as.factor(FE_LG$Vietnam_call_Time)
FE_LG$Vietnam_call_Time <- unclass(FE_LG$Vietnam_call_Time)
FE_LG$Vietnam_call_Time <- as.numeric(FE_LG$Vietnam_call_Time)

FE_LG$s_user_name <- as.factor(FE_LG$s_user_name)
FE_LG$s_user_name <- unclass(FE_LG$s_user_name)
FE_LG$s_user_name <- as.numeric(FE_LG$s_user_name)

FE_LG$sales_team_r <- as.factor(FE_LG$sales_team_r)
FE_LG$sales_team_r <- unclass(FE_LG$sales_team_r)
FE_LG$sales_team_r <- as.numeric(FE_LG$sales_team_r)



FE_LG$monthly_income <- as.factor(FE_LG$monthly_income)
FE_LG$monthly_income <- unclass(FE_LG$monthly_income)
FE_LG$monthly_income <- as.numeric(FE_LG$monthly_income)


FE_LG$name <- as.factor(FE_LG$name)
FE_LG$name <- unclass(FE_LG$name)
FE_LG$name <- as.numeric(FE_LG$name)

FE_LG$birth_date<- as.factor(FE_LG$birth_date)
FE_LG$birth_date <- unclass(FE_LG$birth_date)
FE_LG$birth_date <- as.numeric(FE_LG$birth_date)

#importance of variables
#cor plot
a <- cor(FE_LG)
summary(a)
corrplot(a, method = "number", type = "upper")

#random forest
FE_LG <- subset(FE_LG , select = -15)
m <-na.omit(FE_LG)
m <- subset(FE_LG, select = -Golden_Segment)
str(m)

library(ggplot2)

#relation between dependent and independent variable
quick_RF <- randomForest(x = m, y = FE_LG$Golden_Segment, ntree=50,importance=F)
imp_RF <- importance(quick_RF)
imp_DF <- data.frame(Variables = row.names(imp_RF), MSE = imp_RF[,1])
imp_DF <- imp_DF[order(imp_DF$MSE, decreasing = TRUE),]

ggplot(imp_DF[1:20,], aes(x=reorder(Variables, MSE), y=MSE, fill=MSE)) + 
  geom_bar(stat = 'identity') + 
  labs(x = 'Variables', y= '% increase MSE if variable is randomly permuted') + 
  coord_flip() + 
  theme(legend.position="none")

#separating data in test and train
set.seed(2018)
p = runif(nrow(Golden_Segment), 0, 1)
testing <- Golden_Segment [p > 0.7, ]
training <-Golden_Segment [p < 0.7, ]
str(training)

Golden_Segment<- as.data.frame(FE_LG)

model <- glm(Golden_Segment ~ ., data = training)
summary(model)

model1 <- glm(Golden_Segment ~ campaign_name + age + state_change+
                sale_team + s_call_time + , data = training)
summary(model1)

model2<- glm(Golden_Segment~ campaign_name + age +
               sale_team + s_call_time + date_No., data = training)
summary(model2)
library(MASS)
step_model <- stepAIC(model1, direction = "both")
summary(step_model)

# Predictions
predict <- predict(model1,type="response")
residuals <- residuals(model1)
plot(predict, residuals)
hist(predict)


#roc curve
library(ROCR)
prd = prediction(predict, training$Golden_Segment)
roc = performance(prd,x.measure = "fpr",measure="tpr")
plot(roc,colorize=TRUE)

# Cross validation cv+number = kfold
kctrl <-trainControl(method = "cv",number = 10)
fit <- train(golden_segment ~  age + state_change +
               sale_team + s_call_time + date_No., training,method = "glm", metric="Rsquared")
fit 

#confusion matric
pred_class <- ifelse(predict < 0.07, 0, 1)
table(actual = training$Golden_Segment, predicted = pred_class)

confusionMatrix(factor(pred_class), factor(training$Golden_Segment))


 #Predicting for testing
pred <- predict(model1, newdata = testing)
test_pred_class <- ifelse(pred < 0.07, 0, 1)


table(actual = testing$golden_segment, predicted = test_pred_class)

confusionMatrix(factor(test_pred_class), factor(testing$golden_segment))

pred_gs <- predict(model1, newdata = INS)

INS$pred_gs <- pred_gs

write.csv(INS,"E:/Redwood/Rainbow/analysis PL/ins_prob_score.csv")

#Annalysis

age <- table(INS$pred_gs, INS$age)
barplot(age, beside = T, legend = rownames(INS$age),
        xlab = "Age-group", ylab = "No of customers", xpd = F, fill = INS$age,
        plot = T, col = c("blue", "green"))

state_change <- table(INS$pred_gs, INS$state_change)
barplot(state_change, beside = T, legend = rownames(INS$state_change),
        xlab = "Age-group", ylab = "No of customers", xpd = F, fill = INS$age,
        plot = T, col = c("blue", "green"))

campaign_team <- table(INS$pred_gs, INS$campaign_team)
barplot(campaign_team, beside = T, legend = rownames(INS$campaign_team),
        xlab = "Age-group", ylab = "No of customers", xpd = F, fill = INS$age,
        plot = T, col = c("blue", "green"))

day_call <- table(INS$pred_gs, INS$day_call)
barplot(day_call, beside = T, legend = rownames(INS$day_call),
        xlab = "day", ylab = "No of customers", xpd = F, fill = INS$age,
        plot = T, col = c("blue", "green"))
