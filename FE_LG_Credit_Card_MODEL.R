library(readxl) #to read excel files
require(randomForest) 
library(caret)
library(tidyverse)
library(ggraph)
library(corrplot)
library(caret) #cross validation




FE_LG<-read.csv(file.choose())
sapply(FE_LG, function(x){sum(is.na(x))})
attach(FE_LG)
#INS <- subset(INS, select = -4)
str(FE_LG)
View(FE_LG)
#PL_campaigns <- na.omit(INS)

FE_LG$state_change <- as.factor(FE_LG$state_change)
FE_LG$state_change <- unclass(FE_LG$state_change)
FE_LG$state_change <- as.numeric(FE_LG$state_change)

FE_LG$campaign_team_708 <- as.factor(FE_LG$campaign_team_708)
FE_LG$campaign_team_708 <- unclass(FE_LG$campaign_team_708)
FE_LG$campaign_team_708<- as.numeric(FE_LG$campaign_team_708)

FE_LG$Campaign_team_505 <- as.factor(FE_LG$Campaign_team_505)
FE_LG$Campaign_team_505 <- unclass(FE_LG$Campaign_team_505)
FE_LG$Campaign_team_505<- as.numeric(FE_LG$Campaign_team_505)

FE_LG$Campaign_team_650 <- as.factor(FE_LG$Campaign_team_650)
FE_LG$Campaign_team_650 <- unclass(FE_LG$Campaign_team_650)
FE_LG$Campaign_team_650<- as.numeric(FE_LG$Campaign_team_650)

FE_LG$campaign_team_712 <- as.factor(FE_LG$campaign_team_712)
FE_LG$campaign_team_712 <- unclass(FE_LG$campaign_team_712)
FE_LG$campaign_team_712<- as.numeric(FE_LG$campaign_team_712)

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




FE_LG$Vietnam_call_time <- as.factor(FE_LG$Vietnam_call_time)
FE_LG$Vietnam_call_time <- unclass(FE_LG$Vietnam_call_time)
FE_LG$Vietnam_call_time <- as.numeric(FE_LG$Vietnam_call_time)

FE_LG$subdisposition_status <- as.factor(FE_LG$subdisposition_status)
FE_LG$subdisposition_status <- unclass(FE_LG$subdisposition_status)
FE_LG$subdisposition_status <- as.numeric(FE_LG$subdisposition_status)



FE_LG$subdisposition_name_sale <- as.factor(FE_LG$subdisposition_name_sale)
FE_LG$subdisposition_name_sale <- unclass(FE_LG$subdisposition_name_sale)
FE_LG$subdisposition_name_sale <- as.numeric(FE_LG$subdisposition_name_sale)




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

FE_LG$differ_time<- as.factor(FE_LG$differ_time)
FE_LG$differ_time <- unclass(FE_LG$differ_time)
FE_LG$differ_time <- as.numeric(FE_LG$differ_time)

FE_LG$submition_date <- as.factor(FE_LG$submition_date)
FE_LG$submition_date <- unclass(FE_LG$submition_date)
FE_LG$submition_date<- as.numeric(FE_LG$submition_date)

FE_LG$Month<- as.factor(FE_LG$Month)
FE_LG$Month <- unclass(FE_LG$Month)
FE_LG$Month<- as.numeric(FE_LG$Month)

FE_LG$Weekday<- as.factor(FE_LG$Weekday)
FE_LG$Weekday <- unclass(FE_LG$Weekday)
FE_LG$Weekday<- as.numeric(FE_LG$Weekday)

FE_LG$sale_team_name<- as.factor(FE_LG$sale_team_name)
FE_LG$sale_team_name <- unclass(FE_LG$sale_team_name)
FE_LG$sale_team_name<- as.numeric(FE_LG$sale_team_name)

View(FE_LG)
View(FE_LG$pred_gs)
#importance of variables
#cor plot
f<- cor(FE_LG)
summary(f)
corrplot(f, method = "number", type = "upper")

#random forest
FE_LG <- subset(FE_LG , select = -24)
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
set.seed(1999)
p = runif(nrow(FE_LG), 0, 1)
testing <- FE_LG [p > 0.2, ]
training <-FE_LG [p < 0.9, ]
str(training)
View(training)
Golden_Segment<- as.data.frame(FE_LG)

model <- glm(Golden_Segment ~ ., family = binomial, data = training)
summary(model)

model1 <- glm(Golden_Segment ~ campaign_team + age + state_change+
                sales_team_r  + Weekday+ monthly_income + s_user_name + name+ campaign_team_708+ birth_date+Campaign_team_650+differ_time , data = training)
summary(model1)

model2<- glm(Golden_Segment~ campaign_team_708+ age +
               sale_team+differ_time+birth_date+gender+monthly_income+state_change+Weekday  , data = training)
summary(model2)
library(MASS)
step_mode2 <- stepAIC(model2, direction = "both")
summary(step_mode2)

# Predictions
predict <- predict(model2,type="response")
residuals <- residuals(model2)
plot(predict, residuals)
hist(predict)
View(training$Golden_Segment)
View(predict)
write.csv(predict,"D:/Insurance_analysis/pred.csv",row.names = FALSE)
#roc curve
library(ROCR)
prd = prediction(predict, training$Golden_Segment)
roc = performance(prd,x.measure = "fpr",measure="tpr")
plot(roc,colorize=TRUE)

`w2w  # Cross validation cv+number = kfold
kctrl <-trainControl(method = "cv",number = 10)
fit <- train(Golden_Segment ~  campaign_team_708+ age +
               sale_team+differ_time+birth_date+gender+statttt, training,method = "glm", metric="Rsquared")
fit 

#confusion matric
pred_class <- ifelse(predict < 0.9, 0, 1)
table(actual = training$Golden_Segment, predicted = pred_class)

confusionMatrix(factor(pred_class), factor(training$Golden_Segment))


#Predicting for testing
pred <- predict(model2, newdata = testing)
test_pred_class <- ifelse(pred < 0.9, 0, 1)


table(actual = testing$Golden_Segment, predicted = test_pred_class)

confusionMatrix(factor(test_pred_class), factor(testing$Golden_Segment))

pred_gs <- predict(model2, newdata = FE_LG)

FE_LG$pred_gs <- pred_gs

write.csv(INS,"E:/Redwood/Rainbow/analysis PL/ins_prob_score.csv")

#Annalysis

age <- table(FE_LG$pred_gs, FE_LG$age)
barplot(age, beside = T, legend = rownames(FE_LG$age),
        xlab = "Age-group", ylab = "No of customers", xpd = F, fill = FE_LG$age,
        plot = T, col = c("blue", "green"))

state_change <- table(FE_LG$pred_gs, FE_LG$state_change)
barplot(state_change, beside = T, legend = rownames(FE_LG$state_change),
        xlab = "Age-group", ylab = "No of customers", xpd = F, fill = FE_LG$age,
        plot = T, col = c("blue", "green"))

campaign_team <- table(FE_LG$pred_gs, FE_LG$campaign_team)
barplot(campaign_team, beside = T, legend = rownames(FE_LG$campaign_team),
        xlab = "Age-group", ylab = "No of customers", xpd = F, fill = FE_LG$age,
        plot = T, col = c("blue", "green"))

weekday <- table(FE_LG$pred_gs, FE_LG$Weekday)
barplot(Weekday, beside = T, legend = rownames(FE_LG$Weekday),
        xlab = "day", ylab = "No of customers", xpd = F, fill = FE_LG$age,
        plot = T, col = c("blue", "green"))


Traindata$predict.score <- predict(m1, Traindata)
> pred <- prediction(Traindata$predict.score, Traindata$Subscribe) 
> perf <- performance(pred, "tpr", "fpr")
> plot(perf)

