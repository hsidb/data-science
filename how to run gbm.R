# how to run gbm
library(caret)
library(pROC)
(l <- sapply(final_data_coke_70, function(x) is.factor(x)))

final_data_coke_70_copy <- final_data_coke_70
#final_data_coke_70_copy$product_group_description <- NULL
#final_data_coke_70_copy$new_causal_name <- NULL
#final_data_coke_70_copy$new_causal_name2 <- NULL
final_data_coke_70_copy$item_selected <- NULL
final_data_coke_70_copy$shop_selected <- NULL

#dates
#date
#month
#retailer
#product
#display_level

final_data_coke_copy_Dummy <- dummyVars("~.", data = final_data_coke_70_copy, fullRank = F)
final_data_coke_copy_v10 <- as.data.frame(predict(final_data_coke_copy_Dummy, final_data_coke_70_copy))
print(names(final_data_coke_copy_v10))
outcomeName <- 'No_display_Ind'
predictorsNames <- names(final_data_coke_copy_v10[names(final_data_coke_copy_v10) != outcomeName])
final_data_coke_copy_v10$No_display_Ind2 <- ifelse(final_data_coke_copy_v10$No_display_Ind==1, 'no display', 'display')
final_data_coke_copy_v10$No_display_Ind2 <- as.factor(final_data_coke_copy_v10$No_display_Ind2)
outcomeName <- 'No_display_Ind2'
set.seed(1234)
splitIndex <- createDataPartition(final_data_coke_copy_v10[,outcomeName], p = .75, list=FALSE, times= 1)
trainDF <- final_data_coke_copy_v10[splitIndex,]
testDF <- final_data_coke_copy_v10[-splitIndex,]
objControl <- trainControl(method='cv', number=3, returnResamp='none', summaryFunction = twoClassSummary, classProbs = TRUE)
objModel <- train(trainDF[,predictorsNames], trainDF[,outcomeName],method='gbm',trControl=objControl,metric = "ROC",preProc = c("center", "scale"))
summary(objModel)
objModel 

predictions <- predict(object=objModel, testDF[,predictorsNames], type='prob')
head(predictions)

print(postResample(pred=predictions, obs=as.factor(testDF[,outcomeName])))
auc <- roc(ifelse(testDF[,outcomeName]=="display",1,0), predictions[[2]])
print(auc$auc)


set.seed(1234)
splitIndex <- createDataPartition(final_data_coke_copy_v10[,outcomeName], p = .75, list = FALSE, times = 1)
trainDF <- final_data_coke_copy_v10[ splitIndex,]
testDF  <- final_data_coke_copy_v10[-splitIndex,]