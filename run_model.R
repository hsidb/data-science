library(foreign)
library(ggplot2)
library(GGally)
library(grid)
library(arm)
library(texreg)

##########################################
#split the data in test and training data
#########################################

training_data <- final_data_coke_70[which(final_data_coke_70$week_id < 889),]
test_data <- final_data_coke_70[which(final_data_coke_70$week_id > 888),]

training_data_70 <- final_data_coke_70[which(final_data_coke_70$week_id < 885),]
test_data_70 <- final_data_coke_70[which(final_data_coke_70$week_id > 884),]

########################
#call the model formula
########################

model108 <- glm(No_display_Ind ~ actual_price + ave_price + corrected_sales_in_volume + mean_volume + mode_volume  
model107 <- glm(No_display_Ind ~  sales_index  * price_index + price_difference + corrected_sales_in_volume +   mode_volume + mean_volume  , training_data, family = binomial())
 + ave_price:mean_volume + actual_price:last_price + last_volume + ave_price:last_price  , training_data, family = binomial())


model107 <- glm(No_display_Ind ~ actual_price + ave_price + corrected_sales_in_volume + mean_volume + mode_volume  + ave_price:mean_volume   , training_data, family = binomial())

model119 <- glm(No_display_Ind ~ corrected_sales_in_volume +  sales_index + price_index  + 
                  factor(summary(final_data_coke_70)
                         
prop.table(table(final_data_coke_70$Display_Ind))
prop.table(table(final_data_coke_70$new_causal_name))
prop.table(table(final_data_coke_70$new_causal_name2))
product_group_description) + volume_difference + price_difference , training_data_copy,
                  family = binomial())

model121 <- glm(No_display_Ind ~ actual_price + corrected_sales_in_volume +  sales_index + price_index  + price_drop + price_twoweeksago +
                  factor(product_group_description) + last_price +  last_volume + price_difference + sales_value_index , training_data_copy,
                family = binomial())

model125 <- polr(display_level ~  corrected_sales_in_volume +  sales_index + price_index  + price_drop + price_twoweeksago +factor(product_group_description) + last_price  + sales_value_index + percentage + Catalogue_feature_Ind + price_fourweeksago + price_threeweeksago, training_data_copyv2, Hess = TRUE)

#####################
#run model statistics
####################

anova(model122, test= 'Chisq')
fitted.results <- predict(model122, newdata = training_data_copy, type = 'response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != training_data_copy$Display_Ind)
print(paste('Accuracy',1-misClasificError))
confusionMatrix(data= fitted.results, reference = training_data_copy$Display_Ind)

pred <- predict(model165, mergedtest6d7d9d, type = 'response')
pred_list <- as.list(pred)
pred_column <- do.call(rbind.data.frame, pred_list)
mergeddatapredictions <- cbind(mergedtest6d7d9d, pred_column)

names(mergeddatapredictions)[58]<-"predicted_display"
test_data_70_model128prediction <- cbind(test_data_70model128, fitted.results)

write.csv(test_data_copy_3, file = "model121_prediction_01062017a.csv")

pred1 <- predict(model136, test_data_copyv2)
  (tab1 <- table(pred1, test_data_copyv2$display_level))
  1-sum(diag(tab1))/sum(tab1)


anova(model125, test= 'Chisq')
fitted.results <- predict(model125, newdata = test_data_copyv2, type = 'response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test_data_copyv2$display_level)
print(paste('Accuracy',1-misClasificError))
confusionMatrix(data= fitted.results, reference = test_data_copyv2$display_level)

anova(model107, test= 'Chisq')
fitted.results <- predict(model107, newdata = training_data, type = 'response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != training_data$No_display_Ind)
print(paste('Accuracy',1-misClasificError))
confusionMatrix(data= fitted.results, reference = training_data$No_display_Ind)

pred <- predict(model121, test_data, type = 'response')

pred_list <- as.list(pred)
pred_column <- do.call(rbind.data.frame, pred_list)
myvars <- paste("v", 1:23, sep="")
training_data_copy <- training_data[c(1:20)]
test_data_copy_2 <- cbind(test_data_copy_2, pred_column)
View(training_data_copy)
names(test_data_copy_2)[37]<-"predicted_display [1=no display]"
write.csv(test_data_copy_2, file = "model121_prediction_01062017.csv")

###################
# create data plot
####################
scatter1 <- ggplot(final_data_coke_70, aes(x=actual_price,y = log(actual_price *corrected_sales_in_volume), color = new_causal_name)) + geom_point()
scatter6


##########################
# calculate mode performance
###########################

#profile(model134)
p <- predict(model134, newdata = final_data_ambient_70, type = 'response')
pr <- prediction(p, final_data_ambient_70$No_display_Ind)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
f <- performance(pr, "f")
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
library(pscl)
pR2(model107)

predict <- predict(model128, type = 'response')
#table(training_data$No_display_Ind, predict > 0.5)
ROCRpred <- prediction(predict, training_data$No_display_Ind)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
plot(model107)
ggplot(training_data, aes(x= actual_price, y = No_display_Ind))+geom_point()+stat_smooth(method= 'glm', family= "binominal", se = FALSE)
rp.perf <- performance(ROCRpred, "f")
head(rp.perf)
rp.perf


fitted.results <- as.factor(fitted.results)
precision <- posPredValue(fitted.results, training_data_70$No_display_Ind)
recall <- sensitivity(fitted.results, training_data_70$No_display_Ind)

F1 <- (2 * precision * recall) / (precision + recall)

training_data$No_display_Ind <- as.factors(training_data$No_display_Ind)
training_data$No_display_Ind <- as.factor(training_data$No_display_Ind)

confusionMatrix(data= fitted.results, reference = test_data$No_display_Ind)
summary(model107)
 
#predict(model133, test_data_copyv2,type = 'terms')

pred <- predict(model133, test_data_copyv2, type = 'response')
pred_list <- as.list(pred)
pred_column <- do.call(rbind.data.frame, pred_list)
#training_data_copy <- training_data
model133_prediction <- cbind(model133_prediction, pred_column)
#View(training_data_copy)
names(model133_prediction)[57]<-"predicted_display"

#model133_prediction$agreement <- NA
#test_data_70_model127prediction$agreement_1 <- with(test_data_70_model127prediction, ifelse(No_display_Ind == predicted_display, 1,0))
#model133_prediction <- within(test_data_70_model127prediction, aggrement1 = ifelse(No_display_Ind == predicted_display, 1,0))
#agreement <- ifelse(model133_prediction$No_display_Ind == model133_prediction$predicted.results,"yes","no")
agreement_boolean <- ifelse(model133_prediction$No_display_Ind == model133_prediction$predicted_display,1,0)

agreement_boolean <- as.list(agreement_boolean)
agreement_boolean <- do.call(rbind.data.frame, agreement_boolean)
#prediction_Ind <- do.call(rbind.data.frame, fitted.results)
#test_data_70_model127prediction <- cbind(test_data_70_model127prediction, fitted.results)
model133_prediction <- cbind(model133_prediction, agreement_boolean)
names(test_data_70_model128prediction)[54]<-"agreement_boolean"
write.csv(test_data_70_model128prediction, file = "test_data_70_model128prediction.csv")




model134 <- glm(No_display_Ind ~  corrected_sales_in_volume +  sales_index + price_index  + price_drop + percentage 
                + sales_index_base + price_index_max + price_index_mode + price_index_max_month +
                  price_twoweeksago + last_price  + sales_value_index +  
                  Catalogue_feature_Ind + price_fourweeksago + price_threeweeksago +  final_baseline_in_volume + factor(retailer) +
                  factor(quarter) + factor(month),
                training_data_copyv2,family = binomial())

library(VGAM)
model135 <- vglm(display_level ~  corrected_sales_in_volume +  sales_index + price_index  + price_drop + percentage 
                 + sales_index_base + price_index_max + price_index_mode + price_index_max_month +
                   price_twoweeksago + last_price  + sales_value_index +  
                   Catalogue_feature_Ind + price_fourweeksago + price_threeweeksago +  final_baseline_in_volume + factor(retailer) +
                   factor(quarter) + factor(month),family=cumulative(link='logit', parallel = TRUE, reverse = TRUE), data= training_data_copyv2)
                 
anova(model134, test= 'Chisq')

fitted.results <- predict(model134, newdata = final_data_pet_food_70, type = 'response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != final_data_pet_food_70$No_display_Ind)
print(paste('Accuracy',1-misClasificError))
confusionMatrix(data= fitted.results, reference = final_data_pet_food_70$No_display_Ind)

model128 <- glm(No_display_Ind ~  corrected_sales_in_volume +  sales_index + price_index  + 
                  price_drop* percentage + price_twoweeksago +factor(product_group_description) +
                  last_price  + sales_value_index  + Catalogue_feature_Ind + price_fourweeksago + 
                  price_threeweeksago + factor(month) + product, training_data_70,family = binomial())
anova(model128, test= 'Chisq')
fitted.results <- predict(model128, newdata = test_data_70, type = 'response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test_data_70$No_display_Ind)
print(paste('Accuracy',1-misClasificError))
confusionMatrix(data= fitted.results, reference = test_data_70$No_display_Ind)


model126 <- vglm(display_level ~ 1 + Catalogue_feature_Ind + factor(product_group_description) +   sales_index +
                   corrected_sales_in_volume + product +  price_index  + price_drop + percentage + price_twoweeksago +
                   last_price  + sales_value_index   + price_fourweeksago +
                   price_threeweeksago + factor(month) , 
                 family=cumulative(link='logit', parallel = FALSE, reverse = TRUE),data= training_data_copyv2)
                 #family = multinomial(),data= training_data_copyv2)
mo <- vglm(new_causal_name2 ~ 1  , 
           family=cumulative(link='logit', parallel = FALSE, reverse = TRUE),data= training_data_copyv2)
  
  
model125 <- polr(display_level2 ~  Catalogue_feature_Ind + factor(product_group_description) +   sales_index +
corrected_sales_in_volume + product +  price_index  + price_drop + percentage + price_twoweeksago +
last_price  + sales_value_index   + price_fourweeksago +
price_threeweeksago + factor(month) , training_data_copyv2, Hess = TRUE)
               


#model126 <- vglm(display_level ~ 1 + corrected_sales_in_volume + week_id + item_id + retailer + 
#product_group_description,family=cumulative(link='logit', parallel = TRUE, reverse = TRUE),data=training_data)
#fitted.results <- predict(model91, newdata=test_data, type = "response")

anova(model125, test= 'Chisq')
fitted.results <- predict(model125, newdata = test_data_copyv2, type = 'response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test_data_copyv2$display_level)
print(paste('Accuracy',1-misClasificError))
confusionMatrix(data= fitted.results, reference = test_data_copyv2$display_level)
pred1 <- predict(model125, test_data_copyv2)
(tab1 <- table(pred1, test_data_copyv2$display_level))
1-sum(diag(tab1))/sum(tab1)


model132 <- glm(No_display_Ind ~  corrected_sales_in_volume +  sales_index + price_index  + price_drop * percentage + sales_index_base +
price_twoweeksago +factor(product_group_description) + last_price  + sales_value_index +  Catalogue_feature_Ind + price_fourweeksago +
price_threeweeksago + factor(month) + product,
training_data_copyv2,family = binomial())
anova(model132, test= 'Chisq')
fitted.results <- predict(model132, newdata = test_data_copyv2, type = 'response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test_data_copyv2$No_display_Ind)
print(paste('Accuracy',1-misClasificError))
confusionMatrix(data= fitted.results, reference = test_data_copyv2$No_display_Ind)



model130 <- glm(No_display_Ind ~  corrected_sales_in_volume +  sales_index + price_index  + price_drop*percentage +
                  price_twoweeksago +factor(product_group_description) + last_price  + sales_value_index +  
                  Catalogue_feature_Ind + price_fourweeksago + price_threeweeksago + factor(month) + factor(month_1) + final_baseline_in_volume+ factor(product), training_data_copyv2,family = binomial())

anova(model136, test= 'Chisq')
fitted.results <- predict(model136, newdata = test_data_copyv2, type = 'response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test_data_copyv2$Display_Ind)
print(paste('Accuracy',1-misClasificError))
confusionMatrix(data= fitted.results, reference = test_data_copyv2$Display_Ind)

#pred <- predict(model133, test_data, type = 'response')
pred_list <- as.list(fitted.results)
pred_column <- do.call(rbind.data.frame, pred_list)
#training_data_copy_2 <- cbind(training_data_copy_2, pred_column)
model133_prediction <- cbind(test_data_copyv2, pred_column)
#write.csv(training_data_copy_2, file = "model133_prediction_15062017.csv")
write.csv(model133_prediction, file = "model133_prediction_15062017.csv")



model137 <- glm(No_display_Ind ~  corrected_sales_in_volume +  sales_index + price_index  + price_drop + percentage 
                + sales_index_base + price_index_max + price_index_mode + price_index_max_month +
                  price_twoweeksago + last_price  + sales_value_index +  
                  Catalogue_feature_Ind + price_fourweeksago + price_threeweeksago +  final_baseline_in_volume + factor(retailer) +
                  factor(quarter) + factor(month) + factor(product_group_description) + factor(product),
                training_data_pet_70,family = binomial())
anova(model137, test= 'Chisq')
fitted.results <- predict(model137, newdata = test_data_pet_70, type = 'response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test_data_pet_70$No_display_Ind)
print(paste('Accuracy',1-misClasificError))
confusionMatrix(data= fitted.results, reference = test_data_pet_70$No_display_Ind)

model138 <- glm(No_display_Ind ~  corrected_sales_in_volume +  sales_index + price_index  + price_drop + percentage 
                + sales_index_base + price_index_max + price_index_mode + price_index_max_month +
                  price_twoweeksago + last_price  + sales_value_index +  
                  Catalogue_feature_Ind + price_fourweeksago + price_threeweeksago +  final_baseline_in_volume + factor(retailer) +
                  factor(quarter) + factor(month) + factor(product_group_description) + factor(product),
                training_data_ambient_70,family = binomial())

anova(model138, test= 'Chisq')
fitted.results <- predict(model138, newdata = test_data_ambient_70, type = 'response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test_data_ambient_70$No_display_Ind)
print(paste('Accuracy',1-misClasificError))
confusionMatrix(data= fitted.results, reference = test_data_ambient_70$No_display_Ind)


model140 <- glm(No_display_Ind ~  corrected_sales_in_volume +  sales_index + price_index  + price_drop + percentage 
                + sales_index_base + price_index_max + price_index_mode + price_index_max_month +
                  price_twoweeksago + last_price  + sales_value_index +  
                  Catalogue_feature_Ind + price_fourweeksago + price_threeweeksago +  final_baseline_in_volume + factor(retailer) +
                  factor(quarter) + factor(month) + factor(product_group_description) + factor(product),
                training_data_ambient_70,family = binomial())

anova(model134, test= 'Chisq')
fitted.results <- predict(model134, newdata = final_data_ambient_70, type = 'response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != final_data_ambient_70$No_display_Ind)
print(paste('Accuracy',1-misClasificError))
confusionMatrix(data= fitted.results, reference = final_data_ambient_70$No_display_Ind)

error.analysis <- data.frame(sales = training_data$corrected_sales_in_volume, index = training_data$sales_index, fitted.results)



model141 <- glm(No_display_Ind ~  corrected_sales_in_volume +  sales_index + price_index  + price_drop + percentage 
                + price_index_mode + factor(product) +   
                  price_twoweeksago + last_price  + sales_value_index +   price_index_max + price_index_max_month +
                  Catalogue_feature_Ind +  price_threeweeksago +  final_baseline_in_volume + factor(retailer) +
                  factor(quarter) + factor(month),
                training_data_copyv7,family = binomial())
anova(model141, test= 'Chisq')
fitted.results <- predict(model141, newdata = test_data_copyv7, type = 'response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test_data_copyv7$No_display_Ind)
print(paste('Accuracy',1-misClasificError))
confusionMatrix(data= fitted.results, reference = test_data_copyv7$No_display_Ind)



model143 <- train(display_level ~  corrected_sales_in_volume +  sales_index + price_index  + price_drop + percentage 
                + price_index_mode + factor(product) +   
                  price_twoweeksago + last_price  + sales_value_index +   price_index_max + price_index_max_month +
                  Catalogue_feature_Ind +  price_threeweeksago +  final_baseline_in_volume + factor(retailer) +
                  factor(quarter) + factor(month),
                training_data_copyv7, method = "LogitBoost")


model144 <- glm(No_display_Ind ~  corrected_sales_in_volume +  sales_index + price_index  + price_drop + percentage 
                + price_index_mode + factor(product) + price_index_max + price_index_max_month + 
                price_twoweeksago + last_price  + sales_value_index +  price_index_mode +
                  Catalogue_feature_Ind +  price_threeweeksago +  final_baseline_in_volume + factor(retailer) +
                  factor(quarter) + factor(month)+ factor(product_group_description),
                training_data_copyv8,family = binomial())
anova(model144, test= 'Chisq')
fitted.results <- predict(model144, newdata = test_data_copyv8, type = 'response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test_data_copyv8$No_display_Ind)
print(paste('Accuracy',1-misClasificError))
confusionMatrix(data= fitted.results, reference = test_data_copyv8$No_display_Ind)

fitted.results <- predict(model145, newdata = test_data_copyv8, type = 'response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test_data_copyv8$No_display_Ind)
print(paste('Accuracy',1-misClasificError))
confusionMatrix(data= fitted.results, reference = test_data_copyv8$No_display_Ind)

model145 <- glm(No_display_Ind ~  corrected_sales_in_volume +  sales_index + price_index  + price_drop + percentage 
                + price_index_mode + factor(product) + price_index_max + price_index_max_month + 
                   last_price  + sales_value_index +  price_index_mode +
                  Catalogue_feature_Ind +  price_threeweeksago +   factor(retailer) +
                  + factor(month),
                training_data_copyv8,family = binomial())

model146 <- glm(No_display_Ind ~  corrected_sales_in_volume +  sales_index + price_index  + price_drop + percentage 
                + price_index_mode + factor(product) + price_index_max + price_index_max_month + 
                  last_price  + sales_value_index +  price_index_mode +
                  Catalogue_feature_Ind +  price_threeweeksago +   factor(retailer) +
                  + factor(month),
                training_data_copyv8a,family = binomial())
anova(model146, test= 'Chisq')
fitted.results <- predict(model146, newdata = test_data_copyv8a, type = 'response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test_data_copyv8a$No_display_Ind)
print(paste('Accuracy',1-misClasificError))
confusionMatrix(data= fitted.results, reference = test_data_copyv8a$No_display_Ind)

model150 <- glm(No_display_Ind ~  corrected_sales_in_volume +  sales_index + price_index  + price_drop + percentage 
                + price_index_mode + factor(product) + price_index_max + price_index_max_month + 
                  last_price  + sales_value_index +  price_index_mode +
                  Catalogue_feature_Ind +  price_threeweeksago +   factor(retailer) ,
                training_data_copyv9,family = binomial())
anova(model150, test= 'Chisq')
fitted.results <- predict(model150, newdata = test_data_copy9, type = 'response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test_data_copy9$No_display_Ind)
print(paste('Accuracy',1-misClasificError))
confusionMatrix(data= fitted.results, reference = test_data_copy9$No_display_Ind)

model151 <- glm(No_display_Ind ~  corrected_sales_in_volume +  sales_index + price_index  + price_drop + percentage 
                + price_index_mode + factor(product) + price_index_max + price_index_max_month + 
                  last_price  + sales_value_index +  price_index_mode +
                  Catalogue_feature_Ind +  price_threeweeksago +   factor(retailer) ,
                training_data_copyv9a,family = binomial())
anova(model151, test= 'Chisq')
fitted.results <- predict(model151, newdata = test_data_copyv9a, type = 'response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test_data_copyv9a$No_display_Ind)
print(paste('Accuracy',1-misClasificError))
confusionMatrix(data= fitted.results, reference = test_data_copyv9a$No_display_Ind)

model152 <- glm(No_display_Ind ~  corrected_sales_in_volume +  sales_index1 + price_index1  + price_drop +
                  percentage + price_index_mode + factor(product) + price_index_max + price_index_max_month +
                 price_twoweeksago + last_price  +  Catalogue_feature_Ind +  price_threeweeksago +  
                  final_baseline_in_volume + factor(retailer) +    factor(quarter) + factor(month),
                   training_data_copyv6b,family = binomial())

model153 <- glm(No_display_Ind ~  corrected_sales_in_volume  +  sales_index1 + price_index1  + price_drop + 
                  percentage + price_index_mode + factor(product) + price_index_max + price_index_max_month + 
                  sinsales +    price_twoweeksago  + final_baseline_in_volume + 
                  Catalogue_feature_Ind +  price_threeweeksago +   factor(retailer)  + price_threeweeksago   +
                  factor(quarter) + factor(month) ,
                training_data_copyv6b,family = binomial())
anova(model153, test= 'Chisq')
fitted.results <- predict(model153, newdata = test_data_copyv6b, type = 'response')
fitted.results <- ifelse(fitted.results > 0.7,1,0)
misClasificError <- mean(fitted.results != test_data_copyv6b$No_display_Ind)
print(paste('Accuracy',1-misClasificError))
confusionMatrix(data= fitted.results, reference = test_data_copyv6b$No_display_Ind)

model154 <- glm(No_display_Ind ~  sales_index1 + Catalogue_feature_Ind + corrected_sales_in_volume *logprice +  factor(product)
                + price_index1  + price_drop +  price_index_max + price_index_max_month + sales_index_base +
                price_twoweeksago  +  price_threeweeksago +   factor(retailer)  + price_threeweeksago   +
                  factor(quarter) + factor(month) ,
                training_data_copyv6b,family = binomial())
anova(model154, test= 'Chisq')
fitted.results <- predict(model154, newdata = test_data_copyv6b, type = 'response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test_data_copyv6b$No_display_Ind)
print(paste('Accuracy',1-misClasificError))
confusionMatrix(data= fitted.results, reference = test_data_copyv6b$No_display_Ind)

sumofsquares <- function(x) { return(sum(x^2)) }

predicted <- predict(model154, newdata= test_data_copyv6b)
plot(predicted, test_data_copyv6b$No_display_Ind)
diff <- predicted - test_data_copyv6b$No_display_Ind

sumofsquares(diff)

plot(resid(model154))

x <- test_data_copyv6b$No_display_Ind
Y <- predicted

b1 <- sum((x-mean(x))*(Y-mean(Y)))/sum((x-mean(x))^2)
b0 <- mean(Y)-b1*mean(x)




################
# Decision Tree
####################

model155 <- rpart(No_display_Ind ~  sales_index1 + Catalogue_feature_Ind + corrected_sales_in_volume  + logprice +  factor(product)
                  + price_index1  + price_drop +  price_index_max + price_index_max_month + sales_index_base +
                    price_twoweeksago  +  price_threeweeksago +   factor(retailer)  + price_threeweeksago   +
                    factor(quarter) + factor(month) , method = "anova", data = training_data_copyv6b
anova(model155, test= 'Chisq')
treePredict <- predict(model155, newdata = test_data_copyv6b)
treePredict <- ifelse(treePredict > 0.5,1,0)
confusionMatrix(data= treePredict, reference = test_data_copyv6b$No_display_Ind)

model157 <- rpart(formula = No_display_Ind ~  sales_index1 + Catalogue_feature_Ind + corrected_sales_in_volume  + logprice +  factor(product)
                  + price_index1  + price_drop +  price_index_max + price_index_max_month + sales_index_base +
                    price_twoweeksago  +  price_threeweeksago +   factor(retailer)  + price_threeweeksago   +
                    factor(quarter) + factor(month) , method = "class", data = training_data_copyv6b
      treePredict2 <- predict(model157, newdata = test_data_copyv6b)
      treePredict2 <- ifelse(treePredict2 > 0.5,1,0)
       confusionMatrix(data= treePredict2, reference = test_data_copyv6b$No_display_Ind)

#####################
#random forest
#####################
       
training_data_copyv6c <- na.omit(training_data_copyv6b)
test_data_copyv6c <- na.omit(test_data_copyv6b)

model156  <- randomForest(No_display_Ind ~  sales_index1 + Catalogue_feature_Ind + corrected_sales_in_volume  + logprice +  
                                   + price_index1  + price_drop +  price_index_max + price_index_max_month + sales_index_base +
                                     price_twoweeksago  +  price_threeweeksago +   price_threeweeksago  
                                      ,  data = training_data_copyv6c)

forestPredict <- predict(model156,newdata = test_data_copyv6c)
#forestPredict <- as.data.frame.list(forestPredict)
forestPredict <- ifelse(forestPredict > 0.5,1,0)
confusionMatrix(data= forestPredict, reference = test_data_copyv6c$No_display_Ind)

# coke.ts <- ts(data = training_data_copyv6b, frequency = 56, start= c(2014,52)
#               stl(coke.ts, s.window =  "periodic")
#   #svm
              library(e1071)
  
  model158 <- svm(No_display_Ind ~  sales_index1 + Catalogue_feature_Ind + corrected_sales_in_volume  + logprice 
                  + price_index1  + price_drop +  price_index_max + price_index_max_month + sales_index_base +
                    price_twoweeksago  +  price_threeweeksago   + price_threeweeksago , na.action =na.omit, scale = FALSE , data = training_data_copyv6b, method = "C-classification",
                  kernel = "radial", cost = 10, gamma = 0.1)
           #   kernel = "linear"
  
  
  anova(model161, test= 'Chisq')  
fitted.results <- predict(model161, newdata = test_data_copyv6, type = 'response')
fitted.results <- ifelse(fitted.results > 0.7,1,0)
   misClasificError <- mean(fitted.results != test_data_copyv6$y)
   print(paste('Accuracy',1-misClasificError))
   confusionMatrix(data= fitted.results, reference = test_data_copyv6$y)
 pred <- fitted.results 
#pred <- predict(model139, newdata = test_data_copyv6, type = 'response')
pred_list <- as.list(pred)
pred_column <- do.call(rbind.data.frame, pred_list)
training_data_copyv6b<- training_data_copyv2a
test_data_copyv6b <- cbind(test_data_copyv6b, pred_column)

model160 <- glm(No_display_Ind ~  corrected_sales_in_volume  +  sales_index1 + price_index1  + price_drop + 
                   + price_index_mode + factor(product) + price_index_max + percentage + price_index_max_month +
                  +    price_twoweeksago  + final_baseline_in_volume + predicted_display + 
                  Catalogue_feature_Ind +  price_threeweeksago +   factor(retailer)  + price_threeweeksago   +
                  factor(quarter) + factor(month) ,
                training_data_copyv6b,family = binomial())
anova(model160, test= 'Chisq')
fitted.results <- predict(model160, newdata = test_data_copyv6b, type = 'response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test_data_copyv6b$No_display_Ind)
print(paste('Accuracy',1-misClasificError))
confusionMatrix(data= fitted.results, reference = test_data_copyv6b$No_display_Ind)

model161 <- glm(y ~  corrected_sales_in_volume +  sales_index + price_index  + price_drop + percentage 
                +  price_index_max + price_index_mode + price_index_max_month +
                  price_twoweeksago + sales_value_index +  price_twoweeksago +
                  Catalogue_feature_Ind +  price_threeweeksago +  final_baseline_in_volume + factor(retailer) +
                  factor(quarter) + factor(month) +  factor(product),
                training_data_copyv6,family = binomial())

anova(model161, test= 'Chisq')
fitted.results <- predict(model161, newdata = test_data_copyv6, type = 'response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test_data_copyv6$y)
print(paste('Accuracy',1-misClasificError))
confusionMatrix(data= fitted.results, reference = test_data_copyv6$y)

model162 <- glm(y ~  corrected_sales_in_volume  +  sales_index1 + price_index1  + price_drop + 
                  percentage + price_index_mode + factor(product) + price_index_max + price_index_max_month + 
                     price_twoweeksago  + final_baseline_in_volume + 
                  Catalogue_feature_Ind +  price_threeweeksago +   factor(retailer)  + price_threeweeksago   +
                  factor(quarter) + factor(month) ,
                training_data_copyv6b,family = binomial())
anova(model162, test= 'Chisq')
fitted.results <- predict(model162, newdata = test_data_copyv6b, type = 'response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test_data_copyv6b$y)
print(paste('Accuracy',1-misClasificError))
confusionMatrix(data= fitted.results, reference = test_data_copyv6b$y)

model163 <- glm(y ~  corrected_sales_in_volume  +  sales_index1 + price_index1  + price_drop + 
                  percentage + price_index_mode + factor(product) + price_index_max + price_index_max_month + 
                  price_twoweeksago  + final_baseline_in_volume + 
                  Catalogue_feature_Ind +  price_threeweeksago +   factor(retailer)  + price_threeweeksago   +
                  factor(quarter) + factor(month) ,
                training_data_copyv7a,family = binomial())
anova(model163, test= 'Chisq')
fitted.results <- predict(model163, newdata = test_data_copyv7a, type = 'response')
fitted.results <- ifelse(fitted.results > 0.9,1,0)
misClasificError <- mean(fitted.results != test_data_copyv7a$y)
print(paste('Accuracy',1-misClasificError))
confusionMatrix(data= fitted.results, reference = test_data_copyv7a$y)

model164 <- glm(y ~  corrected_sales_in_volume  +  sales_index1 + price_index1  + price_drop + 
                  percentage + price_index_mode + factor(product) + price_index_max + price_index_max_month + 
                  price_twoweeksago  + final_baseline_in_volume + 
                  Catalogue_feature_Ind +  price_threeweeksago +   factor(retailer)  + price_threeweeksago ,
                training_data_copyv9a,family = binomial())
anova(model164, test= 'Chisq')
fitted.results <- predict(model164, newdata = test_data_copyv9a, type = 'response')
fitted.results <- ifelse(fitted.results > 0.9,1,0)
misClasificError <- mean(fitted.results != test_data_copyv9a$y)
print(paste('Accuracy',1-misClasificError))
confusionMatrix(data= fitted.results, reference = test_data_copyv9a$y)



training_data_copyv6d <- training_data_copyv6b[,-c(22,33:36,60:62,64:66)]
training_data_copyv7d <- training_data_copyv7a[,-c(56)]
training_data_copyv9d <- training_data_copyv9a[,-c(56)]
#training_data_threecatego_70 <- rbind(training_data_copyv6d,training_data_copyv7d,training_data_copyv9d) 


##############################################
#merge all data sets: coke, ambient, pet food
###############################################


merged6d7d <- merge(training_data_copyv6d,training_data_copyv7d, all.x = TRUE, all.y = TRUE)
mergedtraining6d7d9d <- merge(merged6d7d,training_data_copyv9d, all.x = TRUE, all.y = TRUE)

test_data_copyv6d <- test_data_copyv6b[,-c(22,33:36,60:62,64:66,68:69)]
test_data_copyv7d <- test_data_copyv7a[,-c(56)]
test_data_copyv9d <- test_data_copyv9a[,-c(56)]
#test_data_threecatego_70 <- rbind(test_data_copyv6d,test_data_copyv7d,test_data_copyv9d) 

merged6d7d <- merge(test_data_copyv6d,test_data_copyv7d, all.x = TRUE, all.y = TRUE)
mergedtest6d7d9d <- merge(merged6d7d,test_data_copyv9d, all.x = TRUE, all.y = TRUE)

model165 <- glm(y ~  corrected_sales_in_volume  +  sales_index1 + price_index1  + price_drop + 
                  percentage + price_index_mode + factor(product) + price_index_max + price_index_max_month + 
                  price_twoweeksago  + final_baseline_in_volume + factor(quarter)+ factor(month)+
                  Catalogue_feature_Ind +  price_threeweeksago +   factor(retailer)  + price_threeweeksago ,
                mergedtraining6d7d9d,family = binomial())
anova(model165, test= 'Chisq')
fitted.results <- predict(model165, newdata = mergedtest6d7d9d, type = 'response')
fitted.results <- ifelse(fitted.results > 0.915,1,0)
misClasificError <- mean(fitted.results != mergedtest6d7d9d$y)
print(paste('Accuracy',1-misClasificError))
confusionMatrix(data= fitted.results, reference = mergedtest6d7d9d$y)

# model166 <- glm(y ~  corrected_sales_in_volume  +  sales_index1 + price_index1  + price_drop + 
#                   percentage + price_index_mode + factor(product) + price_index_max + price_index_max_month + 
#                   price_twoweeksago  + final_baseline_in_volume + factor(quarter)+ factor(month)+
#                   Catalogue_feature_Ind +  price_threeweeksago +   factor(retailer)  + price_threeweeksago ,
#                 mergedtraining6d7d9dretailer,family = binomial())
# anova(model166, test= 'Chisq')
# fitted.results <- predict(model166, newdata = mergedtest6d7d9dretailer, type = 'response')
# fitted.results <- ifelse(fitted.results > 0.6,1,0)
# misClasificError <- mean(fitted.results != mergedtest6d7d9dretailer$y)
# print(paste('Accuracy',1-misClasificError))
# confusionMatrix(data= fitted.results, reference = mergedtest6d7d9dretailer$y)
